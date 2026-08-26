#lang racket

(define name-map
  (hash "w" "weights"
        "var" "variance"))

(define (encode-dim i)
  (integer->integer-bytes i 8 #f #f))

(define (futhark-header/f32 shape)
  (bytes-append (bytes 98 2 (length shape)) (string->bytes/utf-8 " f32") (apply bytes-append (map encode-dim shape))))


;; Bytes (Listof Nat) -> Listof Bytes
(define (split-bin-into-layers bin sizes)
  (match sizes
    ['() '()]
    [(cons h t)
     (define b (subbytes bin 0 h))
     (define r (subbytes bin h))
     (cons b (split-bin-into-layers r t))]))

(define (bin->futhark-bin b shape)
  (bytes-append (futhark-header/f32 shape) b))


(module+ main
  
  (define filename (make-parameter "../yolov4.remora"))
  (define weights-file (make-parameter "../yolov4.weights"))
  (define exec-file (make-parameter "../../../yolov4"))
  (define input (make-parameter "../input.bin"))
  (define output (make-parameter "yolo_out_fut.bin"))
  (define file (file->list (filename)))

  (define entry-main (last file))

  (define args (rest (rest (second entry-main))))
  ;; List (name, size in bytes
  (define args-clean (map (lambda (p)
                            (define name (symbol->string (first p)))
                            (match-define (list t_p id) (string-split name "-"))
                            (define t (if (hash-has-key? name-map t_p) (hash-ref name-map t_p) t_p))
                            (list t id (rest (second p))))
                          args))
  (define img (first (rest (second entry-main))))
  (define img-shape (rest (second img)))
  (command-line
   #:program "Organize input to be able to run YOLOv4 using a Futhark executable"
   #:once-each
   [("-r" "--remora") r "Path to the yolov4 source code" (filename r)]
   [("-w" "--weights") w "Path to the yolov4.weights file" (weights-file w)]
   [("-e" "--exec") e "Path to yolov4 executable" (exec-file e)]
   [("-i" "--input") i "Path to the input image" (input i)]
   [("-o" "--output") o "Path to the output" (output o)]
   )
  (define exec (format "~a -b > ~a" (exec-file) (output)))

  (define weights-bin (subbytes (file->bytes (weights-file)) 20))
  (println "read weights")
  (define layer-sizes (map (lambda (x) (* 4 (apply * (third x)))) args-clean))
  (define layers-weights (split-bin-into-layers weights-bin layer-sizes))
  (define weights-data (apply bytes-append (map bin->futhark-bin layers-weights (map third args-clean))))
  (define img-data (bin->futhark-bin (file->bytes (input)) img-shape))
  (define data (bytes-append img-data weights-data))
  (println (bytes-length data))

  (println "formatted binary data")
  (time
    (match-define (list in-p out-p _ in-err h)
      (process exec))
    (println "started yolo process")
    (match (h 'status)
      ['done-error (println (port->string in-err))]
      ['done-ok (println "success")]
      ['running (println "running")])

    (write-bytes data out-p)
    (close-output-port out-p)
    (println "wrote binary data to process")
    (h 'status)
    (h 'wait)
    (match (h 'status)
      ['done-error (println (port->string in-err))]
      ['done-ok (println "success")])
    (println "writing output")
    (copy-port in-p (current-output-port))))
