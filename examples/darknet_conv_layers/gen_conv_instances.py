#!/usr/bin/env python3
# usage: thisScript yolov4_layers.txt
# will print remora code to stdout
import argparse
import re
import sys
    
def main() -> int:
    parser = argparse.ArgumentParser(
        prog='gen_conv_instances',
        usage="%(prog)s inputFilePath",
        description='generates to stdout remora code that contains concrete calls to polymorphic conv2d')
    parser.add_argument('inFilePath', help="path to input file describing layers",
                        action='store', nargs='?',
                        default='yolov4_layers_full.txt')
    args = parser.parse_args()

    # copy polymorphic conv_2d_rem to stdout
    with open("../darknet/conv_remainder.remora", 'r') as f:
        print(";; start of conv_remainder.remora")
        for line in f:
            print(line, end='')
        print(";; end of conv_remainder.remora")

    with open(args.inFilePath, 'r') as f:
        _ignore = f.readline() # ignore 1st line
        for line in f:
            fields = re.split(r'[\sx/]+', line)
            #    layer   filters  size/strd(dil)      input                output
            #    0 conv     32       3 x 3/ 1    608 x 608 x   3 ->  608 x 608 x  32 0.639 BF
            #  0 1  2       3        4   5  6     7    8       9 10  11     12    13   14 
            # print("0:", fields[0], "1:", fields[1], "2:", fields[2], "3:", fields[3], "4:", fields[4],
            #       "5:", fields[5], "6:", fields[6], "7:", fields[7], "8:", fields[8], "9:", fields[9],
            #       "10:", fields[10], "11:", fields[11], "12:", fields[12], "13:", fields[13],
            #       "14:", fields[14])
            layer = fields[1]
            filters = fields[3]
            win_dim = fields[4]
            stride = fields[6]
            input_dim = fields[7]
            input_depth = fields[9]
            output_dim = fields[11]
            output_depth = fields[13]
            pad = int(win_dim) // 2
            output_combined = int(output_dim) * int(output_dim)
            n_minus_k = int(input_dim) - int(win_dim)
            prediv = int(input_dim) + 2 * pad - int(win_dim)
            q = prediv // int(stride)
            r = prediv %  int(stride)
            # defn_str = f'''(def (fun conv2d/layer_{layer}
            #     (in [Float {input_depth} {input_dim} {input_dim}])
            #     (w [Float {filters} {win_dim} {win_dim}])
            #     : [Float {output_depth} {output_combined}]
            #   (@conv2d (Float) ({n_minus_k} {win_dim} {input_depth} {output_depth} 1) in w)))
            # '''
            defn_str = f'''(entry (conv_2d_rem_layer_{layer}
                (in [Float {input_depth} {input_dim} {input_dim}])
                (w [Float {output_depth} {input_depth} {win_dim} {win_dim}])
                : [Float {output_depth} {output_combined}])
              (@conv_2d_rem _ ({q} {win_dim} {input_depth} {output_depth} {pad} {stride} {r}) in w))
            '''
            print(defn_str)
    return 0

if __name__ == '__main__':
    sys.exit(main())
