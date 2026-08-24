module Futhark (compile, compileExp) where

import Control.Monad
import Control.Monad.Error.Class
import Data.List (group, singleton, sort, transpose)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Futhark.Construct qualified as F
import Futhark.IR.SOACS qualified as F
import Futhark.Intrinsics (compileIntrinsic)
import Futhark.Monad
import Pass
import Prettyprinter hiding (group)
import Prettyprinter.Render.Text
import Prop
import Syntax hiding (ArrayType)
import Util
import VName

compileFunName :: VName -> F.Name
compileFunName = F.nameFromText . prettyText

escIfReserved :: T.Text -> T.Text
escIfReserved v = if v `elem` reserved then v <> "_" else v
  where
    reserved = ["true", "false", "if", "then", "else", "def", "let", "loop", "in", "for", "do", "with", "types"]

retAls :: Int -> Int -> F.RetAls
retAls params results = F.RetAls [0 .. params - 1] [0 .. results - 1]

compileAtom :: Atom -> FutharkM F.SubExp
compileAtom (Base (BoolVal x) _ _) =
  pure $ F.Constant $ F.BoolValue x
compileAtom (Base (IntVal x) _ _) =
  pure $ F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral x
compileAtom (Base (FloatVal x) _ _) =
  pure $ F.Constant $ F.FloatValue $ F.Float32Value x
compileAtom e = error $ unlines ["compileAtom: unhandled:", prettyString e]

compileExp' :: Exp -> FutharkM [F.SubExp]
compileExp' e@(Array ds atoms _ _) = do
  elems <- NE.toList <$> mapM compileAtom atoms
  t <- compileArrayType $ arrayTypeOf e
  singleton <$> nestArrayLits ds elems t
compileExp' (EmptyArray _ _ (Info t) _) =
  mapM (F.letSubExp "empty" <=< F.eBlank) =<< compileArrayTypes t
compileExp' (EmptyFrame _ _ (Info t) _) =
  mapM (F.letSubExp "empty" <=< F.eBlank) =<< compileArrayTypes t
compileExp' e@(Frame ds es _ _) = do
  elems <- NE.toList <$> mapM compileExp' es
  ts <- compileArrayTypes $ arrayTypeOf e
  zipWithM (nestArrayLits ds) (transpose elems) ts
compileExp' e@(Var v (Info t) _)
  | Just intrinsic <- compileIntrinsic v =
      if isFunctionType t
        then error $ unlines ["compileExp': unapplied intrinsic:", prettyString e]
        else intrinsic [] t
compileExp' (Var v _ _) =
  fromMaybe [F.Var $ compileVName v] <$> lookupRecord v
compileExp' e@(App _ _ (Info (t, pframe)) _)
  | (f@(Var f' _ _), allArgs) <- unfoldApp e,
    isScalar (typeOf f) = do
      let (paramTys, _) = unfoldArrow (arrayTypeOf f)
      unless (length paramTys == length allArgs) $
        error $
          unlines ["compileExp': partial application:", prettyString e]
      args <- zipWithM mkArg paramTys allArgs
      withMapNest (intShape pframe) t args $ applyName f'
  | otherwise =
      error $
        unlines
          [ "compileExp': unhandled lifted apply with func array:",
            prettyString e
          ]
compileExp' (Let bs e _ _) =
  mapM_ compileBind bs >> compileExp' e
compileExp' (Struct fs (Info (_, pframe)) _) =
  concat <$> mapM field (NE.toList fs)
  where
    frame = intShape pframe

    field (_, cell, e) = do
      ses <- compileExp' e
      let fieldRank = length (intShape $ shapeOf e) - length (intShape cell)
      mapM (replicateOver $ take (length frame - fieldRank) frame) ses

    replicateOver [] se = pure se
    replicateOver ds se =
      F.letSubExp "replicate" $
        F.BasicOp $
          F.Replicate (F.Shape $ map constInt64 ds) se
compileExp' e@(FieldProj x f _ _) =
  case arrayTypeOf x of
    Record fs :@ _
      | (before, (_, ft) : _) <- break ((== f) . fst) (NE.toList fs) ->
          take (valueCount ft) . drop (sum $ map (valueCount . snd) before)
            <$> compileExp' x
    _ -> error $ unlines ["compileExp': unhandled projection:", prettyString e]
compileExp' e = error $ unlines ["compileExp': unhandled:", prettyString e]

nestArrayLits :: [Int] -> [F.SubExp] -> F.Type -> FutharkM F.SubExp
nestArrayLits [] [e] _ = pure e
nestArrayLits (_ : ds) elems t = do
  let inner_t = F.stripArray 1 t
  elems' <- mapM (\es -> nestArrayLits ds es inner_t) $ chunksOf (product ds) elems
  F.letSubExp "array" $ F.BasicOp $ F.ArrayLit elems' inner_t
nestArrayLits ds elems t =
  error $
    "nestArrayLits: "
      ++ prettyString ds
      ++ " does not describe "
      ++ show (length elems)
      ++ " elements of "
      ++ F.prettyString t

mkArg :: ArrayType -> Exp -> FutharkM Arg
mkArg t x | isFunctionType t = FunArg <$> compileFunArg x
  where
    compileFunArg e
      | (f@(Var f' _ _), applied) <- unfoldApp e,
        (paramTys, retTy) <- unfoldArrow $ arrayTypeOf f,
        (appliedTys, missingTys) <- splitAt (length applied) paramTys,
        not $ null missingTys = do
          appliedArgs <- zipWithM mkArg appliedTys applied
          params <- forM missingTys $ mapM (F.newParam "x") <=< compileArrayTypes
          let missingArgs =
                zipWith
                  (\ps -> Arg . SExpArg [] (map (F.Var . F.paramName) ps))
                  params
                  missingTys
          F.mkLambda (concat params) $
            map F.subExpRes <$> applyName f' (appliedArgs <> missingArgs) retTy
    compileFunArg e =
      error $ unlines ["compileFunArg: unhandled:", prettyString e]
mkArg t_param x = do
  x' <- compileExp' x
  pure $
    Arg
      SExpArg
        { argFrame =
            let argShape = intShape $ shapeOf x
                paramShape = intShape $ arrayTypeShape t_param
             in take (length argShape - length paramShape) argShape,
          argSExps = x',
          argType = arrayTypeOf x
        }

applyName :: VName -> [Arg] -> ArrayType -> FutharkM [F.SubExp]
applyName v = fromMaybe (compileApp $ compileFunName v) (compileIntrinsic v)

compileApp :: F.Name -> [Arg] -> ArrayType -> FutharkM [F.SubExp]
compileApp f args t = do
  ts <- compileArrayTypes t
  let ses = concatMap (argSExps . sexpArg) args
  map F.Var
    <$> F.letTupExp
      "apply"
      ( F.Apply
          f
          (map (,F.Observe) ses)
          [ (F.staticShapes1 $ F.toDecl t' F.Nonunique, retAls (length ses) (length ts))
          | t' <- ts
          ]
          F.Safe
      )
  where
    sexpArg (Arg a) = a
    sexpArg (FunArg _) =
      error $ "compileApp: function argument to " ++ F.nameToString f

withMapNest ::
  [Int] ->
  ArrayType ->
  [Arg] ->
  ([Arg] -> ArrayType -> FutharkM [F.SubExp]) ->
  FutharkM [F.SubExp]
withMapNest [] t args m = m args t
withMapNest (d : ds) t args m = do
  argPairs <- mapM mapArg args
  let (inputs, params) = unzip $ concatMap fst argPairs
      args' = map snd argPairs
  lam <-
    F.mkLambda params $
      map F.subExpRes <$> withMapNest ds (peelArrayType t) args' m
  form <- F.mapSOAC lam
  map F.Var <$> F.letTupExp "map" (F.Op $ F.Screma (constInt64 d) inputs form)
  where
    mapArg :: Arg -> FutharkM ([(F.VName, F.LParam F.SOACS)], Arg)
    mapArg (Arg (SExpArg (_ : frame) ses aty)) = do
      inputs <- mapM (F.letExp "xs" . F.BasicOp . F.SubExp) ses
      params <- mapM (F.newParam "x") =<< compileArrayTypes (peelArrayType aty)
      pure
        ( zip inputs params,
          Arg $
            SExpArg frame (map (F.Var . F.paramName) params) $
              peelArrayType aty
        )
    mapArg arg = pure ([], arg)

compileParam :: Pat -> FutharkM [F.LParam F.SOACS]
compileParam (PatId v _ (Info t) _) = do
  ts <- compileArrayTypes t
  case ts of
    [t'] -> pure [F.Param mempty (compileVName v) t']
    _ -> do
      ps <- mapM (F.newParam $ F.nameFromText $ varName v) ts
      bindRecord v $ map (F.Var . F.paramName) ps
      pure ps

compileBind :: Bind -> FutharkM ()
compileBind BindType {} = pure ()
compileBind BindISpace {} = pure ()
compileBind (BindFun f params _ body (Info ret) _) = do
  (params', rets) <-
    assertNoStms $
      (,)
        <$> (concat <$> mapM compileParam (NE.toList params))
        <*> compileArrayTypes (findRet ret)
  body' <-
    F.localScope (F.scopeOfLParams params') $
      mkBody $
        compileExp' body
  addFunction
    F.FunDef
      { F.funDefEntryPoint = Nothing,
        F.funDefAttrs = mempty,
        F.funDefName = compileFunName f,
        F.funDefRetType =
          [ ( F.toDecl (F.staticShapes1 ret') F.Nonunique,
              retAls (length params') (length rets)
            )
          | ret' <- rets
          ],
        F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params',
        F.funDefBody = body'
      }
compileBind (BindVal v _ e _) = do
  ses <- compileExp' e
  case ses of
    [se] -> F.letBindNames [compileVName v] $ F.BasicOp $ F.SubExp se
    _ -> bindRecord v ses
compileBind b = error $ "compileBind: unhandled " ++ prettyString b

valueType :: F.Type -> F.ValueType
valueType (F.Prim pt) =
  F.ValueType F.Signed mempty pt
valueType (F.Array pt shape _) =
  F.ValueType F.Signed (F.Rank (F.shapeRank shape)) pt
valueType t = error $ "valueType: unhandled " ++ F.prettyString t

entryPointType :: ArrayType -> FutharkM F.EntryPointType
entryPointType t
  | valueCount t == 1 =
      F.TypeTransparent . valueType <$> compileArrayType t
entryPointType (Record fs :@ frame) = do
  row <- addOpaqueType . F.OpaqueRecord =<< fields mempty
  F.TypeOpaque <$> case length $ intShape frame of
    0 -> pure row
    rank -> addOpaqueType . F.OpaqueRecordArray rank row =<< fields frame
  where
    fields fr =
      forM (NE.toList fs) $ \(f, ft :@ fshape) ->
        (F.nameFromText f,) <$> entryPointType (ft :@ (fr <> fshape))
entryPointType t =
  error $ unlines ["entryPointType: unhandled:", prettyString t]

addEntry :: F.Name -> [Pat] -> Exp -> FutharkM ()
addEntry name params body = do
  (params', entryParams) <-
    assertNoStms $
      (,)
        <$> (concat <$> mapM compileParam params)
        <*> mapM entryParam params
  (res, stms) <-
    F.collectStms $
      F.localScope (F.scopeOfLParams params') $
        compileExp' body
  (rets, entryResult) <-
    assertNoStms $
      (,)
        <$> compileArrayTypes (arrayTypeOf body)
        <*> (F.EntryResult F.Nonunique <$> entryPointType (arrayTypeOf body))
  addFunction
    F.FunDef
      { F.funDefEntryPoint = Just (name, entryParams, entryResult, Nothing),
        F.funDefAttrs = mempty,
        F.funDefName = "entry_" <> name,
        F.funDefRetType =
          [ ( F.toDecl (F.staticShapes1 ret) F.Nonunique,
              retAls (length params') (length rets)
            )
          | ret <- rets
          ],
        F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params',
        F.funDefBody = F.Body () stms $ map (F.SubExpRes mempty) res
      }
  where
    entryParam p =
      F.EntryParam (F.nameFromText $ escIfReserved $ varName $ patVar p) F.Nonunique
        <$> entryPointType (arrayTypeOf p)

compileDecl :: Decl -> FutharkM ()
compileDecl (Def b) = compileBind b
compileDecl (Entry f params _ body _ _) =
  addEntry (F.nameFromText $ varName f) params body

runCompile :: FutharkM () -> PassM T.Text
runCompile action = do
  tag <- getVarTag
  case runFutharkM tag action of
    Left err -> throwError err
    Right ((), consts, funs, types, tag') -> do
      putVarTag tag'
      pure $ renderProg tag' $ F.Prog types consts funs
  where
    renderProg tag prog =
      renderStrict . layoutPretty defaultLayoutOptions $
        vsep ["name_source" <+> braces (pretty $ getTag tag), pretty prog]

-- | Turn a Remora exp into Futhark.
compileExp :: Exp -> PassM T.Text
compileExp = runCompile . addEntry "main" []

-- | Turn a Remora program into Futhark.
compile :: Prog -> PassM T.Text
compile (Prog decls)
  | null entries = throwError "compile: program has no entry point"
  | Just name <- duplicate =
      throwError $ "compile: duplicate entry point: " <> name
  | otherwise = runCompile $ mapM_ compileDecl decls
  where
    entries = [varName v | Entry v _ _ _ _ _ <- decls]
    duplicate = listToMaybe $ concatMap (drop 1) $ group $ sort entries
