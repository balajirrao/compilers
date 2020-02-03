module TypeCheck where

import AST
import Control.Monad (foldM, forM_, replicateM, unless)
import Data.Char
import Data.Functor.Foldable
import Data.List (unfoldr)
import qualified Data.Map as M
import Numeric
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Type
import Unify
import Prelude hiding (lookup)

import Errors

-- Map from symbols to types
newtype TypeEnv = TypeEnv (M.Map String Type)

lookup :: Members [Error CompilerError, Reader TypeEnv] r => String -> Sem r Type
lookup x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
    Nothing -> throw (TypeCheckError $ "lookup error: " ++ x)
    (Just t) -> return t

newtype NextTypeId = NextTypeId Int deriving (Show)

freshTypeVar :: Member (State NextTypeId) r => Sem r Type
freshTypeVar = do
  (NextTypeId x) <- get
  put (NextTypeId $ x + 1)
  return $ TyVar . TypeVar $ (showIntAtBase 26 (chr . (+ 97)) x) ""

typecheckSem ::
  Members
    [ Reader TypeEnv,
      Error CompilerError,
      State NextTypeId,
      State Bindings
    ]
    r =>
  AST (Sem r Type) ->
  Sem r Type
typecheckSem (Number _) = return $ TyBase "Int"
typecheckSem (LowerVar x) = lookup x
typecheckSem (UpperVar x) = lookup x
typecheckSem (Case caseExpr branches) = do
  caseType <- caseExpr
  branchType <- freshTypeVar
  forM_ branches $ \(Branch pattern e) -> do
    branchEnv <- match pattern caseType
    curBranchType <- local (const branchEnv) e
    unify branchType curBranchType
  return branchType
typecheckSem (BinOp op x y) = do
  xType <- x
  yType <- y
  opType <- lookup op
  returnType <- freshTypeVar
  unify opType (TyFn xType (TyFn yType returnType))
  return returnType
typecheckSem (App f x) = do
  fType <- f
  xType <- x
  returnType <- freshTypeVar
  unify fType (TyFn xType returnType)
  return returnType

match ::
  Members
    [ Reader TypeEnv,
      Error CompilerError,
      State Bindings
    ]
    r =>
  Pattern ->
  Type ->
  Sem r TypeEnv
match (PatternVar x) t = do
  (TypeEnv env) <- ask
  return $ TypeEnv (M.insert x t env)
match (PatternCons constr params) t = do
  initConstrType <- lookup constr
  initEnv <- ask
  (constrType, env) <- foldM processParam (initConstrType, initEnv) params
  unify t constrType
  case constrType of
    TyBase _ -> return env
    _ -> throw $ TypeCheckError "match error: bad constructor type"
  where
    processParam ::
      Member (Error CompilerError) r =>
      (Type, TypeEnv) ->
      String ->
      Sem r (Type, TypeEnv)
    processParam (TyFn t1 t2, TypeEnv env) param =
      return
        ( t2,
          TypeEnv $ M.insert param t1 env
        )
    processParam _ _ = throw $ TypeCheckError "processParam error: not a function type"

typecheck ::
  Members
    [ Error CompilerError,
      Reader TypeEnv,
      State NextTypeId,
      State Bindings
    ]
    r =>
  Fix AST ->
  Sem r Type
typecheck = fold typecheckSem

typecheckDefinitionPass1 ::
  Member (State NextTypeId) r =>
  Definition ->
  Sem r TypeEnv
typecheckDefinitionPass1 (Defn name params _) = do
  retType <- freshTypeVar
  freshTypeVars <- replicateM (length params) freshTypeVar
  let fullType = foldr TyFn retType freshTypeVars
  return $ TypeEnv (M.singleton name fullType)
typecheckDefinitionPass1 (Data name constructors) =
  return $
    foldl goCons (TypeEnv M.empty) constructors
  where
    goCons :: TypeEnv -> Constructor -> TypeEnv
    goCons (TypeEnv env) (Constructor cons params) =
      TypeEnv $
        M.insert cons (foldl goParam (TyBase name) params) env
    goParam :: Type -> String -> Type
    goParam t p = TyFn (TyBase p) t

unfoldTyFn :: Type -> [Type]
unfoldTyFn (TyFn t1 t2) = t1 : unfoldTyFn t2
unfoldTyFn t = [t]

typecheckDefinitionPass2 ::
  Members
    [ Error CompilerError,
      Reader TypeEnv,
      State NextTypeId,
      State Bindings
    ]
    r =>
  Definition ->
  Sem r ()
typecheckDefinitionPass2 (Defn name params ast) = do
  (TypeEnv env) <- ask
  defnType <- lookup name
  let unfolded = unfoldTyFn defnType
      paramTypes = init unfolded
      retType = last unfolded
  unless (length paramTypes == length params) $ throw $ TypeCheckError ("paramtypes length mismatch"  ++ show defnType)
  let paramsTypes = zip params paramTypes
      envWithParams = TypeEnv $ foldr (\(p, t) -> M.insert p t) env paramsTypes
  body_type <- local (const envWithParams) $ typecheck ast
  unify retType body_type
typecheckDefinitionPass2 (Data _ _) = return ()

union :: TypeEnv -> TypeEnv -> TypeEnv
union (TypeEnv env1) (TypeEnv env2) = TypeEnv $ M.union env1 env2

typecheckProgram ::
  Members
    [ Error CompilerError,
      State NextTypeId,
      State Bindings
    ]
    r =>
  [Definition] ->
  Sem r ()
typecheckProgram definitions = do
  let intType = TyBase "Int"
      binOpType = TyFn intType (TyFn intType intType)
      binOpEnv =
        TypeEnv $
          M.fromList
            [ ("+", binOpType),
              ("-", binOpType),
              ("*", binOpType),
              ("/", binOpType)
            ]
  env <-
    foldM
      (\env def -> union env <$> typecheckDefinitionPass1 def)
      binOpEnv
      definitions

  runReader env $ forM_ definitions typecheckDefinitionPass2

typecheckProg :: [Definition] -> (Either CompilerError (NextTypeId, (M.Map TypeVar Type, ())))
typecheckProg = run . runError . runState (NextTypeId 0) . runState (M.empty) . typecheckProgram