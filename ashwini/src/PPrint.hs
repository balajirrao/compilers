{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PPrint where

import Data.Functor.Foldable

import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import Data.Text.Lazy hiding (foldr, map)
import Data.Int

import Control.Monad(forM_, foldM, forM)

import Polysemy
import Polysemy.Reader
import Polysemy.State

import AST
import Type
import Unify
import TypeCheck

import Prelude hiding (replicate, print, unlines, lines, concat)

printIndent :: Int -> Text
printIndent x = replicate (fromIntegral x) "  "

indent :: Member (Reader Int) r => Sem r a -> Sem r a
indent = local (+ 1)

noIndent :: Member (Reader Int) r => Sem r a -> Sem r a
noIndent = local (const 0)

pprintPattern :: Member (Reader Int) r => Pattern -> Sem r (NE.NonEmpty Text)
pprintPattern (PatternVar x) = print (pack x)
pprintPattern (PatternCons constr params) = concat' $ (print $ pack constr) NE.:| (map (print . pack) params)

print :: Member (Reader Int) r => Text -> Sem r (NE.NonEmpty Text)
print t = do
    i <- ask
    return $ replicate (fromIntegral i) "  " <> t NE.:| []

concat' :: NE.NonEmpty (Sem r (NE.NonEmpty Text)) -> Sem r (NE.NonEmpty Text)
concat' (x NE.:| xs) = do
    px <- x
    pxs <- sequence xs
    let py = intercalate "\n" (NE.toList px)
        pys = intercalate "\n" <$> (NE.toList <$> pxs)

    return (py NE.:| pys)

pprint' :: Member (Reader Int) r => AST (Sem r (NE.NonEmpty Text)) -> Sem r (NE.NonEmpty Text)
pprint' (Number x) = print $ "INT: " <> (pack $ show x)
pprint' (LowerVar x) = print $ "LID: " <> (pack x)
pprint' (UpperVar x) = print $ "UID: " <> (pack x)
pprint' (BinOp op x y) = concat' $ (print $ "BINOP: " <> pack op) NE.:|
    [indent x, indent y ]
pprint' (App f x) = concat' $ (print $ "APP:") NE.:|
        [ indent f, indent x]
pprint' (Case x branches) = do
    (ppx NE.:| xs) <- noIndent x
    let ppBranches = map (\(Branch p e) -> indent $ concat' ((pprintPattern p) NE.:| [indent e])) branches
    concat' $ (print $ "CASE: " <> ppx) NE.:| (map print xs ++ ppBranches)

pprintSem :: Member (Reader Int) r => Fix AST -> Sem r (NE.NonEmpty Text)
pprintSem = fold pprint'

pprint :: Fix AST -> Text
pprint x = unlines $ NE.toList $ run $ runReader 0 (pprintSem x)

pprintType :: Member (Reader Bindings) r => Type -> Sem r Text
pprintType (TyVar x@(TypeVar tv)) = do
    bindings <- ask
    case (M.lookup x bindings) of
        Just t -> pprintType t
        Nothing -> pure $ pack tv
pprintType (TyBase t) = pure $ pack t
pprintType (TyFn t1 t2) = do
    left <- pprintType t1
    right <- pprintType t2
    pure $ case t2 of
        (TyFn _ _) -> left <> " -> (" <> right <> ")"
        _ -> left <> " -> " <> right

pprintDefinition ::  Member (Reader Int) r => Definition -> Sem r [Text]
pprintDefinition (Defn name params body) =
    (NE.toList <$>) <$> concat' $ (print .pack $ name) NE.:|
        [ print $ (intercalate " " (pack <$> params)), indent $ pprintSem body ]
pprintDefinition  (Data _ _) = return []

pprintTypeEnv :: Member (Reader Bindings) r => TypeEnv -> Sem r [Text]
pprintTypeEnv (TypeEnv tEnv) = printMapping <$> forM tEnv pprintType
    where
        printMapping :: M.Map String Text -> [Text]
        printMapping = M.foldrWithKey (\n t x -> (pack n <> ": " <> t) : x) []
