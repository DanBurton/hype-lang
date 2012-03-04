module Hype.Interp where

import Hype.Syntax
import Hype.Pretty
import Hype.Parse

import qualified Data.Map as M
import Control.Applicative((<$>),(<*>))
import Data.List (intercalate, foldl')

data HypeContext = HC {
    hDefs :: M.Map String Value
  , hData :: M.Map String Constructor
  }

data Value = Closure { runClosure :: Value -> Value
                     , runProjection :: Value -> Value }
           | Value { valCons :: Constructor
                   , subVals :: [Value]
                   }

instance Show Value where
  show Closure{} = "< Closure >"
  show (Value c vs) = show (consName c) ++ case vs of
    [] -> ""
    _  -> "{" ++ intercalate ", " (map show vs) ++ "}"

calcHype :: Expression -> HypeContext -> Maybe Value
calcHype (Id (Identifier i)) cx = M.lookup i . hDefs $ cx
calcHype (ProjExpr n f) cx =
  project <$> calcHype n cx <*> calcHype f cx
calcHype (Construction (ConsName n) es) cx@(HC _ ns) =
   construct <$> M.lookup n ns <*> mapM (flip calcHype cx) es
calcHype (App f x) cx = apply <$> calcHype f cx <*> calcHype x cx

evalHype :: String -> Hype -> Maybe Value
evalHype str hy = calcHype cx (mkContext hy)
  where Right cx = testParser exprP str

apply :: Value -> Value -> Value
apply c@Closure{} x = runClosure c x
apply _ _ = error "Can't apply argument to non-closure"

project :: Value -> Value -> Value
project v f@Closure{} = runProjection f v
project _ _ = error "cannot project non-closure"

construct :: Constructor -> [Value] -> Value
construct c@(Constructor n fs) vs
  | length fs == length vs = Value c vs
  | length fs < length vs = error "Too few inputs to constructor" 
  | length fs > length vs = error "Too many inputs to constructor"

mkContext :: Hype -> HypeContext
mkContext (Hype _ ds fs) = HC myDefs myData
  where myDefs = foldl' step M.empty fs
          where step m (Def _ ls) = foldl' step' m ls
                step' m defLine = undefined -- TODO
        myData = foldl' step M.empty ds
          where step m (Data _ _ cs) = foldl' step' m cs
                step' m c@(Constructor (ConsName n) fs) =
                  case M.lookup n m of
                    Nothing -> M.insert n c m
                    Just _ -> error "Conflicting constructor names"

