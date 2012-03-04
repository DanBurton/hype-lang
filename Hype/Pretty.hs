module Hype.Pretty where

import Hype.Syntax
import Data.List (intercalate)

instance Show Hype where
  show (Hype m tys ds) =
    "#module " ++ m ++ "\n"
    ++ "\n"
    ++ "#Data\n\n"
    ++ (unlines $ map show tys)
    ++ "\n"
    ++ "#Definitions\n\n"
    ++ (unlines $ map show ds)
    ++ "\n"

instance Show Data where
  show (Data n tys cs) =
    show n ++ intercalate " " (map show tys) ++ "\n"
    ++ (unlines $ map (("  " ++) . show) cs)

instance Show Constructor where
  show (Constructor n fs) =
    show n ++ case fs of
      [] -> ""
      _ -> "{" ++ intercalate ", " (map show fs) ++ "}"


instance Show Field where
  show (Field n ty) = show n ++ " : " ++ show ty

instance Show ConsName where
  show (ConsName n) = n

instance Show Identifier where
  show (Identifier n) = n

instance Show Type where
  show (Type n) = n

instance Show TypeName where
  show (TypeName n) = n

instance Show DefHead where
  show (DefHead n ty) = show n ++ " : " ++ show ty

instance Show Def where
  show (Def h ls) =
    case h of
      Just h' -> show h' ++ "\n"
      Nothing -> ""
    ++ (unlines $ map show ls)

instance Show DefLine where
  show (DefLine lhs rhs) =
    show lhs ++ " = " ++ show rhs

instance Show LHS where
  show (Projection p f as) =
    show p ++ "." ++ show f ++ case as of
      [] -> ""
      _ -> " " ++ intercalate " " (map show as)
  show (NonProjection f as) =
    show f ++ case as of
      [] -> ""
      _ -> " " ++ intercalate " " (map show as)
      

instance Show Binding where
  show (BName n) = show n
  show (BCons n bs) = show n ++ case bs of
    [] -> ""
    _ -> "{" ++ intercalate ", " (map show bs) ++ "}"

instance Show Expression where
  show (ProjExpr n f) = "(" ++ show n ++ ").(" ++ show f ++ ")"
  show (Id i) = show i
  show (Construction n es) = show n ++ case es of
    [] -> ""
    _ -> "{" ++ intercalate ", " (map show es) ++ "}"
  show (App f x) = "(" ++ show f ++ ") (" ++ show x ++ ")"

