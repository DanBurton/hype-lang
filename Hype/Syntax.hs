module Hype.Syntax where

data DefLine = DefLine
  { defLhs :: LHS
  , defRhs :: Expression
  }

data Def = Def
  { defName :: Identifier
  , defType :: Type
  , defLines :: [DefLine]
  }

data LHS =
    Projection
    { projectee :: Binding
    , funcName :: Identifier
    , args :: [Binding]
    }
  | NonProjection
    { funcName :: Identifier
    , args :: [Binding]
    }

newtype ConsName = ConsName String
newtype Identifier = Identifier String
newtype TypeName = TypeName String
newtype Type = Type String


data Binding = BName { bindingName :: Identifier }
             | BCons { bcName :: ConsName
                     , subBindings :: [Binding]
                     }

data Expression = ProjExpr { exprProjectee :: Expression
                           , exprProjection :: Expression
                           }
                | Id { exprId :: Identifier }
                | Construction { exprConsName :: ConsName
                               , exprs :: [Expression]
                               }
                | App { exprFunc :: Expression
                      , exprArg :: Expression
                      }

data Data = Data
  { dName :: TypeName
  , typeParams :: [TypeName]
  , constructors :: [Constructor]
  }

data Constructor = Constructor
  { consName :: ConsName
  , fields :: [Field]
  }

data Field = Field
  { fieldName :: Identifier
  , fieldType :: Type
  }

data Hype = Hype { moduleName :: String
                 , datum :: [Data]
                 , defs :: [Def]
                 }

