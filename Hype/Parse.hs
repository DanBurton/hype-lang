module Hype.Parse where

import Hype.Syntax
import Text.Parsec hiding (space)
import Control.Applicative ((<$>))
import Data.Char (isUpper)

loadHype :: String -> IO (Either ParseError Hype)
loadHype file = parse hypeP file <$> readFile file

testParser :: Parser a -> String -> Either ParseError a
testParser = flip parse "test string"


type Parser a = Parsec String () a

skip :: Parser a -> Parser ()
skip p = do
  _ <- p
  return ()

newline' = skip newline
string' = skip . string
char' = skip . char
space = char ' '
space' = skip space

parens = between (char '(') (char ')')
braces = between (char '{') (char '}')

reservedChars = ". {}()[]#\n\t="

identifierCharP :: Parser Char
identifierCharP = noneOf reservedChars

wordStartsWith :: Parser Char -> Parser String
wordStartsWith f = do
  c <- f
  cs <- many identifierCharP
  return (c : cs)

upperWord = wordStartsWith upper
lowerWord = wordStartsWith (satisfy (\c -> not (isUpper c)
                                           && c `notElem` reservedChars))


hypeP :: Parser Hype
hypeP = (<?> "Hype program") $ do
  m <- moduleP
  newline'
  string' "#Data\n\n"
  tys <- many dataP
  newline'
  string' "#Definitions\n\n"
  ds <- many defP
  eof
  return $ Hype m tys ds

moduleP :: Parser String
moduleP = (<?> "module declaration") $ do
  string' "#module "
  n <- moduleNameP
  newline'
  return n

moduleNameP :: Parser String
moduleNameP = many1 letter <?> "module name"

dataP :: Parser Data
dataP = (<?> "data declaration") $ do
  n <- dataNameP
  tys <- option [] (space' >> typeNameP `sepBy` space)
  newline'
  cs <- endBy1 (string "  " >> constructorP) newline
  newline'
  return $ Data n tys cs

dataNameP :: Parser TypeName
dataNameP = TypeName <$> upperWord

typeNameP :: Parser TypeName
typeNameP = TypeName <$> upperWord

constructorP :: Parser Constructor
constructorP = (<?> "constructor") $ do
  name <- consNameP
  fs <- option [] $ braces (consFieldP `sepBy` string ", ")
  return $ Constructor name fs

consFieldP :: Parser Field
consFieldP = (<?> "constructor field") $ do
  n <- identifierP
  space'
  char' ':'
  space'
  ty <- typeP
  return $ Field n ty

identifierP :: Parser Identifier
identifierP = Identifier <$> lowerWord

typeP :: Parser Type
typeP = chainl1 typeP' typeAppP <?> "Type"
  where typeAppP = space' >> return typeAppend
        typeAppend (Type a) (Type b) = Type (a ++ " " ++ b)

typeP' :: Parser Type
typeP' = Type <$> many1 (noneOf ". {}[]\n\t") <?> "Single type"

defHeadP :: Parser DefHead
defHeadP = (<?> "function type declaration") $ do
  n <- identifierP
  string' " : "
  t <- typeP
  newline'
  return $ DefHead n t

defP :: Parser Def
defP = (<?> "function definition") $ do
  h <- optionMaybe $ try defHeadP
  ds <- many1 defLineP
  newline'
  return $ Def h ds

defLineP :: Parser DefLine
defLineP = (<?> "function definition line") $ do
  lhs <- lhsP
  string' "= "
  e <- exprP
  newline'
  return $ DefLine lhs e

lhsP :: Parser LHS
lhsP = try lhsProjP <|> lhsNoProjP <?> "lhs"

lhsArgsP :: Parser [Binding]
lhsArgsP = space' >> bindingP `endBy` space

lhsProjP :: Parser LHS
lhsProjP = do
  n <- bindingP
  char '.'
  f <- identifierP
  bs <- lhsArgsP
  return $ Projection n f bs

lhsNoProjP :: Parser LHS
lhsNoProjP = do
  f <- identifierP
  bs <- lhsArgsP
  return $ NonProjection f bs

bindingP :: Parser Binding
bindingP = try bConsP <|> try bNameP <?> "binding"

consNameP :: Parser ConsName
consNameP = ConsName <$> upperWord

bConsP :: Parser Binding
bConsP = do
  n <- consNameP
  bs <- option [] $ braces (bindingP `sepBy` string ", ")
  return $ BCons n bs

bNameP :: Parser Binding
bNameP = BName <$> identifierP

exprP :: Parser Expression
exprP = chainl1 exprP' appP <?> "expression"
  where appP = space' >> return App

exprP' :: Parser Expression
exprP' = chainl1 exprP'' projP <?> "single expresion"
  where projP = char' '.' >> return ProjExpr

exprP'' = idP <|> constructionP <|> (parens exprP)
          <?> "simple expression'"

idP :: Parser Expression
idP = Id <$> identifierP

constructionP :: Parser Expression
constructionP = do
  n <- consNameP
  bs <- option [] $ braces (exprP `sepBy` string ", ")
  return $ Construction n bs

