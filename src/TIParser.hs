module TIParser (Expr(..), tiParse) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative()
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

type Name = String

data Expr = Label Name
  | Var Char
  | VarBind Name Expr
  | Goto Name
  | DynVar String
  | Raw String
  | Let String Expr
  | Store Expr Expr
  | Nat String
  | Math [Expr]
  | Op Char
  | Block [Expr]
  | Str String
  | If Expr Expr Expr
  | FCall Name [Expr]
  | ListLit [Expr]
  | RawLabel Name deriving Show


expr :: Parser Expr
expr = goto <||> ifParser <||> forwardStore <||> backwardStore <||> letParser <||> varBindParser <||> exprNotStore

exprNotStore :: Parser Expr 
exprNotStore = labelParser <||> listLitParser <||> fcall <||> str <||> block <||> math <||> nat <||> raw <||> var {-<||> fcall-} <||> dynVar

storage :: Parser Expr 
storage = raw <||> var <||> fcall <||> dynVar

mathable :: Parser Expr 
mathable = listLitParser <||> fcall <||> nat <||> storage

storeable :: Parser Expr
storeable = str <||> math <||> mathable

-- newline :: Parser Char
-- newline = char '\n'

listLitParser :: Parser Expr
listLitParser = ListLit <$> between (char '[') (char ']') (mathable `sepBy` (spaces *> char ',' <* spaces))
  --between (char '[') (char ']') (mathable `sepBy` (spaces *> char ',' <* spaces)) >>= return . ListLit

ifParser :: Parser Expr
ifParser = do
  string "if" <* spaces
  cond <- between (char '(') (char ')') storeable <* spaces
  ifBody <- block <* spaces
  mElseBody <- optionMaybe $ string "else" *> spaces *> (block <||> ifParser)
  return . If cond ifBody $ fromMaybe (Raw "") mElseBody
  -- case mElseBody of
    -- Just elseBody -> elseBody
    -- Nothing -> Raw ""


-- examples
-- %ClrHome% --> Raw "ClrHome"
goto :: Parser Expr
goto = do
  DynVar c <- string "goto" *> spaces *> dynVar
  return $ Goto c

str :: Parser Expr
str = (Str . map toUpper) <$> between (char '"') (char '"') (many (noneOf "\""))

block :: Parser Expr
block = Block <$> between (char '{') (char '}') (many1 (many (oneOf "\n ") *> expr <* many (oneOf "\n ")))

-- TODO verify actually math expression
math :: Parser Expr
math = do
  c <- many1 (between (char '(') (char ')') math <||> mathable <||> try (spaces *> mathop <* spaces))
  if length c <= 1 then fail "not math" else return $ Math c

validMathOps :: String
validMathOps = "+*/-^<>="

mathop :: Parser Expr
mathop = do
  c <- lookAhead . try $ oneOf validMathOps
  case c of
    '-' -> do
      a <- lookAhead (try $ string "->") <||> return "-"
      case a of
        "->" -> fail "infringing on a store"
        "-" -> do
          c2 <- oneOf validMathOps
          return $ Op c2
    _ -> do
      c2 <- oneOf validMathOps
      return $ Op c2

  -- (oneOf "+*/" <||> (try (char '-') <* notFollowedBy (char '>')))
  -- return $ Op c

nat :: Parser Expr
nat = do
  c <- many1 (digit <||> char '.')
  return $ Nat c

fcall :: Parser Expr
fcall = do
  DynVar fname <- dynVar
  es <- between (char '(') (char ')') (exprNotStore `sepBy` (spaces *> char ',' <* spaces))
  return $ FCall fname es


dynVar :: Parser Expr
dynVar = do
  c <- letter
  mcs <- optionMaybe $ many1 (alphaNum <||> char '_')
  return . DynVar $ case mcs of
    Just cs -> c:cs
    Nothing -> [c]
  -- return $ DynVar (c:cs)

backwardStore :: Parser Expr
backwardStore = do
  v <- storage
  spaces >> string "<-" >> spaces
  e <- storeable
  return $ Store e v

forwardStore :: Parser Expr
forwardStore = do
  -- e <- manyTill (storeable <* spaces) (try $ string "->" <* spaces)
  e <- storeable
  spaces >> string "->" >> spaces
  v <- storage
  return $ Store e v

raw :: Parser Expr
raw = do
  char '%'
  s <- many1 (noneOf "%")
  char '%'
  return $ Raw s

-- examples
-- $a --> Var 'A'

var :: Parser Expr
var = do
  char '$'
  c <- letter
  return $ Var $ toUpper c

letParser :: Parser Expr
letParser = do
  DynVar name <- string "let" *> spaces *> dynVar <* spaces <* char '=' <* spaces
  v <- exprNotStore
  return $ Let name v

varBindParser :: Parser Expr
varBindParser = do
  DynVar name <- string "var" *> spaces *> dynVar <* spaces <* char '=' <* spaces
  v <- exprNotStore
  return $ VarBind name v

labelParser :: Parser Expr
labelParser = do -- yay
  name <- string "label" *> spaces *> many1 alphaNum <* char ':'
  return $ Label name

infixr 1 <||>
(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b


tiParse :: String -> Either ParseError [Expr]
tiParse = parse (many1 (many (oneOf "\n ") *> expr <* many (oneOf "\n ")) <* eof) "ti parser"
