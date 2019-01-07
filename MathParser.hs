module Main (main) where

-- stack: resolver: lts-13.1

import Control.Monad (void)
import Control.Applicative (empty)
import Control.Monad.Combinators.Expr   -- parser-combinators
import Data.Void
import Text.Megaparsec                  -- megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


{-

b ::= true | false | not b | b opb b | a opr a | (b)
a ::= f(r) | x | n | - a | a opa a | (a)
opa ::= + | - | * | /
opb ::= and | or
opr ::= == | >= | > | <= | <
s ::= x = e | s1; s2
e ::= let s in e1 | if b then e1 else e2 | a | (e)
r ::= e | e1, e2

-}


-- | Boolean expression.
data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

-- | Boolean binary operators.
data BBinOp
  = And
  | Or
  deriving (Show)

-- | Relational binary operators.
data RBinOp
  = Equal
  | GreaterOrEqual
  | Greater
  | LessOrEqual
  | Less
  deriving (Show)

-- | Arithmetic expressions.
data AExpr
  = FuncAExpr String [Expr]
  | Var String
  | FConst Double
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  deriving (Show)

-- | Arithmetic binary operators.
data ABinOp
  = Add
  | Subtract 
  | Multiply
  | Divide
  deriving (Show)

-- | Statements.
data Stmt
  = Assign String AExpr
  | Seq [Stmt]
  deriving (Show)
  
-- | General expressions.
data Expr
  = Let Stmt Expr
  | If BExpr Expr Expr
  | Expr AExpr
  deriving (Show)

type Parser = Parsec Void String


-- | Lexer

-- | Space consumer.
sc :: Parser ()
sc = L.space space1 lineComment empty
  where
    lineComment = L.skipLineComment "#"

-- | Lexeme.
--   Consumes trailling whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol.
--   Consumes trailing space.
symbol = L.symbol sc

-- | Parenthesized expression.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Integer.
float :: Parser Double
float = lexeme L.float

-- | Comma.
comma :: Parser String
comma = symbol ","

-- | Semicolon.
semicolon :: Parser String
semicolon = symbol ";"

-- | Reserved word.
--   Checks that parsed reserved word is a prefix of an identifier
rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- | List of reserved words.
rws :: [String]
rws = ["if", "then", "else", "true", "false", "not", "and", "or"]

-- | Identifier.
--   Checks that parsed identifier is not a reserved word
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


-- | Parser.

-- | Parser.
--   Handles initial whitespace.
parser :: Parser Expr
parser = between sc eof expr

-- | Statement parser.
--   Statements are separated by semicolons
stmt = f <$> sepBy1 stmt' semicolon
  where
    -- a statement may be single or multiple
    f s = if length s == 1 then head s else Seq s

-- Currently, there is only one statement type: assignment
-- Perhaps there will be more in the future
stmt' :: Parser Stmt
stmt' = assignStmt

-- Only allow assignment of arithmetic expression for now.
assignStmt :: Parser Stmt
assignStmt = do
  name <- identifier
  void (symbol "=")
  expr <- aExpr
  return (Assign name expr)

-- | Expression parser.
expr :: Parser Expr
expr = parens expr
  <|>  letExpr
  <|>  ifExpr
  <|>  Expr <$> aExpr

args :: Parser [Expr]
args = sepBy1 expr comma

funcAExpr :: Parser AExpr
funcAExpr = do
  name <- identifier
  args <- parens args
  return (FuncAExpr name args)

ifExpr :: Parser Expr
ifExpr = do
  rword "if"
  cond <- bExpr
  rword "then"
  expr1 <- expr
  rword "else"
  expr2 <- expr
  return (If cond expr1 expr2)

letExpr :: Parser Expr
letExpr = do
  rword "let"
  stmt <- stmt
  rword "in"
  expr <- expr
  return (Let stmt expr)

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

-- | Arithmetic operators.
aOperators :: [[Operator Parser AExpr]]
aOperators = 
  [ [ Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

-- | Boolean operators.
bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [ Prefix (Not <$ rword "not") ]
  , [ InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or  <$ rword "or") ]
  ]

aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> try funcAExpr
  <|> Var    <$> identifier
  <|> FConst <$> float

bTerm :: Parser BExpr
bTerm = parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

-- | Relational expression.
rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation
  =   (Equal <$ symbol "==")
  <|> (GreaterOrEqual <$ symbol ">=")
  <|> (Greater <$ symbol ">")
  <|> (LessOrEqual <$ symbol "<=")
  <|> (Less <$ symbol "<")

main :: IO ()
main = do
  input <- getContents
  parseTest parser input

