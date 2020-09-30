module HW.Parse where

import HW.Lang
import Data.Void
import Control.Monad
import Control.Applicative hiding (some, many)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

lineComment = L.skipLineComment "#"

-- space/comment
sc :: Parser ()
sc = L.space (void $ oneOf [' ', '\t']) lineComment empty

-- space/comment/newline
scn :: Parser ()
scn = L.space space1 lineComment empty

-- parse lexeme and consume trailing space
lexeme = L.lexeme sc

-- parse a symbol
symbol = L.symbol sc

-- surround a parser with parens
parens = between (symbol "(") (symbol ")")

-- reserved identifiers
reserved = ["def", "if", "while", "return", "not", "and", "or", "True", "False", "None"]

-- parses a reserved identifier
rword w = string w *> notFollowedBy alphaNumChar *> sc

-- convert a list of statements to the free monad
collect :: [Stmt] -> Block
collect = sequence_ . fmap liftF

indented :: Parser (L.IndentOpt Parser a b) -> Parser a
indented = L.indentBlock scn

-- parse an int literal
parseInt :: Parser Int
parseInt = lexeme $ choice [ L.decimal
                           , string "0b" >> L.binary
                           , string "0o" >> L.octal
                           , string "0x" >> L.hexadecimal
                           ]

-- parse a bool literal
parseBool :: Parser Bool
parseBool = rword "True" *> return True
        <|> rword "False" *> return False

-- parse a string literal
parseStr :: Parser String
parseStr = (char '"' >> manyTill L.charLiteral (char '"')) <|>
           (char '\'' >> manyTill L.charLiteral (char '\''))

-- parse expression term
parseTerm :: Parser Expr
parseTerm = parens parseExpr
        <|> try parseCall
        <|> Ref <$> try parseIdent
        <|> (Lit . VInt) <$> parseInt
        <|> (Lit . VString) <$> parseStr
        <|> (Lit . VBool) <$> parseBool
        <|> rword "None" *> return (Lit VNone)

-- parse a function call
parseCall :: Parser Expr
parseCall = do
    fun <- parseIdent
    args <- parens $ parseExpr `sepBy` (symbol ",")
    return $ Call fun args

-- parse an identifier
parseIdent :: Parser Ident
parseIdent = (lexeme . try) (p >>= check)
    where p = liftA2 (:) letterChar (many alphaNumChar)
          check x = if x `elem` reserved then fail $ "invalid parseIdent: " <> show x
                                         else return x

-- parse a statement consisting of a single expression
parseExprStmt :: Parser Stmt
parseExprStmt = do
    e <- parseExpr
    scn
    return $ Expr e

-- parse an assignment statement
parseAssign :: Parser Stmt
parseAssign = do
    lhs <- parseIdent
    symbol "="
    rhs <- parseExpr
    scn
    return $ Assign lhs rhs

-- parse a return statement
parseRet :: Parser Stmt
parseRet = do
    rword "return"
    e <- parseExpr
    scn
    return $ Ret e

-- parse an if statement
parseIf :: Parser Stmt
parseIf = indented $ do
    rword "if"
    e <- parseExpr
    symbol ":"
    parseIndentedBlock $ If e

-- parse a while statement
parseWhile :: Parser Stmt
parseWhile = indented $ do
    rword "while"
    e <- parseExpr
    symbol ":"
    parseIndentedBlock $ While e

-- parse a function definition
parseDef :: Parser Stmt
parseDef = indented $ do
    rword "def"
    fun <- parseIdent
    params <- parens $ parseIdent `sepBy` (symbol ",")
    symbol ":"
    parseIndentedBlock $ Def fun params

-- root-level parser
parser :: Parser Block
parser = parseTopLevelBlock <* eof

-- parse a block without indentation (i.e. the whole file)
parseTopLevelBlock :: Parser Block
parseTopLevelBlock = L.nonIndented scn parseBlock

-- parse a statement block
parseBlock :: Parser Block
parseBlock = collect <$> some parseStmt

-- takes a function that converts block to statement and returns a parser
parseIndentedBlock :: (Block -> Stmt) -> Parser (L.IndentOpt Parser Stmt Stmt)
parseIndentedBlock f = return $ L.IndentSome Nothing (return . f . collect) parseStmt

-- try parsing various types of statement
parseStmt :: Parser Stmt
parseStmt = choice $ map try [ parseAssign
                             , parseExprStmt
                             , parseRet
                             , parseIf
                             , parseWhile
                             , parseDef ]

-- parse an expression
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operators
    where operators = [ [ Prefix (Unop Neg <$ symbol "-") ]
                      , [ Prefix (Unop Not <$ symbol "not") ]
                      , [ InfixL (Binop Mul <$ symbol "*")
                        , InfixL (Binop Div <$ symbol "/")
                        , InfixL (Binop Mod <$ symbol "%") ]
                      , [ InfixL (Binop Add <$ symbol "+")
                        , InfixL (Binop Sub <$ symbol "-") ]
                      , [ InfixL (Binop Eq <$ symbol "==")
                        , InfixL (Binop Ne <$ symbol "!=")
                        , InfixL (Binop Ge <$ symbol ">=")
                        , InfixL (Binop Le <$ symbol "<=")
                        , InfixL (Binop Gt <$ symbol ">")
                        , InfixL (Binop Lt <$ symbol "<") ]
                      , [ InfixL (Binop And <$ symbol "and") ]
                      , [ InfixL (Binop Or <$ symbol "or") ]
                      ]
