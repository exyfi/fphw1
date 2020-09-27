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

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

reserved = ["def", "if", "while", "return", "not", "and", "or", "True", "False", "None"]

rword w = string w *> notFollowedBy alphaNumChar *> sc

collect :: [Stmt] -> Block
collect = sequence_ . fmap liftF

indented :: Parser (L.IndentOpt Parser a b) -> Parser a
indented = L.indentBlock scn

parseInt :: Parser Int
parseInt = lexeme $ choice [ L.decimal
                           , string "0b" >> L.binary
                           , string "0o" >> L.octal
                           , string "0x" >> L.hexadecimal
                           ]

parseBool :: Parser Bool
parseBool = rword "True" *> return True
        <|> rword "False" *> return False

parseStr :: Parser String
parseStr = (char '"' >> manyTill L.charLiteral (char '"')) <|>
           (char '\'' >> manyTill L.charLiteral (char '\''))

parseTerm :: Parser Expr
parseTerm = parens parseExpr
        <|> try parseCall
        <|> Ref <$> try parseIdent
        <|> (Lit . VInt) <$> parseInt
        <|> (Lit . VString) <$> parseStr
        <|> (Lit . VBool) <$> parseBool
        <|> rword "None" *> return (Lit VNone)

parseCall :: Parser Expr
parseCall = do
    fun <- parseIdent
    args <- parens $ parseExpr `sepBy` (symbol ",")
    return $ Call fun args

parseIdent :: Parser Ident
parseIdent = (lexeme . try) (p >>= check)
    where p = liftA2 (:) letterChar (many alphaNumChar)
          check x = if x `elem` reserved then fail $ "invalid parseIdent: " <> show x
                                         else return x

parseExprStmt :: Parser Stmt
parseExprStmt = do
    e <- parseExpr
    scn
    return $ Expr e

parseAssign :: Parser Stmt
parseAssign = do
    lhs <- parseIdent
    symbol "="
    rhs <- parseExpr
    scn
    return $ Assign lhs rhs

parseRet :: Parser Stmt
parseRet = do
    rword "return"
    e <- parseExpr
    scn
    return $ Ret e

parseIf :: Parser Stmt
parseIf = indented $ do
    rword "if"
    e <- parseExpr
    symbol ":"
    parseIndentedBlock $ If e

parseWhile :: Parser Stmt
parseWhile = indented $ do
    rword "while"
    e <- parseExpr
    symbol ":"
    parseIndentedBlock $ While e

parseDef :: Parser Stmt
parseDef = indented $ do
    rword "def"
    fun <- parseIdent
    params <- parens $ parseIdent `sepBy` (symbol ",")
    symbol ":"
    parseIndentedBlock $ Def fun params

parser :: Parser Block
parser = parseTopLevelBlock <* eof

parseTopLevelBlock :: Parser Block
parseTopLevelBlock = L.nonIndented scn parseBlock

parseBlock :: Parser Block
parseBlock = collect <$> some parseStmt

parseIndentedBlock :: (Block -> Stmt) -> Parser (L.IndentOpt Parser Stmt Stmt)
parseIndentedBlock f = return $ L.IndentSome Nothing (return . f . collect) parseStmt

parseStmt :: Parser Stmt
parseStmt = choice $ map try [ parseAssign
                             , parseExprStmt
                             , parseRet
                             , parseIf
                             , parseWhile
                             , parseDef ]

parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm operators
    where operators = [ [ InfixL (Binop Mul <$ symbol "*")
                        , InfixL (Binop Div <$ symbol "/")
                        , InfixL (Binop Mod <$ symbol "%") ]
                      , [ InfixL (Binop Add <$ symbol "+")
                        , InfixL (Binop Sub <$ symbol "-") ]
                      , [ InfixL (Binop Eq <$ symbol "==")
                        , InfixL (Binop Ne <$ symbol "!=")
                        , InfixL (Binop Lt <$ symbol "<")
                        , InfixL (Binop Le <$ symbol "<=")
                        , InfixL (Binop Gt <$ symbol ">")
                        , InfixL (Binop Ge <$ symbol ">=") ]
                      , [ InfixL (Binop And <$ symbol "and") ]
                      , [ InfixL (Binop Or <$ symbol "or") ]
                      ]
