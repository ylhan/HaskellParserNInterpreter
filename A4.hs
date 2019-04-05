module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = whitespaces *> expr <* eof
  where
    expr = cond <|> lambda <|> let' <|> infix'
    cond = do
        keyword "if"
        ifBlock <- expr
        keyword "then"
        thenBlock <- expr
        keyword "else"
        elseBlock <- expr
        pure (Cond ifBlock thenBlock elseBlock)
    lambda = do
        char '\\'
        whitespaces
        param <- var
        -- For some reason keyword doesn't work with ->
        char '-'
        char '>'
        whitespaces
        body <- expr
        pure (Lambda param body)
    let' = do
        keyword "let"
        eqns <- many equation
        keyword "in"
        e <- expr
        pure (Let eqns e)
    equation = do
        v <- var
        char '=' *> whitespaces
        e <- expr
        char ';' *> whitespaces
        pure (v, e)
    -- TODO: CHECK IF 0 OR 1
    infix' = chainl2 arith (string "==" *> whitespaces *> pure (Prim2 Eq))
         <|> chainl2 arith (char '<' *> whitespaces *> pure (Prim2 Lt))
         <|> arith
    arith =  chainl1 addend ((char '-' *> whitespaces *> pure (Prim2 Minus))
         <|> (char '+' *> whitespaces *> pure (Prim2 Plus)))
    addend = chainl1 factor ((char '*' *> whitespaces *> pure (Prim2 Mul))
         <|> (char '/' *> whitespaces *> pure (Prim2 Div))
         <|> (char '%' *> whitespaces *> pure (Prim2 Mod)))
    -- TODO: What happens if only 1 (Not two or more?) still App?
    factor = chainl1 factors (pure (App)) <|> atom
    factors = do
        var1 <- atom
        var2 <- atom
        pure (App var1 var2)
        <|> atom
    -- TODO: handle minus sign some how
    atom = fmap Num integer
         <|> keyword "True" *> pure (Bln True)
         <|> keyword "False" *> pure (Bln False)
         <|> between (char '(' *> whitespaces)
                     (char ')' *> whitespaces)
                     expr
         <|> (fmap Var var)
    var = identifier ["if", "then", "else", "let", "in", "True", "False"]

-- | Same as chainl1 except it only it's 0 or 1 not 1 or many
-- left-associative way.
chainl2 :: Parser a               -- ^ operand parser
        -> Parser (a -> a -> a)   -- ^ operator parser
        -> Parser a               -- ^ evaluated answer
chainl2 arg op = do
    a <- arg
    optional a
  where
    optional x = do
        f <- op
        y <- arg
        return (f x y)


mainInterp :: Expr -> Either Error Value
mainInterp = error "TODO"