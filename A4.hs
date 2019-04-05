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
mainInterp = interp Map.empty

intOrDie :: Value -> Either Error Integer
intOrDie (VN i) = pure i
intOrDie _ = Left TypeError

boolOrDie :: Value -> Either Error Bool
boolOrDie (VB b) = pure b
boolOrDie _ = Left TypeError

interp :: Map String Value -> Expr -> Either Error Value

interp _ (Num i) = pure (VN i)
interp _ (Bln b) = pure (VB b)

interp env (Var v) = case Map.lookup v env of
    Just a -> pure a
    Nothing -> Left VarNotFound

interp env (Prim2 Eq e1 e2) = do
    a <- interp env e1
    b <- interp env e2
    case a of
        VB _ -> case b of 
                  VB _ -> let i = boolOrDie a;
                              j = boolOrDie b;
                         in pure (VB (i == j))
                  VN _ -> Left TypeError
        VN _ -> case b of
                  VB _ -> Left TypeError
                  VN _ -> let i = intOrDie a;
                              j = intOrDie b;
                         in pure (VB (i == j))

interp env (Prim2 op e1 e2) = do
  a <- interp env e1
  i <- intOrDie a
  b <- interp env e2
  j <- intOrDie b
  case op of
    Plus -> pure(VN (i + j))
    Mul -> pure(VN (i * j))
    Minus -> pure(VN (i - j))
    Div -> case j == 0 of 
                False -> pure(VN (i `div` j))
                True -> Left DivByZero
    Mod -> pure(VN (mod i j))
    Lt -> pure(VB (i < j))

interp env (Cond test eThen eElse) = do
    a <- interp env test
    case a of
        VB True -> interp env eThen
        VB False -> interp env eElse
        _ -> Left TypeError

interp env (Let equations inClause) = do
    extendedEqs <- extend env equations
    interp extendedEqs inClause
  where
    extend env [] = pure env
    extend env ((v, rhs) : equations) = do
        a <- interp env rhs
        let extendedEqs = Map.insert v a env
        extend extendedEqs equations

interp env (Lambda v body) = pure (VClosure env v body)

interp env (App f e) = do 
    c <- interp env f
    case c of 
      VClosure fEnv v body -> do
        eVal <- interp env e
        let bEnv = Map.insert v eVal fEnv
        interp bEnv body
      _ -> Left TypeError
