module A4Def where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr
    = Num Integer
    | Bln Bool
    | Var String
    | Prim2 Op2 Expr Expr       -- Prim2 op operand operand
    | Let [(String, Expr)] Expr -- Let [(name, rhs), ...] body
    | Lambda String Expr        -- Lambda var body
    | App Expr Expr             -- App func arg
    | Cond Expr Expr Expr       -- Cond test then-branch else-branch
    deriving (Eq, Show)

data Op2 = Eq | Lt | Plus | Minus | Mul | Div | Mod
    deriving (Eq, Show)

data Value = VN Integer
           | VB Bool
           | VClosure (Map String Value) String Expr
    deriving (Eq, Show)

data Error = VarNotFound | TypeError | DivByZero deriving (Eq, Show)
