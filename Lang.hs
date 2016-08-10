-- | Definición del lenguaje.
module Lang where

-- | Los nombres de variables son strings, pero quizás 
-- querramos cambiarlo luego.
type Iden = String

-- | Expresiones enteras COMPLETAR
data IntExpr = Const Int
             | Var Iden
             | Neg IntExpr
             | IBin OpInt IntExpr IntExpr
             deriving (Show, Eq)

-- | Operadores binarios enteros, `Div` corresponde al cociente.
data OpInt = Plus | Minus | Times | Div | Mod
           deriving (Show, Eq)

-- | Expresiones booleanas COMPLETAR
data Assert = CTrue
            | CFalse
            | Not Assert
            | ABin OpBool Assert Assert
            | ABinRel OpRel IntExpr IntExpr
              deriving (Show, Eq)

-- | Operadores de relación (COMPLETAR)
data OpRel = Eq | NEq | Lt | LtEq | Gt | GtEq
           deriving (Show, Eq)

-- | Operadores binarios booleanos (COMPLETAR)
data OpBool = And | Or | Implies | Iff
            deriving (Show, Eq)

-- | Comandos (COMPLETAR)
data Comm = Skip 
          | Assign Iden IntExpr
          | If Assert Comm Comm
          | Seq Comm Comm
          | While Assert Comm
          | NewVar Iden IntExpr Comm
            deriving (Show, Eq)

-- Adivinen por qué están comentadas estas cosas.
-- -- | Nombres cómodos (aka combinadores) para construcciones:
-- plus :: IntExpr -> IntExpr -> IntExpr
-- plus = IBin Plus

-- infixr 8 .+
-- -- | Versión con operadores infijos
-- (.+) :: IntExpr -> IntExpr -> IntExpr
-- (.+) = IBin Plus
