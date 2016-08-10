-- | Parser para el lenguaje imperativo simple. El parser
-- toma como entrada una lista de tokens.
module Parser where

import Lang
import Lexer

import Text.Parsec

type TParser = Parsec [Token] ()

parser :: [Token] -> Either ParseError Comm
parser = runParser pComm () "lexer"

empty :: TParser ()
empty = do s <- getInput
           if null s then return ()
           else fail ""

-- | Un parser muy sencillo para skip
pskip :: TParser Comm
pskip = do _ <- silentKw KSkip
           return Skip

-- | Un parser para if ... then ... else 
pif :: TParser Comm
pif = do _ <- silentKw KIf
         b <- pAssert
         _ <- silentKw Then
         c <- pComm
         _ <- silentKw Else
         c' <- pComm
         _ <- silentKw Fi
         return $ If b c c'

pwhile :: TParser Comm
pwhile = do _ <- silentKw KWhile
            b <- pAssert
            _ <- silentKw Do
            c <- pComm
            _ <- silentKw Od
            return $ While b c

pAssign :: TParser Comm
pAssign = do v <- pvar
             _ <- silentOp TokAssign
             e <- pIntExp
             return $ Assign v e

pNewVar :: TParser Comm
pNewVar = do _ <- silentKw KNewVar
             v <- pvar
             _ <- silentOp TokAssign
             e <- pIntExp
             _ <- silentKw In
             _ <- silentSym OpenCurly
             c <- pComm
             _ <- silentSym CloseCurly
             return $ NewVar v e c

-- | Parser para comandos que no tienen secuencias. (COMPLETAR)
pComm' :: TParser Comm
pComm' = choice [pif,pskip,pAssign,pwhile, pNewVar]

-- | Parser de secuencias
pseq :: TParser Comm
pseq = do c <- pComm'
          _ <- silentOp TokSemicolon
          c' <- pComm
          return $ Seq c c'

-- | Parser de comandos: o bien una secuencia o bien un comando solito.
pComm :: TParser Comm
pComm = try pseq <|> pComm'


pvar :: TParser Iden
pvar = ptk str
    where str (Str v) = Just v
          str _ = Nothing

-- | Parser para expresiones enteras. (COMPLETAR)
pIntExp :: TParser IntExpr
pIntExp = pIntExp3

pIntExp3 :: TParser IntExpr
pIntExp3 = do
                e1 <- pIntExp2
                e' <- pIntExp3'
                case e' of
                    Nothing -> return e1
                    Just (operator, e2) -> return (IBin operator e1 e2)

pIntExp3' :: TParser (Maybe (OpInt, IntExpr))
pIntExp3' = choice[ pOperatorAndExpression TokPlus Plus pIntExp3,
                    pOperatorAndExpression TokMinus Minus pIntExp3,
                    return Nothing]

-- pOperatorAndExpression :: Operator -> OpInt -> TParser IntExpr -> TParser (Maybe (OpInt, IntExpr))
pOperatorAndExpression token binOp parser = do
                                                _ <- silentOp token
                                                e1 <- parser
                                                return (Just (binOp, e1))

pIntExp2 :: TParser IntExpr
pIntExp2 = choice   [pIntExp1,
                    silentOp TokMinus >> pIntExp2 >>= (return . Neg)]

pIntExp1 :: TParser IntExpr
pIntExp1 =  do
                e1 <- pAtom
                e' <- pIntExp1'
                case e' of
                    Nothing -> return e1
                    Just (operator, e2) -> return (IBin operator e1 e2)

pIntExp1' :: TParser (Maybe (OpInt, IntExpr))
pIntExp1' = choice[ pOperatorAndExpression TokTimes Times pIntExp1,
                    pOperatorAndExpression TokDiv Div pIntExp1,
                    pOperatorAndExpression TokMod Mod pIntExp1,
                    return Nothing]


pAtom :: TParser IntExpr
pAtom = choice [var',
                num,
                pSandwich OpenParen CloseParen pIntExp]
            where var' = do v <- pvar
                            return $ Var v
                  num = do n <- ptk isNum
                           return $ Const n
                  isNum (Num n) = Just n
                  isNum _ = Nothing


pSandwich symbol1 symbol2 parser = do
                                    _ <- silentSym symbol1
                                    p <- parser
                                    _ <- silentSym symbol2
                                    return p


-- | Parser para constantes booleanas.
pBoolConst :: TParser Assert
pBoolConst = do b <- pkws [ (KTrue,CTrue) 
                          , (KFalse,CFalse)
                          ]
                return b

-- | Parser para expresiones booleanas.
pAssert :: TParser Assert
pAssert = pAssert4

pAssert4 = do
                e1 <- pAssert3
                e' <- pAssert4'
                case e' of
                    Nothing -> return e1
                    Just (operator, e2) -> return (ABin operator e1 e2)

--pAssert4' :: TParser (Maybe (OpBool, Assert))
pAssert4' = choice[ pOperatorAndExpression TokImpl Implies pAssert4,
                    pOperatorAndExpression TokIff Iff pAssert4,
                    return Nothing]

pAssert3 = do
                e1 <- pAssert2
                e' <- pAssert3'
                case e' of
                    Nothing -> return e1
                    Just (operator, e2) -> return (ABin operator e1 e2)

pAssert3' = choice[ pOperatorAndExpression TokOr Or pAssert3,
                    pOperatorAndExpression TokAnd And pAssert3,
                    return Nothing]

pAssert2 = choice   [pAssert1,
                     silentOp TokNot >> pAssert2 >>= (return . Not)]

pAssert1 = choice  [pAtomAssert,
                    pRel]

pRel = do
                e1 <- pIntExp
                e' <- pRel'
                case e' of
                    Just (operator, e2) -> return (ABinRel operator e1 e2)

pRel' = choice [pOperatorAndExpression TokLess Lt pIntExp,
                pOperatorAndExpression TokEqual Eq pIntExp,
                pOperatorAndExpression TokGreater Gt pIntExp,
                pOperatorAndExpression TokLtEq LtEq pIntExp,
                pOperatorAndExpression TokGtEq GtEq pIntExp,
                pOperatorAndExpression TokNotEq NEq pIntExp]

pAtomAssert = choice [pBoolConst,
                      pSandwich OpenParen CloseParen pAssert]

-- | Consumimos un keyword y nada más.
silentKw :: Keyword -> TParser ()
silentKw k = silent isK k
    where isK (Kw k') = Just k'
          isK _ = Nothing

-- | Consumimos un operador y nada más.
silentOp :: Operator -> TParser ()
silentOp o = silent isOp o
    where isOp (Op o') = Just o'
          isOp _ = Nothing

-- | Consumimos un símbolo y nada más.
silentSym :: Symbol -> TParser ()
silentSym s = silent isSym s
    where isSym (Sym s') = Just s'
          isSym _ = Nothing

-- | Si tenemos la suerte que isA para el token actual devuelva justo
-- a', entonces comparamos a con a', en ese caso consumimos el token
-- actual si no, no consumimos nada.
silent :: Eq a => (Token -> Maybe a) -> a -> TParser ()
silent isA a = ptk check
    where check t = case isA t of
                      Just a' -> if a == a' then Just () else Nothing
                      Nothing -> Nothing

-- | 
pkws :: [(Keyword,a)] -> TParser a
pkws cs = ptk l
    where l (Kw k) = lookup k cs
          l _ = Nothing

-- | Dada una función que, con suerte, nos devuelve un a, vemos 
-- un token, y si tuvimos suerte lo consumimos y devolvemos un a.
ptk :: (Token -> Maybe a) -> TParser a
ptk f = tokenPrim show incpos f

-- | Cada vez que consumimos un token incrementamos la posición de
-- nuestro parser (notar que ya perdimos la relación con la posición
-- en el string original).
incpos :: SourcePos -> Token -> [Token] -> SourcePos
incpos p _ _ = incSourceColumn p 1
