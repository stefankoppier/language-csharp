module Language.CSharp.Parser.Utility where

import Text.Parsec
import Text.Parsec.Pos
import Language.CSharp.Syntax
import Language.CSharp.Lexer

type P = Parsec [Positioned Token] ()

pEqualSign :: P ()
pEqualSign = pToken TOpAssign

pSemi :: P ()
pSemi = pToken TSemicolon

pPeriod :: P ()
pPeriod = pToken TPeriod

pComma :: P ()
pComma = pToken TComma

pColon :: P ()
pColon = pToken TColon

pLambda :: P ()
pLambda = pToken TLambda

betweenParens, betweenCurly, betweenSquare, betweenDiamond :: P a -> P a
betweenParens  = between pOParens pCParens
betweenCurly   = between pOCurly pCCurly
betweenSquare  = between pSOpen pSClose

betweenDiamond p = try (between (pToken TOpLessThan) (pToken TOpGreaterThan) p)
    <|> do pToken TOpLessThan
           r <- p
           i <- getInput
           case head i of
                (Positioned pos@(AlexPn a l c) TOpRightShift) 
                    -> setInput ( Positioned pos TOpGreaterThan 
                                : Positioned (AlexPn (a + 1) l (c + 1)) TOpGreaterThan : tail i)
                _   -> setInput i
           pToken TOpGreaterThan 
           return r

pOParens, pCParens :: P ()
pOParens = pToken TOParens
pCParens = pToken TCParens

pOCurly, pCCurly :: P ()
pOCurly = pToken TOCurly
pCCurly = pToken TCCurly

pSOpen, pSClose :: P ()
pSOpen  = pToken TOSquare
pSClose = pToken TCSquare

pName :: P Name
pName = do
    s  <- pIdentifier
    ss <- many (pToken TPeriod *> pIdentifier)
    return $ Name (s : ss)

pIdentifierKeyword :: String -> P ()
pIdentifierKeyword s = do
    name <- getSourceName
    token show (posFromToken name) matchToken
    where
        matchToken (Positioned _ t)
            | (TIdentifier s') <- t
            , s == s'   = Just ()
            | otherwise = Nothing

pIdentifier :: P Identifier
pIdentifier = do
    name <- getSourceName
    Identifier <$> token show (posFromToken name) matchToken
    where
        matchToken (Positioned _ (TIdentifier s)) = Just s
        matchToken _                              = Nothing

pToken :: Token -> P ()
pToken tok = do
    name <- getSourceName
    token show (posFromToken name) matchToken <?> show tok
    where
        matchToken (Positioned _ tok') = if tok == tok' then Just () else Nothing

eitherOrBoth :: P a -> P b -> P (Maybe a, Maybe b)
eitherOrBoth a b = do
    mA <- optionMaybe (try a)
    case mA of
        Nothing -> do
            b' <- try b
            mA <- optionMaybe a
            return (mA, Just b')
        Just a' -> do
            mB <- optionMaybe b
            return (Just a', mB)

chainlUnary1 :: P a -> P (a -> a) -> P a
chainlUnary1 p op = do
    fs <- many op
    x  <- p
    return $ foldr id x fs

chainPostfix :: P a -> P (a -> a) -> P a
chainPostfix p op = do x <- p; rest x
    where rest x = (do f <- op; rest $ f x) <|> return x

posFromToken :: SourceName -> Positioned a -> SourcePos
posFromToken source (Positioned (AlexPn _ l c) _) = newPos source l c 

getSourceName :: P SourceName
getSourceName = sourceName <$> getPosition