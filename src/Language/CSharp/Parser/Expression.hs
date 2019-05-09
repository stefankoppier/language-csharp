module Language.CSharp.Parser.Expression where

import Text.Parsec                    hiding (Empty, sourceName)
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility
import {-# SOURCE #-} Language.CSharp.Parser.Statement
import Language.CSharp.Parser.Type

pMaybeExpression :: P (Maybe Expression)
pMaybeExpression = optionMaybe pExpression

pExpression :: P Expression
pExpression = pExpressionP1

--Assignment operators: assignment.
pExpressionP1 :: P Expression
pExpressionP1 = pExpressionP2 `chainr1` pAssignment
    where
        pAssignment = do
            operator <- pAssignmentOperator
            return (\ target value -> Assign target operator value)

pAssignmentOperator :: P AssignmentOperator
pAssignmentOperator = choice 
    [ OpAssign                  <$ pToken TOpAssign
    , OpAssignPlus              <$ pToken TOpAssignPlus
    , OpAssignMinus             <$ pToken TOpAssignMinus
    , OpAssignMultiply          <$ pToken TOpAssignMultiply
    , OpAssignDivide            <$ pToken TOpAssignDivide
    , OpAssignModulo            <$ pToken TOpAssignModulo
    , OpAssignBitwiseAnd        <$ pToken TOpAssignBitwiseAnd       
    , OpAssignBitwiseOr         <$ pToken TOpAssignBitwiseOr        
    , OpAssignBitwiseXor        <$ pToken TOpAssignBitwiseXor       
    , OpAssignBitwiseLeftShift  <$ pToken TOpAssignBitwiseLeftShift 
    , OpAssignBitwiseRightShift <$ pToken TOpAssignBitwiseRightShift
    ]

-- Anonymous function expressions: lambda expression.
pExpressionP2 :: P Expression
pExpressionP2 = try pLambdaExpression <|> try pDelegateExpression <|> pExpressionP3

pLambdaExpression :: P Expression
pLambdaExpression = do
    sig <- pAnonymousFunctionSignature
    pLambda
    body <- pAnonymousFunctionBody
    return $ Lambda sig body

pDelegateExpression :: P Expression
pDelegateExpression = do
    pToken TKWdelegate
    sig  <- optionMaybe pExplicitFunctionSignature
    body <- betweenCurly pStatements
    return $ Delegate sig body

pAnonymousFunctionSignature :: P AnonymousFunctionSignature
pAnonymousFunctionSignature = choice
    [ try pExplicitFunctionSignature
    , betweenParens $ ImplicitAnonymousFunctionSignature <$> sepBy pIdentifier pComma
    , ImplicitAnonymousFunctionSignature . (:[]) <$> pIdentifier ]

pExplicitFunctionSignature :: P AnonymousFunctionSignature
pExplicitFunctionSignature = betweenParens $ 
    ExplicitAnonymousFunctionSignature <$> sepBy pAnonymousFunctionParameter pComma

pAnonymousFunctionParameter :: P AnonymousFunctionParameter
pAnonymousFunctionParameter = do
    modifier <- optionMaybe (RefParam <$ pToken TKWref <|> OutParam <$ pToken TKWout)
    ty       <- pType
    name     <- pIdentifier
    return $ ExplicitAnonymousFunctionParameter modifier ty name

pAnonymousFunctionBody :: P AnonymousFunctionBody
pAnonymousFunctionBody = choice
    [ AnonymousFunctionExpressionBody <$> pExpressionP1
    , AnonymousFunctionStatementBody  <$> betweenCurly pStatements ]

-- Conditional operator: conditional.
pExpressionP3 :: P Expression
pExpressionP3 = try pConditional <|> pExpressionP4

pConditional :: P Expression
pConditional = do
    guard <- pExpressionP4
    pToken TQuestion
    exp1 <- pExpressionP4
    pColon
    exp2 <- pExpressionP3
    return $ Conditional guard exp1 exp2

-- Query expressions: query.
pExpressionP4 :: P Expression
pExpressionP4 = pExpressionP5

-- The null coalescing operator: Null coalescing.
pExpressionP5 :: P Expression
pExpressionP5 = pExpressionP6 `chainr1` pNullCoalescing
    where
        pNullCoalescing = BinaryOperator BinaryNullCoalescing <$ pToken TOpNullCoalescing

-- Conditional logical operators: boolean or.
pExpressionP6 :: P Expression
pExpressionP6 = pExpressionP7 `chainl1` pBooleanOr
    where
        pBooleanOr = BinaryOperator BinaryOr <$ pToken TOpOr

-- Conditional logical operators: boolean and.
pExpressionP7 :: P Expression
pExpressionP7 = pExpressionP8 `chainl1` pBooleanAnd
    where
        pBooleanAnd = BinaryOperator BinaryAnd <$ pToken TOpAnd

-- Logical operators: bitwise or.
pExpressionP8 :: P Expression
pExpressionP8 = pExpressionP9 `chainl1` pBitwiseOr
    where
        pBitwiseOr = BinaryOperator BinaryBitwiseOr <$ pToken TOpBitwiseOr

-- Logical operators: bitwise xor.
pExpressionP9 :: P Expression
pExpressionP9 = pExpressionP10 `chainl1` pBitwiseXor
    where
        pBitwiseXor = BinaryOperator BinaryBitwiseXor <$ pToken TOpBitwiseXor

-- Logical operators: bitwise and.
pExpressionP10 :: P Expression
pExpressionP10 = pExpressionP11 `chainl1` pBitwiseAnd
    where
        pBitwiseAnd = BinaryOperator BinaryBitwiseAnd <$ pToken TOpBitwiseAnd

-- Relational and type-testing operators: equality.
pExpressionP11 :: P Expression
pExpressionP11 = pExpressionP12 `chainl1` pOperators
    where
        pOperators = choice 
            [ BinaryOperator BinaryEquals    <$ pToken TOpEqual
            , BinaryOperator BinaryNotEquals <$ pToken TOpNotEqual ]

-- Relational and type-testing operators: relational and type testing.
pExpressionP12 :: P Expression
pExpressionP12 = pExpressionP13 `chainl1` pOperators
    where
        pOperators = choice 
            [ BinaryOperator BinaryLessThan         <$ pToken TOpLessThan
            , BinaryOperator BinaryGreaterThan      <$ pToken TOpGreaterThan
            , BinaryOperator BinaryLessThanEqual    <$ pToken TOpLessThanEqual
            , BinaryOperator BinaryGreaterThanEqual <$ pToken TOpGreaterThanEqual
            , BinaryOperator BinaryIs               <$ pToken TKWis
            , BinaryOperator BinaryAs               <$ pToken TKWas ]

-- Shift operators: shift.
pExpressionP13 :: P Expression
pExpressionP13 = pExpressionP14 `chainl1` pOperators
    where
        pOperators = choice
            [ BinaryOperator BinaryShiftLeft  <$ pToken TOpLeftShift
            , BinaryOperator BinaryShiftRight <$ pToken TOpRightShift ]

-- Arithmetic operators: additive.
pExpressionP14 :: P Expression
pExpressionP14 = pExpressionP15 `chainl1` pOperators
    where
        pOperators = choice 
            [ BinaryOperator BinaryPlus  <$ pToken TOpPlus
            , BinaryOperator BinaryMinus <$ pToken TOpMinus ]

-- Arithmetic operators: multiplicative.
pExpressionP15 :: P Expression
pExpressionP15 = pExpressionP16 `chainl1`  pOperators
    where        
        pOperators = choice
            [ BinaryOperator BinaryMultiply <$ pToken TOpMultiply
            , BinaryOperator BinaryDivide   <$ pToken TOpDivide
            , BinaryOperator BinaryModulo   <$ pToken TOpModulo ]

-- Unary operators: unary.
pExpressionP16 :: P Expression
pExpressionP16 = choice
    [ UnaryAwait <$ pToken TOpAwait <*> pExpressionP16
    , pExpressionP17 `chainlUnary1` pOperators
    ]
    where
        pOperators = choice 
            [ UnaryPlus         <$  pToken TOpPlus      
            , UnaryMinus        <$  pToken TOpMinus     
            , UnaryNot          <$  pToken TOpNot       
            , UnaryBitwiseNot   <$  pToken TOpBitwiseNot
            , UnaryPreIncrement <$  pToken TOpPlusPlus  
            , UnaryPreDecrement <$  pToken TOpMinusMinus
            , try (UnaryCast    <$> betweenParens pType) ]

-- Primary expressions: primary, literals and parenthesized.
pExpressionP17 :: P Expression
pExpressionP17 = pTerm `chainPostfix` pPostfixOperator
    
pPostfixOperator :: P (Expression -> Expression)
pPostfixOperator = choice 
    [ UnaryPostIncrement <$  pToken TOpPlusPlus
    , UnaryPostDecrement <$  pToken TOpMinusMinus
    , flip ElementAccess <$> betweenSquare (sepBy1 pExpression pComma)
    , try pPrimaryMemberAccess
    , flip Invocation    <$> pArguments ]
    where
        pPrimaryMemberAccess = do 
            pPeriod
            name          <- pIdentifier
            typeArguments <- option [] pTypeArguments
            return (\ e -> MemberAccess (PrimaryMemberAccess e name typeArguments))

pTerm :: P Expression
pTerm = choice
    [      This         <$  pToken TKWthis
    ,      Base         <$  pToken TKWbase
    ,      Typeof       <$  pToken TKWtypeof            <*> betweenParens pTypeOfExpression
    ,      Sizeof       <$  pToken TKWsizeof            <*> betweenParens pType
    ,      Checked      <$  pToken TKWchecked           <*> betweenParens pExpression
    ,      Unchecked    <$  pToken TKWunchecked         <*> betweenParens pExpression
    ,      Nameof       <$  pIdentifierKeyword "nameof" <*> betweenParens pNameofEntity
    , try (Default      <$  pToken TKWdefault           <*> betweenParens pType)
    ,      MemberAccess <$> try pMemberAccess
    ,      SimpleName   <$> pIdentifier                 <*> option [] pTypeArguments
    ,      pNewExpression
    ,      pParenthesizedExpression
    ,      pLiteralExpression ]

pNameofEntity :: P NameofEntity
pNameofEntity = choice
    [ NameofThis           <$  pToken TKWthis <* pPeriod <*> pIdentifier
    , NameofBase           <$  pToken TKWbase <* pPeriod <*> pIdentifier
    , NameofPredefinedType <$> pSimpleType    <* pPeriod <*> pIdentifier
    , NameofIdentifier     <$> pIdentifier ]

pTypeOfExpression :: P TypeOfExpression
pTypeOfExpression = TypeofType <$> pTypeWithVoid <?> "a type"

pMemberAccess :: P MemberAccess
pMemberAccess = choice 
    [ PredefinedMemberAccess <$> pSimpleType 
                             <*  pPeriod 
                             <*> pIdentifier 
                             <*> option [] pTypeArguments

    , QualifiedMemberAccess  <$> pIdentifier 
                             <*  pToken TDoubleColon 
                             <*> pIdentifier 
                             <*  pPeriod 
                             <*> pIdentifier ] 

pNewExpression :: P Expression
pNewExpression = pToken TKWnew *> choice
    [ try $ ObjectCreationExpression      <$> pType 
                                          <*> pArguments
                                          <*> optionMaybe pObjectCreationInitializer

    , try $ ObjectCreationTypeInitializer <$> pType <*> pObjectCreationInitializer

    , try $ ArrayCreationExpression       <$> pNonArrayType 
                                          <*> betweenSquare (sepBy1 pExpression pComma) 
                                          <*> many pRankSpecifier 
                                          <*> optionMaybe pArrayCreationInitializer
 
    , try $ ArrayCreationTypeInitializer <$> pType <*> pArrayCreationInitializer

    , ArrayCreationRankInitializer <$> pRankSpecifier <*> pArrayCreationInitializer ]

pObjectCreationInitializer :: P ObjectCreationInitializer
pObjectCreationInitializer = choice 
    [ try $ ObjectInitializer     <$> betweenCurly (sepBy pMemberInitializer pComma)
    , try $ CollectionInitializer <$> pArrayCreationInitializer ]

pMemberInitializer :: P MemberInitializer
pMemberInitializer = 
    MemberInitializer <$> pInitializerTarget <* pEqualSign <*> pInitializerValue

pArrayCreationInitializer :: P ArrayCreationInitializer
pArrayCreationInitializer = betweenCurly $ choice
    [ ArrayCreationInitializerExpression   <$> sepBy1 pExpression pComma
    , ArrayCreationInitializerInitializers <$> sepBy1 pArrayCreationInitializer pComma ]

pInitializerTarget :: P InitializerTarget
pInitializerTarget = choice 
    [ InitializerTargetIdentifier <$> pIdentifier
    , InitializerTargetList       <$> betweenSquare (sepBy pArgument pComma) ]

pInitializerValue :: P InitializerValue
pInitializerValue = choice
    [ InitializerValueExpression  <$> pExpression
    , InitializerValueInitializer <$> pObjectCreationInitializer ]

pLiteralExpression :: P Expression
pLiteralExpression = choice
    [ Literal         <$> pBooleanLiteral
    , Literal         <$> pIntLiteral
    , Literal         <$> pRealLiteral
    , Literal         <$> pCharLiteral
    , Literal         <$> pStringLiteral
    , Literal NullLit <$  pToken TKWnull ]

pBooleanLiteral :: P Literal
pBooleanLiteral = 
    (BooleanLit True  <$ pToken TKWtrue   <|>
     BooleanLit False <$ pToken TKWfalse) <?> "a boolean literal"

pIntLiteral :: P Literal
pIntLiteral = do
    sourceName <- getSourceName
    token show (posFromToken sourceName) matchToken <?> "an integral literal"
    where
        matchToken (Positioned _ (TIntLiteral x))   = Just (IntLit x)
        matchToken (Positioned _ (TUIntLiteral x))  = Just (UIntLit x)
        matchToken (Positioned _ (TLongLiteral x))  = Just (LongLit x)
        matchToken (Positioned _ (TULongLiteral x)) = Just (ULongLit x)
        matchToken _                                = Nothing

pRealLiteral :: P Literal
pRealLiteral = do
    sourceName <- getSourceName
    token show (posFromToken sourceName) matchToken <?> "a floating point literal"
    where
        matchToken (Positioned _ (TFloatLiteral x))   = Just (FloatLit x)
        matchToken (Positioned _ (TDoubleLiteral x))  = Just (DoubleLit x)
        matchToken (Positioned _ (TDecimalLiteral x)) = Just (DecimalLit x)
        matchToken _                                  = Nothing

pCharLiteral :: P Literal
pCharLiteral = do
    sourceName <- getSourceName
    token show (posFromToken sourceName) matchToken <?> "a char literal"
    where
        matchToken (Positioned _ (TCharLiteral x)) = Just (CharLit x)
        matchToken _                               = Nothing

pStringLiteral :: P Literal
pStringLiteral = do
    sourceName <- getSourceName
    token show (posFromToken sourceName) matchToken <?> "a string literal"
    where
        matchToken (Positioned _ tok)
            = case tok of
                TStringLiteral x             -> Just (StringLit x)
              --  TInterpolatedStringLiteral x -> Just (InterpolatedStringLit x)
                TVerbatimStringLiteral x     -> Just (VerbatimStringLit x)
                _                            -> Nothing

pParenthesizedExpression :: P Expression
pParenthesizedExpression = Parenthesized <$> betweenParens pExpression

pArguments :: P [Argument]
pArguments = betweenParens (sepBy pArgument pComma)

pArgument :: P Argument
pArgument = do
    name <- optionMaybe (try (pIdentifier <* pColon))
    choice [argument name, refArgument name, outArgument name]
    where
        argument    name' = Argument    name' <$> pExpression
        refArgument name' = RefArgument name' <$ pToken TKWref <*> pExpression
        outArgument name' = OutArgument name' <$ pToken TKWout <*> pExpression
