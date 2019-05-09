module Language.CSharp.Parser.Declaration where

import Text.Parsec                       hiding (Empty)
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility
import Language.CSharp.Parser.Statement
import Language.CSharp.Parser.Expression
import Language.CSharp.Parser.Type
import Language.CSharp.Parser.Attribute

pDeclaration :: P Declaration
pDeclaration = try pNamespace <|> TypeDeclaration <$> pTypeDeclaration

--------------------------------------------------------------------------------
-- Namespace and type declarations.
--------------------------------------------------------------------------------

pNamespace :: P Declaration
pNamespace = do
    attributes <- pGlobalAttributeSections
    pToken TKWnamespace
    name         <- pName
    declarations <- between pOCurly pCCurly (many pDeclaration)
    return $ NamespaceDeclaration attributes name declarations

pTypeDeclaration :: P TypeDeclaration
pTypeDeclaration = choice 
    [ try pClass, try pStruct  , try pInterface
    , try pEnum , try pDelegate ]

--------------------------------------------------------------------------------
-- Enum declarations.
--------------------------------------------------------------------------------

pEnum :: P TypeDeclaration
pEnum = do
    attributes <- pAttributeSections
    modifiers  <- many pEnumModifier
    pToken TKWenum
    name     <- pIdentifier
    inherits <- optionMaybe (pColon *> pIntegralType)
    body     <- betweenCurly pEnumBody
    optional pSemi
    return $ EnumTypeDeclaration attributes modifiers name inherits body

pEnumBody :: P EnumBody
pEnumBody = EnumBody <$> sepBy pEnumMemberDeclaration pComma

pEnumMemberDeclaration :: P EnumMemberDeclaration
pEnumMemberDeclaration =
    EnumMemberDeclaration <$> pIdentifier <*> optionMaybe (pEqualSign *> pExpression)

pEnumModifier :: P Modifier
pEnumModifier = choice [ New       <$ pToken TKWnew
                       , Public    <$ pToken TKWpublic    
                       , Protected <$ pToken TKWprotected 
                       , Internal  <$ pToken TKWinternal  
                       , Private   <$ pToken TKWprivate   ]

--------------------------------------------------------------------------------
-- Struct declarations.
--------------------------------------------------------------------------------

pStruct :: P TypeDeclaration
pStruct = do
    attributes <- pAttributeSections
    modifiers  <- (++) <$> many pStructModifier <*> pOptionPartial
    pToken TKWstruct
    name        <- pIdentifier
    typeParams  <- option [] pTypeParameters
    inherits    <- option [] (pColon *> sepBy1 pTypeName pComma)
    constraints <- many pTypeParameterConstraintClause
    body        <- between pOCurly pCCurly pStructBody
    optional pSemi
    return $ StructTypeDeclaration attributes modifiers name typeParams inherits constraints body

pStructBody :: P StructBody
pStructBody = StructBody <$> many pMemberDeclaration

pStructModifier :: P Modifier
pStructModifier = choice [ New       <$ pToken TKWnew      
                         , Public    <$ pToken TKWpublic   
                         , Protected <$ pToken TKWprotected
                         , Internal  <$ pToken TKWinternal 
                         , Private   <$ pToken TKWprivate   ]

--------------------------------------------------------------------------------
-- Delegate declarations.
--------------------------------------------------------------------------------

pDelegate :: P TypeDeclaration
pDelegate = do
    attributes <- pAttributeSections
    modifiers  <- many pDelegateModifier
    pToken TKWdelegate
    ty          <- pTypeWithVoid
    name        <- pIdentifier
    typeParams  <- pVariantTypeParameters
    params      <- betweenParens pFormalParams
    constraints <- many pTypeParameterConstraintClause
    pSemi
    return $ DelegateTypeDeclaration attributes modifiers ty name typeParams params constraints

pDelegateModifier :: P Modifier
pDelegateModifier = choice
    [ New       <$ pToken TKWnew
    , Public    <$ pToken TKWpublic
    , Protected <$ pToken TKWprotected
    , Internal  <$ pToken TKWinternal
    , Private   <$ pToken TKWprivate ]

--------------------------------------------------------------------------------
-- interface declarations.
--------------------------------------------------------------------------------

pInterface :: P TypeDeclaration
pInterface = do
    attributes <- pAttributeSections
    modifiers  <- (++) <$> many pInterfaceModifier <*> pOptionPartial
    pToken TKWinterface
    name        <- pIdentifier
    typeParams  <- option [] pVariantTypeParameters
    inherits    <- option [] (pColon *> sepBy1 pTypeName pComma)
    constraints <- many pTypeParameterConstraintClause
    body        <- pInterfaceBody
    optional pSemi
    return $ InterfaceTypeDeclaration attributes modifiers name typeParams inherits constraints body

pInterfaceBody :: P InterfaceBody
pInterfaceBody = InterfaceBody <$> betweenCurly (many pInterfaceMemberDeclaration)

pInterfaceMemberDeclaration :: P InterfaceMemberDeclaration
pInterfaceMemberDeclaration = choice
    [ try pInterfaceMethodMemberDeclaration
    , try pInterfacePropertyMemberDeclaration
    , try pInterfaceEventMemberDeclaration
    , pInterfaceIndexerMemberDeclaration ]

pInterfaceMethodMemberDeclaration :: P InterfaceMemberDeclaration
pInterfaceMethodMemberDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- option [] ([New] <$ pToken TKWnew)
    ty         <- pTypeWithVoid
    name       <- pIdentifier
    typeParams <- option [] pTypeParameters
    params     <- betweenParens pFormalParams
    constraints <- many pTypeParameterConstraintClause
    pSemi
    return $ InterfaceMethodMemberDeclaration attributes modifiers ty name typeParams params constraints

pInterfacePropertyMemberDeclaration :: P InterfaceMemberDeclaration
pInterfacePropertyMemberDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- option [] ([New] <$ pToken TKWnew)
    ty         <- pType
    name       <- pIdentifier
    (accessor1, accessor2) <- betweenCurly (eitherOrBoth (try pGetInterfaceAccessor) (try pSetInterfaceAccessor))
    return $ InterfacePropertyMemberDeclaration attributes modifiers ty name accessor1 accessor2
    
pInterfaceEventMemberDeclaration :: P InterfaceMemberDeclaration
pInterfaceEventMemberDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- option [] ([New] <$ pToken TKWnew)
    pToken TKWevent
    ty   <- pType
    name <- pIdentifier
    pSemi
    return $ InterfaceEventMemberDeclaration attributes modifiers ty name

pInterfaceIndexerMemberDeclaration :: P InterfaceMemberDeclaration
pInterfaceIndexerMemberDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- option [] ([New] <$ pToken TKWnew)
    ty         <- pType
    pToken TKWthis
    params <- betweenSquare pFormalParams   
    (accessor1, accessor2) <- betweenCurly (eitherOrBoth (try pGetInterfaceAccessor) (try pSetInterfaceAccessor))
    return $ InterfaceIndexerMemberDeclaration attributes modifiers ty params accessor1 accessor2

pGetInterfaceAccessor :: P InterfaceAccessor
pGetInterfaceAccessor = 
    GetInterfaceAccessor <$> pAttributeSections <* pIdentifierKeyword "get" <* pSemi

pSetInterfaceAccessor :: P InterfaceAccessor
pSetInterfaceAccessor = 
    SetInterfaceAccessor <$> pAttributeSections <* pIdentifierKeyword "set" <* pSemi

pInterfaceModifier :: P Modifier
pInterfaceModifier = choice
    [ New       <$ pToken TKWnew      , Public   <$ pToken TKWpublic
    , Protected <$ pToken TKWprotected, Internal <$ pToken TKWinternal
    , Private   <$ pToken TKWprivate  ]

--------------------------------------------------------------------------------
-- Class declarations.
--------------------------------------------------------------------------------

pClass :: P TypeDeclaration
pClass = do
    attributes <- pAttributeSections
    modifiers  <- (++) <$> many pClassModifier <*> pOptionPartial
    pToken TKWclass
    name        <- pIdentifier
    typeParams  <- option [] pTypeParameters
    inherits    <- option [] (pColon *> sepBy1 pTypeName pComma)
    constraints <- many pTypeParameterConstraintClause
    body        <- between pOCurly pCCurly pClassBody
    optional pSemi
    return $ ClassTypeDeclaration attributes modifiers name typeParams inherits constraints body

pClassModifier :: P Modifier
pClassModifier = choice
    [ New       <$ pToken TKWnew      , Public   <$ pToken TKWpublic   
    , Protected <$ pToken TKWprotected, Internal <$ pToken TKWinternal 
    , Private   <$ pToken TKWprivate  , Abstract <$ pToken TKWabstract 
    , Sealed    <$ pToken TKWsealed   , Static   <$ pToken TKWstatic   ]

pClassBody :: P ClassBody
pClassBody = ClassBody <$> many pMemberDeclaration

pMemberDeclaration :: P MemberDeclaration
pMemberDeclaration = choice [ try pFieldDeclaration
                            , try pMethodDeclaration
                            , try pConstructorDeclaration
                            , try pDestructorDeclaration
                            , try pPropertyDeclaration
                            , try pIndexerDeclaration
                            , try pOperatorDeclaration
                            , TypeMemberDeclaration <$> pTypeDeclaration ]

--------------------------------------------------------------------------------
-- Field declarations.
--------------------------------------------------------------------------------

pFieldDeclaration :: P MemberDeclaration
pFieldDeclaration = do
    attributes  <- pAttributeSections
    modifiers   <- (++) <$> many pFieldModifier <*> option [] ([Const] <$ pToken TKWconst)
    ty          <- pType
    declarators <- sepBy1 pVariableDeclarator pComma
    pSemi
    return $ FieldMemberDeclaration attributes modifiers ty declarators

pFieldModifier :: P Modifier
pFieldModifier = choice [ New       <$ pToken TKWnew
                        , Public    <$ pToken TKWpublic   
                        , Protected <$ pToken TKWprotected
                        , Internal  <$ pToken TKWinternal 
                        , Private   <$ pToken TKWprivate 
                        , Static    <$ pToken TKWstatic  
                        , Readonly  <$ pToken TKWreadonly  
                        , Volatile  <$ pToken TKWvolatile ]

--------------------------------------------------------------------------------
-- Property declarations.
--------------------------------------------------------------------------------

pPropertyDeclaration :: P MemberDeclaration
pPropertyDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- many pPropertyModifier
    ty         <- pType
    name       <- pName
    body       <- pPropertyLambda <|> pPropertyBody
    return $ PropertyMemberDeclaration attributes modifiers ty name body

pPropertyLambda :: P PropertyBody
pPropertyLambda = PropertyLambda <$ pLambda <*> pExpression <* pSemi

pPropertyBody :: P PropertyBody
pPropertyBody = do
    (accessor1, accessor2) <- betweenCurly (eitherOrBoth pPropertyGetAccessor pPropertySetAccessor)
    initializer            <- optionMaybe (pEqualSign *> pVariableInitializer <* pSemi)
    return $ PropertyBody accessor1 accessor2 initializer

pPropertyGetAccessor :: P AccessorDeclaration
pPropertyGetAccessor = do
    attributes <- pAttributeSections
    modifiers <- pPropertyAccessorModifiers
    pIdentifierKeyword "get"
    body <- Nothing <$ pSemi <|> Just <$> betweenCurly pStatements
    return $ GetAccessorDeclaration attributes modifiers body

pPropertySetAccessor :: P AccessorDeclaration
pPropertySetAccessor = do
    attributes <- pAttributeSections
    modifiers <- pPropertyAccessorModifiers
    pIdentifierKeyword "set"
    body <- Nothing <$ pSemi <|> Just <$> betweenCurly pStatements
    return $ SetAccessorDeclaration attributes modifiers body

pPropertyAccessorModifiers :: P [Modifier]
pPropertyAccessorModifiers = 
    choice [ try ([Protected, Internal]  <$ pToken TKWinternal  <* pToken TKWprotected)
           , try ([Internal , Protected] <$ pToken TKWprotected <* pToken TKWinternal)
           , [Protected] <$ pToken TKWprotected
           , [Internal]  <$ pToken TKWinternal 
           , [Private]   <$ pToken TKWprivate  
           , return [] ]

pPropertyModifier :: P Modifier
pPropertyModifier = 
    choice [ New       <$ pToken TKWnew      , Public   <$ pToken TKWpublic   
           , Protected <$ pToken TKWprotected, Internal <$ pToken TKWinternal   
           , Private   <$ pToken TKWprivate  , Static   <$ pToken TKWstatic   
           , Virtual   <$ pToken TKWvirtual  , Sealed   <$ pToken TKWsealed   
           , Override  <$ pToken TKWoverride , Abstract <$ pToken TKWabstract   
           , Extern    <$ pToken TKWextern   ]

--------------------------------------------------------------------------------
-- Constructor and destructor declarations.
--------------------------------------------------------------------------------

pConstructorDeclaration :: P MemberDeclaration
pConstructorDeclaration = do
    attributes  <- pAttributeSections
    modifiers   <- many pConstructorModifier
    name        <- pIdentifier
    params      <- betweenParens pFormalParams
    initializer <- optionMaybe (pColon *> pConstructorInitializer)
    body        <- ConstructorStatementBody <$> betweenCurly pStatements
    return $ ConstructorMemberDeclaration attributes modifiers name params initializer body

pConstructorInitializer :: P ConstructorInitializer
pConstructorInitializer = baseCall <|> thisCall
    where
        baseCall   = ConstructorBaseCall <$ pToken TKWbase <*> pArguments
        thisCall   = ConstructorThisCall <$ pToken TKWthis <*> pArguments

pConstructorModifier :: P Modifier
pConstructorModifier = choice [ Public    <$ pToken TKWpublic   
                              , Protected <$ pToken TKWprotected
                              , Internal  <$ pToken TKWinternal 
                              , Private   <$ pToken TKWprivate  
                              , Extern    <$ pToken TKWextern   ]

pDestructorDeclaration :: P MemberDeclaration
pDestructorDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- option [] ([Extern] <$ pToken TKWextern)
    pToken TOpBitwiseNot
    name <- pIdentifier
    pOParens; pCParens
    body <- DestructorStatementBody <$> betweenCurly pStatements
    return $ DestructorMemberDeclaration attributes modifiers name body

--------------------------------------------------------------------------------
-- Operator declarations.
--------------------------------------------------------------------------------

pOperatorDeclaration :: P MemberDeclaration
pOperatorDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- many1 pOperatorModifier
    declarator <- pOperatorDeclarator
    body       <- pOperatorBody
    return $ OperatorMemberDeclaration attributes modifiers declarator body

pOperatorDeclarator :: P OperatorDeclarator
pOperatorDeclarator = choice
    [ try $ UnaryOperatorDeclarator <$> pType
                                    <*  pToken TKWoperator
                                    <*> pOverloadableUnaryOperator
                                    <* pOParens
                                    <*> pType <*> pIdentifier
                                    <* pCParens

    , try $ BinaryOperatorDeclarator <$> pType
                                     <*  pToken TKWoperator
                                     <*> pOverloadableBinaryOperator
                                     <* pOParens
                                     <*> pType <*> pIdentifier
                                     <* pComma
                                     <*> pType <*> pIdentifier
                                     <* pCParens

    , ImplicitConversionOperatorDeclarator <$ pToken TKWimplicit
                                           <* pToken TKWoperator
                                           <*> pType
                                           <* pOParens
                                           <*> pType <*> pIdentifier
                                           <* pCParens

    , ExplicitConversionOperatorDeclarator <$ pToken TKWexplicit
                                           <* pToken TKWoperator
                                           <*> pType
                                           <* pOParens
                                           <*> pType <*> pIdentifier
                                           <* pCParens ]

pOverloadableUnaryOperator :: P OverloadableUnaryOperator
pOverloadableUnaryOperator = choice
    [ OverloadableUnaryPlus       <$ pToken TOpPlus 
    , OverloadableUnaryMinus      <$ pToken TOpMinus 
    , OverloadableUnaryNot        <$ pToken TOpNot   
    , OverloadableUnaryBitwiseNot <$ pToken TOpBitwiseNot   
    , OverloadableUnaryPlusPlus   <$ pToken TOpPlusPlus 
    , OverloadableUnaryMinusMinus <$ pToken TOpMinusMinus
    , OverloadableUnaryTrue       <$ pToken TKWtrue
    , OverloadableUnaryFalse      <$ pToken TKWfalse ]

pOverloadableBinaryOperator :: P BinaryOperator
pOverloadableBinaryOperator = choice 
    [ BinaryPlus             <$ pToken TOpPlus
    , BinaryMinus            <$ pToken TOpMinus
    , BinaryMultiply         <$ pToken TOpMultiply
    , BinaryDivide           <$ pToken TOpDivide
    , BinaryModulo           <$ pToken TOpModulo
    , BinaryBitwiseAnd       <$ pToken TOpBitwiseAnd
    , BinaryBitwiseOr        <$ pToken TOpBitwiseOr
    , BinaryBitwiseXor       <$ pToken TOpBitwiseXor
    , BinaryShiftLeft        <$ pToken TOpLeftShift
    , BinaryShiftRight       <$ pToken TOpRightShift
    , BinaryEquals           <$ pToken TOpEqual
    , BinaryNotEquals        <$ pToken TOpNotEqual
    , BinaryGreaterThan      <$ pToken TOpGreaterThan
    , BinaryGreaterThanEqual <$ pToken TOpGreaterThanEqual
    , BinaryLessThan         <$ pToken TOpLessThan
    , BinaryLessThanEqual    <$ pToken TOpLessThanEqual ]


pOperatorBody :: P OperatorBody
pOperatorBody = choice
    [ OperatorStatementBody  <$> betweenCurly pStatements
    , OperatorExpressionBody <$  pLambda <*> pExpression <* pSemi
    , OperatorNoBody         <$  pSemi ]

pOperatorModifier :: P Modifier
pOperatorModifier = choice 
    [ Public <$ pToken TKWpublic 
    , Static <$ pToken TKWstatic
    , Extern <$ pToken TKWextern ]

--------------------------------------------------------------------------------
-- Event declarations.
--------------------------------------------------------------------------------

pEventDeclaration :: P MemberDeclaration
pEventDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- many pEventModifier
    pToken TKWevent
    ty <- pType
    choice [ EventVariableMemberDeclaration attributes modifiers ty 
             <$> sepBy1 pVariableDeclarator pComma
             <*  pSemi
            
           , try $ EventAccessorMemberDeclaration attributes modifiers ty
                <$> pName
                <* pOCurly <*> pAddEventAccessor <*> pRemoveEventAccessor
                <* pCCurly
                
           , try $ EventAccessorMemberDeclaration attributes modifiers ty
                <$> pName
                <* pOCurly <*> pRemoveEventAccessor <*> pAddEventAccessor
                <* pCCurly ]

pAddEventAccessor :: P EventAccessor
pAddEventAccessor = do
    attributes <- pAttributeSections
    pIdentifierKeyword "add"
    body <- betweenCurly pStatements
    return $ AddEventAccessor attributes body
    
pRemoveEventAccessor :: P EventAccessor
pRemoveEventAccessor = do
    attributes <- pAttributeSections
    pIdentifierKeyword "remove"
    body <- betweenCurly pStatements
    return $ RemoveEventAccessor attributes body

pEventModifier :: P Modifier
pEventModifier = choice
    [ New       <$ pToken TKWnew      , Public   <$ pToken TKWpublic
    , Protected <$ pToken TKWprotected, Internal <$ pToken TKWinternal
    , Private   <$ pToken TKWprivate  , Static   <$ pToken TKWstatic
    , Virtual   <$ pToken TKWvirtual  , Sealed   <$ pToken TKWsealed
    , Override  <$ pToken TKWoverride , Abstract <$ pToken TKWabstract
    , Extern    <$ pToken TKWextern   ]

--------------------------------------------------------------------------------
-- Indexer declarations.
--------------------------------------------------------------------------------

pIndexerDeclaration :: P MemberDeclaration
pIndexerDeclaration = do
    attributes <- pAttributeSections
    modifiers  <- many pIndexerModifier
    declarator <- pIndexerDeclarator
    body       <- pIndexerBody
    return $ IndexerMemberDeclaration attributes modifiers declarator body

pIndexerDeclarator :: P IndexerDeclarator
pIndexerDeclarator = choice
    [ try (IndexerDeclaratorThis <$> pType 
                                 <*  pToken TKWthis 
                                 <*> betweenSquare pFormalParams)

    , IndexerDeclaratorInterface <$> pType 
                                 <*> pInterfaceType 
                                 <*  pPeriod 
                                 <*  pToken TKWthis 
                                 <*> betweenSquare pFormalParams
    ]

pIndexerBody :: P IndexerBody
pIndexerBody = choice
    [ uncurry IndexerAccessor <$> betweenCurly (eitherOrBoth pPropertyGetAccessor pPropertySetAccessor)
    , IndexerLambda           <$ pLambda <*> pExpression <* pSemi ]

pIndexerModifier :: P Modifier
pIndexerModifier = choice 
    [ New       <$ pToken TKWnew      , Public   <$ pToken TKWpublic   
    , Protected <$ pToken TKWprotected, Internal <$ pToken TKWinternal 
    , Private   <$ pToken TKWprivate  , Virtual  <$ pToken TKWprivate  
    , Sealed    <$ pToken TKWprivate  , Override <$ pToken TKWprivate  
    , Abstract  <$ pToken TKWprivate  , Extern   <$ pToken TKWextern ]

--------------------------------------------------------------------------------
-- Method declarations.
--------------------------------------------------------------------------------

pMethodDeclaration :: P MemberDeclaration
pMethodDeclaration = MethodMemberDeclaration
    <$> pAttributeSections
    <*> ((++) <$> many pMethodModifier <*> pOptionPartial)
    <*> pTypeWithVoid
    <*> pName
    <*> option [] (betweenDiamond (sepBy1 pTypeParameter pComma))
    <*> betweenParens pFormalParams
    <*> many pTypeParameterConstraintClause
    <*> pMethodBody
    
pMethodBody :: P MethodBody
pMethodBody = 
    MethodNoBody         <$  pSemi                                <|>
    MethodExpressionBody <$  pLambda <*> pExpression <* pSemi     <|>
    MethodStatementBody  <$> between pOCurly pCCurly pStatements

pMethodModifier :: P Modifier
pMethodModifier = choice
    [ New       <$ pToken TKWnew      , Public    <$ pToken TKWpublic    
    , Protected <$ pToken TKWprotected, Internal  <$ pToken TKWinternal  
    , Private   <$ pToken TKWprivate  , Static    <$ pToken TKWstatic    
    , Virtual   <$ pToken TKWvirtual  , Sealed    <$ pToken TKWsealed    
    , Override  <$ pToken TKWoverride , Abstract  <$ pToken TKWabstract  
    , Extern    <$ pToken TKWextern   , Async     <$ pIdentifierKeyword "async" ]  

--------------------------------------------------------------------------------
-- Formal parameters.
--------------------------------------------------------------------------------

pFormalParams :: P FormalParams
pFormalParams = do
    params     <- sepBy pFormalParam pComma
    paramArray <- optionMaybe pParamArray
    return $ FormalParams params paramArray

pFormalParam :: P FormalParam
pFormalParam = do
    modifier   <- optionMaybe pParameterModifier
    ty         <- pType
    name       <- pIdentifier
    defaultArg <- optionMaybe (pEqualSign *> pExpression)
    return $ FormalParam modifier ty name defaultArg

pParamArray :: P ParamArray
pParamArray = ParamArray <$ pToken TKWparams <*> pArrayType <*> pIdentifier

pParameterModifier :: P ParameterModifier
pParameterModifier =
    RefParam  <$ pToken TKWref  <|>
    OutParam  <$ pToken TKWout  <|>
    ThisParam <$ pToken TKWthis

--------------------------------------------------------------------------------
-- Utility.
--------------------------------------------------------------------------------

pOptionPartial :: P [Modifier]
pOptionPartial = option [] ([Partial] <$ pIdentifierKeyword "partial")