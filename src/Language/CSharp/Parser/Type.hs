module Language.CSharp.Parser.Type where

import Text.Parsec
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility

pType :: P Type
pType = pTypeTerm `chainPostfix` pArrayOrNullableType <?> "a type or dynamic"

pNonArrayType :: P Type
pNonArrayType = pTypeTerm `chainPostfix` (TypeNullable <$ pToken TQuestion)

pClassType :: P Type
pClassType = choice
    [ TypeNamed         <$> pTypeName                  
    , TypeDynamic       <$  pIdentifierKeyword "dynamic"
    , TypeSimple Object <$  pToken TKWobject
    , TypeSimple String <$  pToken TKWstring ]

pInterfaceType :: P Type
pInterfaceType = TypeNamed <$> pTypeName

pArrayOrNullableType :: P (Type -> Type)
pArrayOrNullableType = choice
    [ TypeNullable <$ pToken TQuestion
    , (\ ranks -> TypeArray . flip ArrayType ranks) <$> many1 pRankSpecifier
    ]

pTypeTerm :: P Type
pTypeTerm = choice [ TypeNamed   <$> pTypeName
                   , TypeSimple  <$> pSimpleType
                   , TypeDynamic <$  pIdentifierKeyword "dynamic" ]

pTypeWithVoid :: P (Maybe Type)
pTypeWithVoid = 
    Nothing <$  pToken TKWvoid <|>
    Just    <$> pType

pSimpleType :: P SimpleType
pSimpleType = choice
    [ IntegralType <$> pIntegralType   , FloatingPointType <$> pFloatingPointType
    , Char         <$  pToken TKWchar  , Bool              <$  pToken TKWbool
    , Object       <$  pToken TKWobject, String            <$  pToken TKWstring ]
    
pIntegralType :: P IntegralType
pIntegralType =
    (SByte <$ pToken TKWsbyte <|> Byte   <$ pToken TKWbyte   <|>
     Short <$ pToken TKWshort <|> UShort <$ pToken TKWushort <|>
     Int   <$ pToken TKWint   <|> UInt   <$ pToken TKWuint   <|>
     Long  <$ pToken TKWlong  <|> ULong  <$ pToken TKWulong) <?> "an integral type"

pFloatingPointType :: P FloatingPointType
pFloatingPointType = choice
    [ Float   <$ pToken TKWfloat
    , Double  <$ pToken TKWdouble
    , Decimal <$ pToken TKWdecimal ] <?> "a floating point type"

pArrayType :: P ArrayType
pArrayType = ArrayType <$> pNonArrayType <*> many1 pRankSpecifier <?> "an array type"

pRankSpecifier :: P RankSpecifier
pRankSpecifier = RankSpecifier . length <$> betweenSquare (many pComma)

pTypeName :: P TypeName
pTypeName = choice
    [ TypeName  <$> pName <*> option [] pTypeArguments

    , TypeAlias <$> pIdentifier 
                <*  pToken TDoubleColon 
                <*> pIdentifier  
                <*> option [] pTypeArguments ]

--------------------------------------------------------------------------------
-- Type parameters.
--------------------------------------------------------------------------------

pTypeParameters :: P [TypeParameter]
pTypeParameters = try (betweenDiamond (sepBy1 pTypeParameter pComma))

pTypeParameter :: P TypeParameter
pTypeParameter = TypeParameter <$> pIdentifier

pVariantTypeParameters :: P [VariantTypeParameter]
pVariantTypeParameters 
    = option [] (betweenDiamond (sepBy1 pVariantTypeParameter pComma))

pVariantTypeParameter :: P VariantTypeParameter
pVariantTypeParameter 
    = VariantTypeParameter <$> optionMaybe pVariance <*> pIdentifier
    where
        pVariance = VarianceIn <$ pToken TKWin <|> VarianceOut <$ pToken TKWout

pTypeParameterConstraintClause :: P TypeParameterConstraintClause
pTypeParameterConstraintClause = do
    pIdentifierKeyword "where"
    typeParam <- pTypeParameter
    pColon
    constraints <- pTypeParameterConstraints
    return $ TypeParameterConstraintClause typeParam constraints

pTypeParameterConstraints :: P [TypeParameterConstraint]
pTypeParameterConstraints = choice
    [ try ((\ a b c -> a ++ b ++ c) <$> pPrimaryConstraint <* pComma <*> pSecondaryConstraints <* pComma <*> pConstructorConstraint)
    , try ((++) <$> pPrimaryConstraint <* pComma <*> pSecondaryConstraints)
    , try ((++) <$> pPrimaryConstraint <* pComma <*> pConstructorConstraint)
    , try ((++) <$> pSecondaryConstraints <* pComma <*> pConstructorConstraint)
    , try pPrimaryConstraint, try pSecondaryConstraints, try pConstructorConstraint ]
    where
        pPrimaryConstraint     = choice [ (\ t -> [TypeConstraint t]) <$> pClassType
                                        , [ClassConstraint]           <$ pToken TKWclass
                                        , [StructConstraint]          <$ pToken TKWstruct ]
        pSecondaryConstraints  = sepBy1 (TypeConstraint <$> pInterfaceType) pComma
        pConstructorConstraint = [NewConstraint] <$ pToken TKWnew <* pToken TOParens <* pToken TCParens

pTypeArguments :: P [TypeArgument]
pTypeArguments = try (betweenDiamond (sepBy1 pTypeArgument pComma))

pTypeArgument :: P TypeArgument
pTypeArgument = TypeArgument <$> pType <?> "a type argument"