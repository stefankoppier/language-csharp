module Language.CSharp.Parser.Attribute where

import Text.Parsec                       hiding (Empty)
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility
import Language.CSharp.Parser.Expression
import Language.CSharp.Parser.Type

pGlobalAttributeSections :: P [GlobalAttributeSection]
pGlobalAttributeSections = many pGlobalAttributeSection

pGlobalAttributeSection :: P GlobalAttributeSection
pGlobalAttributeSection = betweenSquare $ GlobalAttributeSection
    <$> optionMaybe (pGlobalAttributeTarget <* pColon)
    <*> sepBy1 pAttribute pComma

pGlobalAttributeTarget :: P GlobalAttributeTarget
pGlobalAttributeTarget = choice
    [ AttributeTargetAssembly <$ pIdentifierKeyword "assembly"
    , AttributeTargetModule   <$ pIdentifierKeyword "module" ]
    
pAttributeSections :: P [AttributeSection]
pAttributeSections = many pAttributeSection

pAttributeSection :: P AttributeSection
pAttributeSection = betweenSquare $ AttributeSection
    <$> optionMaybe (pAttributeTarget <* pColon)
    <*> sepBy1 pAttribute pComma

pAttributeTarget :: P AttributeTarget
pAttributeTarget = choice
    [ AttributeTargetField    <$ pIdentifierKeyword "field"
    , AttributeTargetEvent    <$ pToken TKWevent
    , AttributeTargetMethod   <$ pIdentifierKeyword "method"
    , AttributeTargetParam    <$ pIdentifierKeyword "param"
    , AttributeTargetProperty <$ pIdentifierKeyword "property"
    , AttributeTargetReturn   <$ pToken TKWreturn
    , AttributeTargetType     <$ pIdentifierKeyword "type" ]
    
pAttribute :: P Attribute
pAttribute = Attribute <$> pTypeName <*> betweenParens pAttributeArguments

pAttributeArguments :: P [AttributeArgument]
pAttributeArguments = choice
    [ try ((++) <$> sepEndBy1 pAttributeArgumentExpression pComma
                <*> sepBy1 pAttributeArgumentNamed pComma)

    , try (sepBy1 pAttributeArgumentNamed pComma)

    , sepBy pAttributeArgumentExpression pComma ]
    
pAttributeArgumentExpression :: P AttributeArgument
pAttributeArgumentExpression = AttributeArgumentExpression <$> pExpression

pAttributeArgumentNamed :: P AttributeArgument
pAttributeArgumentNamed =  
    AttributeArgumentNamed <$> pIdentifier <* pEqualSign <*> pExpression