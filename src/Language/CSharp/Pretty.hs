{-|
Module      : Language.CSharp.Pretty
Description : Pretty printing of C#

This module contains the pretty printing of the abstract syntax tree defined
in "Language.CSharp.Syntax". Pretty printing results in a syntactically valid 
program.
-}

{-# LANGUAGE FlexibleInstances #-}

module Language.CSharp.Pretty(
      prettyPrint
    , Pretty(..)
) where

import Text.PrettyPrint
import Data.List
import Language.CSharp.Syntax

-- | Pretty printing type class.
class Pretty a where
    pretty :: a -> Doc

instance Pretty Doc where
    pretty = id

-- | Results in the pretty printed value of `a`.
prettyPrint :: Pretty a => a -> String
prettyPrint = render . pretty

instance Pretty CompilationUnit where
    pretty (CompilationUnit usings declarations)
        = pretty usings $+$ pretty declarations

instance Pretty [Using] where
    pretty = vsep

instance Pretty Using where
    pretty (Using name isStatic)
        = text "using" <+> pIsStatic <+> pretty name <> semi
        where
            pIsStatic = if isStatic then text "static" else empty

instance Pretty [Declaration] where
    pretty = foldr (($+$) . pretty) empty 

instance Pretty Declaration where
    pretty (NamespaceDeclaration attributes name declarations)
        = pretty attributes $+$ text "namespace" <+> pretty name 
        $+$ lbrace $+$ tab (pretty declarations) $+$ rbrace

    pretty (TypeDeclaration declaration)
        = pretty declaration

instance Pretty TypeDeclaration where
    pretty (ClassTypeDeclaration attributes modifiers name typeParameters inherits constraints body)
        = pretty attributes $+$ pretty modifiers <+> text "class" <+> pretty name <> pretty typeParameters
        <+> pInherits $+$ pConstraints $+$ lbrace $+$ tab (pretty body) $+$ rbrace
        where
            pConstraints    = (vsep . map (tab . pretty)) constraints
            pInherits       = case inherits of
                                [] -> empty
                                _  -> colon <+> hcatIntersperse comma inherits
                    
    pretty (StructTypeDeclaration attributes modifiers name typeParameters inherits constraints body)
        = pretty attributes $+$ pretty modifiers <+> text "struct" <+> pretty name <> pretty typeParameters
        <+> pInherits $+$ pConstraints $+$ lbrace $+$ tab (pretty body) $+$ rbrace
        where
            pConstraints    = (vsep . map (tab . pretty)) constraints
            pInherits  = case inherits of
                            [] -> empty
                            _  -> colon <+> hcatIntersperse comma inherits

    pretty (InterfaceTypeDeclaration attributes modifiers name typeParameters inherits constraints body)
        = pretty attributes $+$ pretty modifiers <+> text "interface" <+> pretty name <> pretty typeParameters
        <+> pInherits $+$ pConstraints $+$ lbrace $+$ tab (pretty body) $+$ rbrace
        where
            pConstraints = (vsep . map (tab . pretty)) constraints
            pInherits    = case inherits of
                                [] -> empty
                                _  -> colon <+> hcatIntersperse comma inherits

    pretty (EnumTypeDeclaration attributes modifiers name inherits body)
        = pretty attributes $+$ pretty modifiers <+> text "enum" <+> pretty name <+> pInherits
        $+$ lbrace $+$ tab (pretty body) $+$ rbrace
        where
            pInherits = maybe empty (\ i -> colon <+> pretty i) inherits

    pretty (DelegateTypeDeclaration attributes modifiers ty name typeParameters params constraints)
        = pretty attributes $+$ pretty modifiers <+> text "delegate" <+> pretty ty <+> pretty name 
        <+> pretty typeParameters <> parens (pretty params) $+$ pConstraints <> semi
        where
            pConstraints = case constraints of
                              [] -> empty
                              _  -> (vsep . map (tab . pretty)) constraints

instance Pretty ClassBody where
    pretty (ClassBody declarations) 
        = vsep declarations
                   
instance Pretty StructBody where
    pretty (StructBody declarations)
        = vsep declarations
                 
instance Pretty EnumBody where
    pretty (EnumBody body) -- TODO: find a cleaner way, e.g. `intersperse`
        = (vsep . (++ [pretty (last body)]) . init . map (\ b -> pretty b <> comma)) body

instance Pretty EnumMemberDeclaration where
    pretty (EnumMemberDeclaration name expression)
        = pretty name <+> maybe empty (\ e -> equals <+> pretty e) expression

instance Pretty InterfaceBody where
    pretty (InterfaceBody declarations)
        = vsep declarations
        
instance Pretty InterfaceMemberDeclaration where
    pretty (InterfaceMethodMemberDeclaration attributes modifiers ty name typeParameters params constraints)
        = pretty attributes $+$ pretty modifiers <+> pretty ty <+> pretty name
        <+> pretty typeParameters <> (parens (pretty params) $+$ tab pConstraints) <> semi
        where
            pConstraints = (vsep . map (tab . pretty)) constraints

    pretty (InterfacePropertyMemberDeclaration attributes modifiers ty name accessor1 accessor2)
        = pretty attributes $+$ pretty modifiers <+> pretty ty <+> pretty name
        $+$ lbrace $+$ tab pAccessor1 $+$ tab pAccessor2 $+$ rbrace
        where
            pAccessor1 = maybe empty pretty accessor1
            pAccessor2 = maybe empty pretty accessor2

    pretty (InterfaceEventMemberDeclaration attributes modifiers ty name)
        = pretty attributes $+$ pretty modifiers <+> text "event" <+> pretty ty
        <+> pretty name <+> semi

    pretty (InterfaceIndexerMemberDeclaration attributes modifiers ty params accessor1 accessor2)
        = pretty attributes $+$ pretty modifiers <+> pretty ty <+> text "this"
        <> brackets (pretty params) $+$ lbrace $+$ tab pAccessor1 $+$ tab pAccessor2 $+$ rbrace
        where
            pAccessor1 = maybe empty pretty accessor1
            pAccessor2 = maybe empty pretty accessor2

instance Pretty InterfaceAccessor where
    pretty (GetInterfaceAccessor attributes) = pretty attributes $+$ text "get;"
    pretty (SetInterfaceAccessor attributes) = pretty attributes $+$ text "set;"

instance Pretty MemberDeclaration where
    pretty (FieldMemberDeclaration attributes modifiers ty declarators)
        = pretty attributes $+$ pretty modifiers <+> pretty ty 
        <+> pretty declarators <> semi

    pretty (MethodMemberDeclaration attributes modifiers ty name typeParameters params constraints body)
        = pretty attributes $+$ pretty modifiers <+> pretty ty <+> pretty name 
        <> pretty typeParameters <> parens (pretty params) $+$ pConstraints $+$ pretty body
        where
            pConstraints = (vsep . map (tab . pretty)) constraints

    pretty (PropertyMemberDeclaration attributes modifiers ty name body)
        = pretty attributes $+$ pretty modifiers <+> pretty ty <+> pretty name 
        <+> pretty body

    pretty (EventVariableMemberDeclaration attributes modifiers ty declarators)
        = pretty attributes $+$ pretty modifiers <+> text "event" <+> pretty ty
        <+> pretty declarators <> semi

    pretty (EventAccessorMemberDeclaration attributes modifiers ty name accessor1 accessor2)
        = pretty attributes $+$ pretty modifiers <+> text "event" <+> pretty ty
        <+> pretty name $+$ lbrace $+$ tab (pretty accessor1) 
        $+$ tab (pretty accessor2) $+$ rbrace

    pretty (IndexerMemberDeclaration attributes modifiers declarator body)
        = pretty attributes $+$ pretty modifiers <+> pretty declarator 
        $+$ pretty body

    pretty (OperatorMemberDeclaration attributes modifiers declarator body)
        = pretty attributes $+$ pretty modifiers <+> pretty declarator
        $+$ pretty body

    pretty (ConstructorMemberDeclaration attributes modifiers name params initializer body)
        = pretty attributes $+$ pretty modifiers <+> pretty name 
        <> parens (pretty params) $+$ pInitializer $+$ pretty body
        where
            pInitializer = maybe empty (tab . pretty) initializer

    pretty (DestructorMemberDeclaration attributes modifiers name body)
        = pretty attributes $+$ pretty modifiers <+> char '~' <> pretty name 
        <> text "()" $+$ pretty body 

    pretty (TypeMemberDeclaration declaration)
        = pretty declaration

instance Pretty EventAccessor where
    pretty (AddEventAccessor attributes body)
        = pretty attributes $+$ text "add" <+> braces (pretty body)

    pretty (RemoveEventAccessor attributes body)
        = pretty attributes $+$ text "remove" <+> braces (pretty body)

instance Pretty OperatorDeclarator where
    pretty (UnaryOperatorDeclarator ty op paramTy paramName)
        = pretty ty <+> text "operator" <+> pretty op <> parens pParam
        where pParam = pretty paramTy <+> pretty paramName

    pretty (BinaryOperatorDeclarator ty op paramTy1 paramName1 paramTy2 paramName2)
        = pretty ty <+> text "operator" <+> pretty op <> parens pParams
        where 
            pParams = pParam1 <> comma <> pParam2
            pParam1 = pretty paramTy1 <+> pretty paramName1
            pParam2 = pretty paramTy2 <+> pretty paramName2
            
    pretty (ImplicitConversionOperatorDeclarator ty paramTy paramName)
        = text "implicit operator" <+> pretty ty <> parens pParam
        where pParam = pretty paramTy <+> pretty paramName
    
    pretty (ExplicitConversionOperatorDeclarator ty paramTy paramName)
        = text "explicit operator" <+> pretty ty <> parens pParam
        where pParam = pretty paramTy <+> pretty paramName

instance Pretty OverloadableUnaryOperator where
    pretty op = text $ 
        case op of
            OverloadableUnaryPlus       -> "+"
            OverloadableUnaryMinus      -> "-"
            OverloadableUnaryNot        -> "!"
            OverloadableUnaryBitwiseNot -> "~"
            OverloadableUnaryPlusPlus   -> "++"
            OverloadableUnaryMinusMinus -> "--"
            OverloadableUnaryTrue       -> "true"
            OverloadableUnaryFalse      -> "false"

instance Pretty OperatorBody where
    pretty (OperatorStatementBody statements)
        = lbrace $+$ tab (pretty statements) $+$ rbrace

    pretty (OperatorExpressionBody expression)
        = tab (text "=>" <+> pretty expression <> semi)

    pretty OperatorNoBody
        = semi

instance Pretty IndexerBody where
    pretty (IndexerAccessor accessor1 accessor2)
        = lbrace $+$ tab pAccessor1 $+$ tab pAccessor2 $+$ rbrace
        where
            pAccessor1 = maybe empty pretty accessor1
            pAccessor2 = maybe empty pretty accessor2

    pretty (IndexerLambda expression)
        = text "=>" <+> pretty expression <> semi

instance Pretty IndexerDeclarator where
    pretty (IndexerDeclaratorThis ty params)
        = pretty ty <+> text "this" <> brackets (pretty params)
        
    pretty (IndexerDeclaratorInterface ty interface params)
        = pretty ty <+> pretty interface <> dot <> text "this" <> brackets (pretty params)

instance Pretty PropertyBody where
    pretty (PropertyBody accessor1 accessor2 initializer)
        = braces (pAccessor1 <+> pAccessor2) <+> pInitializer 
        where
            pAccessor1   = maybe empty pretty accessor1
            pAccessor2   = maybe empty pretty accessor2
            pInitializer = maybe empty (\ i -> equals <+> pretty i <> semi) initializer

    pretty (PropertyLambda expression)
        = text "=>" <+> pretty expression <> semi

instance Pretty AccessorDeclaration where
    pretty (GetAccessorDeclaration attributes modifiers body)
        = pretty attributes $+$ pretty modifiers <+> text "get" <> pBody
        where
            pBody = maybe semi (\ b -> space <> (braces . pretty) b) body

    pretty (SetAccessorDeclaration attributes modifiers body)
        = pretty attributes $+$ pretty modifiers <+> text "set" <> pBody
        where
            pBody = maybe semi (\ b -> space <> (braces . pretty) b) body        

instance Pretty ConstructorBody where
    pretty (ConstructorStatementBody statements)
        = lbrace $+$ tab (pretty statements) $+$ rbrace

instance Pretty DestructorBody where
    pretty (DestructorStatementBody statements)
        = lbrace $+$ tab (pretty statements) $+$ rbrace

instance Pretty ConstructorInitializer where
    pretty (ConstructorBaseCall arguments)
        = colon <+> text "base" <> parens (pretty arguments)
    
    pretty (ConstructorThisCall arguments)
        = colon <+> text "this" <> parens (pretty arguments)

instance Pretty [Argument] where
    pretty = hcatIntersperse comma

instance Pretty Argument where
    pretty (Argument name expression)
        = pName <+> pretty expression
        where
            pName = maybe empty (\ n -> pretty n <> semi) name 
            
    pretty (RefArgument name expression)
        = pName <+> text "ref" <+> pretty expression
        where
            pName = maybe empty (\ n -> pretty n <> semi) name 
        
    pretty (OutArgument name expression)
        = pName <+> text "out" <+> pretty expression
        where
            pName = maybe empty (\ n -> pretty n <> semi) name 

instance Pretty MethodBody where
    pretty (MethodStatementBody statements)
        = lbrace $+$ tab (pretty statements) $+$ rbrace

    pretty (MethodExpressionBody expression)
        = tab (text "=>" <+> pretty expression <> semi)

    pretty MethodNoBody
        = semi

--------------------------------------------------------------------------------
-- Formal parameters.
--------------------------------------------------------------------------------

instance Pretty FormalParams where
    pretty (FormalParams params paramsParam)
        = (hcat . intersperse comma) pParams
        where
            pParams      = map pretty params ++ pParamsParam
            pParamsParam = maybe [] (\ p -> [pretty p]) paramsParam

instance Pretty FormalParam where
    pretty (FormalParam modifier ty name expr)
        = pModifier <+> pretty ty <+> pretty name <+> pExpr
        where
            pModifier = maybe empty pretty modifier
            pExpr     = maybe empty ((equals <>) . pretty) expr

instance Pretty ParamArray where
    pretty (ParamArray ty name) 
        = text "params" <+> pretty ty <+> pretty name

instance Pretty ParameterModifier where
    pretty modifier = text $ 
        case modifier of
            RefParam -> "ref" ; OutParam -> "out" ; ThisParam -> "this"

--------------------------------------------------------------------------------
-- Statements.
--------------------------------------------------------------------------------

instance Pretty [Statement] where
    pretty = vsep

instance Pretty Statement where
    pretty (Labeled label statement) 
        = pretty label <> colon <+> pretty statement

    pretty (Declaration declaration)
        = pretty declaration <> semi

    pretty (Block statements)
        = lbrace $+$ tab (pretty statements) $+$ rbrace

    pretty Empty
        = semi

    pretty (ExpressionStatement expression)
        = pretty expression <> semi

    pretty (IfThenElse guard trueBody falseBody)
        = text "if" <+> parens (pretty guard)
        $+$ prettyEmbedded trueBody $+$ pFalseBody
        where
            pFalseBody = maybe empty (\ b -> text "else" $+$ prettyEmbedded b) falseBody

    pretty (Switch expression blocks)
        = text "switch" <+> parens (pretty expression) 
        $+$ lbrace $+$ tab (pretty blocks) $+$ rbrace

    pretty (While guard body)
        = text "while" <+> parens (pretty guard) $+$ prettyEmbedded body

    pretty (Do body guard)
        = text "do" $+$ prettyEmbedded body $+$ text "while" <+> parens (pretty guard) <> semi

    pretty (For initializer guard iterator body)
        = text "for" <+> parens (pInitializer <> semi <+> pGuard <> semi <+> pIterator)
        $+$ prettyEmbedded body
        where
            pInitializer = maybe empty pretty initializer
            pGuard       = maybe empty pretty guard
            pIterator    = maybe empty (hcatIntersperse comma) iterator

    pretty (ForEach ty var expression body)
        = text "foreach" <+> parens pHeader $+$ prettyEmbedded body
        where
            pHeader = pretty ty <+> pretty var <+> text "in" <+> pretty expression

    pretty Break         = text "break" <> semi
    pretty Continue      = text "continue" <> semi
    pretty (Goto target) = text "goto" <+> pretty target <> semi

    pretty (Return expression)
        = text "return" <+> maybe empty pretty expression <> semi

    pretty (Throw expression)
        = text "throw" <+> maybe empty pretty expression <> semi

    pretty (Try statements catches finally)
        = text "try" $+$ lbrace $+$ tab (pretty statements) $+$ rbrace
        $+$ pretty catches $+$ pFinally
        where
            pFinally = case finally of 
                        [] -> empty
                        _  -> text "finally" $+$ lbrace $+$ tab (pretty finally) $+$ rbrace

    pretty (CheckedStatement statements)
        = text "checked" $+$ lbrace $+$ tab (pretty statements) $+$ rbrace
        
    pretty (UncheckedStatement statements)
        = text "unchecked" $+$ lbrace $+$ tab (pretty statements) $+$ rbrace

    pretty (Lock expression body)
        = text "lock" <+> parens (pretty expression) $+$ prettyEmbedded body

    pretty (UsingStatement resource body)
        = text "using" <+> parens (pretty resource) $+$ prettyEmbedded body

    pretty (Yield expression)
        = text "yield" <+> pExpression <> semi
        where
            pExpression = maybe (text "break") (\ e -> text "return" <+> pretty e) expression

instance Pretty ForInitializer where
    pretty (ForInitializerDeclaration declaration)
        = pretty declaration

    pretty (ForInitializerExpressions expression)
        = (hcat . intersperse comma . map pretty) expression

instance Pretty LocalVarDeclaration where
    pretty (LocalVarDeclaration ty declarators)
        = pretty ty <+> pretty declarators

instance Pretty [Catch] where
    pretty = vsep

instance Pretty Catch where
    pretty (Catch specifier when statements)
        = text "catch" <+> pSpecifier $+$ tab pWhen
        $+$ lbrace $+$ tab (pretty statements) $+$ rbrace
        where
            pSpecifier = maybe empty (parens . pretty) specifier
            pWhen      = maybe empty (\ e -> text "when" <+> parens (pretty e)) when

instance Pretty ExceptionSpecifier where
    pretty (ExceptionSpecifier ty name)
        = pretty ty <+> maybe empty pretty name

instance Pretty GotoTarget where
    pretty (GotoLabel label)     = pretty label
    pretty (GotoCase expression) = text "case" <+> pretty expression
    pretty GotoDefault           = text "default"

instance Pretty [SwitchBlock] where
    pretty = vsep

instance Pretty SwitchBlock where
    pretty (LabeledBlock guard statements) 
        = text "case" <+> pretty guard <> semi $+$ tab (pretty statements)
        
    pretty (DefaultBlock statements) 
        = text "default:" $+$ tab (pretty statements)

instance Pretty ResourceAcquisition where
    pretty (ResourceAcquisitionVariable declarators)
        = pretty declarators

    pretty (ResourceAcquisitionExpression expression)
        = pretty expression

instance Pretty [VariableDeclarator] where
    pretty = hcatIntersperse comma

instance Pretty VariableDeclarator where
    pretty (VariableDeclarator name initializer)
        = pretty name <+> pInitializer
        where
            pInitializer = maybe empty (\ i -> equals <+> pretty i) initializer

instance Pretty VariableInitializer where
    pretty (VariableInitializerExpression expression)
        = pretty expression

    pretty (VariableInitializerArray initializer)
        = braces (pretty initializer)

instance Pretty ArrayInitializer where
    pretty (ArrayInitializer initializers)
        = hcatIntersperse comma initializers

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

instance Pretty Expression where
    pretty (Literal literal) 
        = pretty literal

    pretty (SimpleName name typeArguments)
        = pretty name <> pretty typeArguments

    pretty (Parenthesized expression)
        = parens (pretty expression)

    pretty (Assign target op expression)
        = pretty target <+> pretty op <+> pretty expression
        
    pretty (MemberAccess access)
        = pretty access

    pretty (Invocation expression arguments)
        = pretty expression <> parens (pretty arguments)

    pretty (ElementAccess expression indices)
        = pretty expression <> brackets (hcatIntersperse comma indices)

    pretty This = text "this"
    pretty Base = text "base"

    pretty (ObjectCreationExpression ty arguments initializer)
        = text "new" <+> pretty ty <> parens pArguments <+> pInitializer
        where
            pArguments   = hcatIntersperse comma arguments 
            pInitializer = maybe empty pretty initializer

    pretty (ObjectCreationTypeInitializer ty initializer)
        = text "new"<+> pretty ty <+> pretty initializer

    pretty (ArrayCreationExpression ty sizes ranks initializer)
        = text "new" <+> pretty ty <> brackets (hcatIntersperse comma sizes)
        <> (hcat . map pretty) ranks <+> maybe empty pretty initializer

    pretty (ArrayCreationTypeInitializer ty initializer)
        = text "new" <+> pretty ty <+> pretty initializer

    pretty (ArrayCreationRankInitializer rank initializer)
        = text "new" <> pretty rank <+> pretty initializer

    pretty (Sizeof ty)
        = text "sizeof" <> parens (pretty ty)

    pretty (Typeof expression)
        = text "typeof" <> parens (pretty expression)

    pretty (Checked expression)
        = text "checked" <> parens (pretty expression)

    pretty (Unchecked expression)
        = text "unchecked" <> parens (pretty expression)
        
    pretty (Default ty)
        = text "default" <> parens (pretty ty)

    pretty (BinaryOperator op exp1 exp2)
        = pretty exp1 <+> pretty op <+> pretty exp2

    pretty (Conditional guard exp1 exp2)
        = pretty guard <+> char '?' <+> pretty exp1 <+> char ':' <+> pretty exp2

    pretty (Nameof entity)
        = text "nameof" <> parens (pretty entity)

    pretty (Delegate sig body)
        = text "delegate" <+> maybe empty pretty sig <+> braces (pretty body)

    pretty (Lambda sig body)
        = pretty sig <+> text "=>" <+> pretty body

    pretty (UnaryPlus expression) 
        = text "+" <> pretty expression

    pretty (UnaryMinus expression) 
        = text "-"  <> pretty expression

    pretty (UnaryNot expression) 
        = text "!" <> pretty expression

    pretty (UnaryBitwiseNot expression) 
        = text "~" <> pretty expression

    pretty (UnaryPreIncrement expression) 
        = text "++" <> pretty expression
        
    pretty (UnaryPreDecrement  expression) 
        = text "--" <> pretty expression

    pretty (UnaryPostIncrement expression) 
        = pretty expression <> text "++"

    pretty (UnaryPostDecrement expression) 
        = pretty expression <> text "--"

    pretty (UnaryCast ty expression)
        = parens (pretty ty) <> pretty expression

    pretty (UnaryAwait expression) 
        = text "await" <+> pretty expression

instance Pretty AnonymousFunctionSignature where
    pretty (ExplicitAnonymousFunctionSignature params)
        = parens (hcatIntersperse comma params)

    pretty (ImplicitAnonymousFunctionSignature [param])
        = pretty param

    pretty (ImplicitAnonymousFunctionSignature params)
        = parens (hcatIntersperse comma params)

instance Pretty AnonymousFunctionParameter where
    pretty (ExplicitAnonymousFunctionParameter modifier ty name)
        = maybe empty pretty modifier <+> pretty ty <+> pretty name

instance Pretty AnonymousFunctionBody where
    pretty (AnonymousFunctionStatementBody body)
        = braces (pretty body)

    pretty (AnonymousFunctionExpressionBody expression)
        = pretty expression

instance Pretty NameofEntity where
    pretty (NameofIdentifier name)        = pretty name
    pretty (NameofThis name)              = text "this" <> dot <> pretty name
    pretty (NameofBase name)              = text "base" <> dot <> pretty name
    pretty (NameofEntity entity name)     = pretty entity <> dot <> pretty name
    pretty (NameofPredefinedType ty name) = pretty ty <> dot <> pretty name

instance Pretty TypeOfExpression where
    pretty (TypeofType ty)
        = pretty ty

instance Pretty ObjectCreationInitializer where
    pretty (ObjectInitializer initializers)
        = braces (hcatIntersperse comma initializers)
        
    pretty (CollectionInitializer initializer)
        = pretty initializer

instance Pretty ArrayCreationInitializer where
    pretty (ArrayCreationInitializerExpression expressions)
        = braces (hcatIntersperse comma expressions)

    pretty (ArrayCreationInitializerInitializers initializers)
        = braces (hcatIntersperse comma initializers)

instance Pretty MemberInitializer where
    pretty (MemberInitializer target value)
        = pretty target <+> equals <+> pretty value

instance Pretty InitializerTarget where
    pretty (InitializerTargetIdentifier name)
        = pretty name

    pretty (InitializerTargetList arguments)
        = brackets (pretty arguments)

instance Pretty InitializerValue where
    pretty (InitializerValueExpression expression)
        = pretty expression

    pretty (InitializerValueInitializer initializer)
        = pretty initializer

instance Pretty Literal where
    pretty (BooleanLit True)         = text "true"
    pretty (BooleanLit False)        = text "false"
    pretty (IntLit value)            = text $ show value
    pretty (UIntLit value)           = text $ show value
    pretty (LongLit value)           = text $ show value
    pretty (ULongLit value)          = text $ show value
    pretty (FloatLit value)          = text $ show value ++ "f"
    pretty (DoubleLit value)         = text $ show value ++ "d"
    pretty (DecimalLit value)        = text $ show value ++ "m"
    pretty (CharLit value)           = quotes (text value)
    pretty (StringLit value)         = doubleQuotes (text value)
    pretty (VerbatimStringLit value) = char '@' <> doubleQuotes (text value)
    pretty NullLit                   = text "null"

instance Pretty MemberAccess where
    pretty (PrimaryMemberAccess expression name typeArguments)
        = pretty expression <> dot <> pretty name <> pretty typeArguments

    pretty (PredefinedMemberAccess ty name typeArguments)
        = pretty ty <> dot <> pretty name <> pretty typeArguments

    pretty (QualifiedMemberAccess namespace member name)
        = pretty namespace <> text "::" <> pretty member <> dot <> pretty name

instance Pretty AssignmentOperator where
    pretty op = text $
        case op of
            OpAssign                  -> "="  ; OpAssignPlus             -> "+="
            OpAssignMinus             -> "-=" ; OpAssignMultiply         -> "*="
            OpAssignDivide            -> "/=" ; OpAssignModulo           -> "%="
            OpAssignBitwiseAnd        -> "&=" ; OpAssignBitwiseOr        -> "|="
            OpAssignBitwiseXor        -> "^=" ; OpAssignBitwiseLeftShift -> "<<="
            OpAssignBitwiseRightShift -> ">>="

instance Pretty BinaryOperator where
    pretty op = text $ 
        case op of 
            BinaryPlus             -> "+" ; BinaryMinus       -> "-" 
            BinaryMultiply         -> "*" ; BinaryDivide      -> "/"
            BinaryModulo           -> "%" ; BinaryShiftLeft   -> "<<"
            BinaryShiftRight       -> ">>"; BinaryEquals      -> "==" 
            BinaryNotEquals        -> "!="; BinaryLessThan    -> "<"   
            BinaryLessThanEqual    -> "<="; BinaryGreaterThan -> ">"
            BinaryGreaterThanEqual -> ">="; BinaryBitwiseAnd  -> "&"
            BinaryBitwiseXor       -> "^" ; BinaryBitwiseOr   -> "|"
            BinaryAnd              -> "&&"; BinaryOr          -> "||"
            BinaryIs               -> "is"; BinaryAs          -> "as"
            BinaryNullCoalescing   -> "??"


--------------------------------------------------------------------------------
-- Type parameters.
--------------------------------------------------------------------------------

instance Pretty [TypeParameter] where
    pretty []     = empty
    pretty params = diamonds (hcatIntersperse comma params)

instance Pretty TypeParameter where
    pretty (TypeParameter name) = pretty name

instance Pretty TypeParameterConstraintClause where
    pretty (TypeParameterConstraintClause typeParam constraints)
        = text "where" <+> pretty typeParam <+> colon <+> hcatIntersperse comma constraints

instance Pretty TypeParameterConstraint where
    pretty (TypeConstraint ty) = pretty ty
    pretty ClassConstraint     = text "class"
    pretty StructConstraint    = text "struct"
    pretty NewConstraint       = text "new()"

instance Pretty [VariantTypeParameter] where
    pretty []     = empty
    pretty params = diamonds (hcatIntersperse comma params)

instance Pretty VariantTypeParameter where
    pretty (VariantTypeParameter variance name)
        = maybe empty pretty variance <+> pretty name

instance Pretty [TypeArgument] where
    pretty []        = empty
    pretty arguments = (diamonds . hcatIntersperse comma) arguments

instance Pretty Variance where
    pretty VarianceIn  = text "in"
    pretty VarianceOut = text "out"

instance Pretty TypeArgument where
    pretty (TypeArgument ty) = pretty ty

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

instance Pretty Type where
    pretty (TypeNamed name)  = pretty name
    pretty (TypeArray ty)    = pretty ty
    pretty (TypeSimple ty)   = pretty ty
    pretty TypeDynamic       = text "dynamic"
    pretty (TypeNullable ty) = pretty ty <> char '?'

instance Pretty (Maybe Type) where
    pretty = maybe (text "void") pretty

instance Pretty SimpleType where
    pretty ty = case ty of 
                    Char   -> text "char"  ; Bool   -> text "bool"   
                    Object -> text "object"; String -> text "string" 
                    IntegralType ty'      -> pretty ty'
                    FloatingPointType ty' -> pretty ty'

instance Pretty IntegralType where
    pretty ty = text $ 
        case ty of
            SByte  -> "sbyte" ; Byte  -> "byte"  ; Short -> "short"
            UShort -> "ushort"; Int   -> "int"   ; UInt  -> "uint"
            Long   -> "long"  ; ULong -> "ulong"         

instance Pretty FloatingPointType where
    pretty Float   = text "float"
    pretty Double  = text "double"
    pretty Decimal = text "decimal"

instance Pretty ArrayType where
    pretty (ArrayType ty ranks)
        = pretty ty <> (hcat . map pretty) ranks

instance Pretty RankSpecifier where
    pretty (RankSpecifier rank) = brackets (hcat (replicate rank comma))

instance Pretty LocalVarType where
    pretty (VarType ty) = pretty ty
    pretty Var          = text "var" 

instance Pretty TypeName where
    pretty (TypeName name args)
        = pretty name <> pretty args

    pretty (TypeAlias namespace name typeArguments)
        = pretty namespace <> text "::" <> pretty name <> pretty typeArguments

--------------------------------------------------------------------------------
-- Attribute definitions.
--------------------------------------------------------------------------------

instance Pretty [GlobalAttributeSection] where
    pretty = vsep

instance Pretty GlobalAttributeSection where
    pretty (GlobalAttributeSection target attributes)
        = brackets (pTarget <+> hcatIntersperse comma attributes)
        where
            pTarget = maybe empty (\ t -> pretty t <+> colon) target

instance Pretty GlobalAttributeTarget where
    pretty AttributeTargetAssembly = text "assembly" 
    pretty AttributeTargetModule   = text "module" 

instance Pretty [AttributeSection] where
    pretty = vsep

instance Pretty AttributeSection where
    pretty (AttributeSection target attributes)
        = brackets (pTarget <+> hcatIntersperse comma attributes)
        where
            pTarget = maybe empty (\ t -> pretty t <+> colon) target

instance Pretty AttributeTarget where
    pretty AttributeTargetField    = text "field" 
    pretty AttributeTargetEvent    = text "event" 
    pretty AttributeTargetMethod   = text "method" 
    pretty AttributeTargetParam    = text "param" 
    pretty AttributeTargetProperty = text "property" 
    pretty AttributeTargetReturn   = text "return" 
    pretty AttributeTargetType     = text "type" 

instance Pretty Attribute where
    pretty (Attribute name arguments) 
        = pretty name <> parens (hcatIntersperse comma arguments)

instance Pretty AttributeArgument where
    pretty (AttributeArgumentExpression expression) 
        = pretty expression

    pretty (AttributeArgumentNamed name expression) 
        = pretty name <+> equals <+> pretty expression

--------------------------------------------------------------------------------
-- Auxiliary definitions.
--------------------------------------------------------------------------------

instance Pretty [Modifier] where
    pretty = hsep . map pretty

instance Pretty Modifier where
    pretty modifier 
        = case modifier of
             Public   -> text "public"   ; Private   -> text "private"
             Internal -> text "internal" ; Protected -> text "protected"
             Abstract -> text "abstract" ; Async     -> text "async"
             Const    -> text "const"    ; Event     -> text "event"
             Extern   -> text "extern"   ; New       -> text "new"
             Override -> text "override" ; Readonly  -> text "readonly"
             Sealed   -> text "sealed"   ; Static    -> text "static"
             Unsafe   -> text "unsafe"   ; Virtual   -> text "virtual"
             Volatile -> text "volatile" ; Partial   -> text "partial"
             
instance Pretty Identifier where
    pretty (Identifier identifier)
        = text identifier

instance Pretty Name where
    pretty (Name name) = hcatIntersperse dot name

--------------------------------------------------------------------------------
-- Utility functions.
--------------------------------------------------------------------------------

prettyEmbedded :: Statement -> Doc
prettyEmbedded body@(Block _) = pretty body
prettyEmbedded body           = tab (pretty body)

tab :: Doc -> Doc
tab = nest 4

dot :: Doc
dot = char '.'

diamonds :: Doc -> Doc
diamonds doc = char '<' <> doc <> char '>'

hcatIntersperse :: Pretty a => Doc -> [a] -> Doc
hcatIntersperse delimiter = hcat . intersperse delimiter . map pretty

vsep :: Pretty a => [a] -> Doc
vsep = foldr (($+$) . pretty) empty