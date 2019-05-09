{-|
Module      : Language.CSharp.Syntax
Description : Abstract syntax tree of C#.

This module contains the abstract syntax tree of C#.
-}

module Language.CSharp.Syntax where

-- | A compilation unit is the top level definition of a C# program.
data CompilationUnit = CompilationUnit [Using] [Declaration]
    deriving (Show)

-- | A using declaration appears within a compilation unit to indicate which
-- namespace to include.
data Using = Using Name Bool
    deriving (Show)

-- | A declaration appears within a compilation unit introducing a namespace,
-- or a type declaration.
data Declaration
    -- | A namespace declaration.
    = NamespaceDeclaration [GlobalAttributeSection] Name [Declaration]
    -- | A type declaration.
    | TypeDeclaration TypeDeclaration
    deriving (Show)

-- | A type declaration appears withing a compilation unit, a namespace, or
-- another type declaration. A type declaration introduces a new type which can
-- be either a class, struct, enum, interface or delegate.
data TypeDeclaration
    -- | A class declaration containing its attributes, modifiers, identifier,
    -- type parameters, the types it inherits from, the type parameter constaints 
    -- and its body.
    = ClassTypeDeclaration [AttributeSection] [Modifier] Identifier [TypeParameter] [TypeName] [TypeParameterConstraintClause] ClassBody
    -- | A struct declaration containing its attributes, modifiers, identifier,
    -- type parameters, the types it inherits from, the type parameter constraints 
    -- and its body.
    | StructTypeDeclaration [AttributeSection] [Modifier] Identifier [TypeParameter] [TypeName] [TypeParameterConstraintClause] StructBody
    -- | An enum declaration containing its attributes, modifiers, identifier,
    -- the type it inherits from and its body.
    | EnumTypeDeclaration [AttributeSection] [Modifier] Identifier (Maybe IntegralType) EnumBody
    -- | An interface declaration containing its attributes, modifiers, identifier,
    -- the type parameters, the types it inherits from, the type parameter constraints 
    -- and its body.
    | InterfaceTypeDeclaration [AttributeSection] [Modifier] Identifier [VariantTypeParameter] [TypeName] [TypeParameterConstraintClause] InterfaceBody
    -- | A delegate declaration containing its attributes, modifiers, return type,
    -- identifier, type parameters, formal parameters and type parameter constraints.
    | DelegateTypeDeclaration [AttributeSection] [Modifier] (Maybe Type) Identifier [VariantTypeParameter] FormalParams [TypeParameterConstraintClause]
    deriving (Show)

-- | A class body appears within a class type declaration.
newtype ClassBody = ClassBody [MemberDeclaration]
    deriving (Show)

-- | A struct body appears within a struct type declaration.
newtype StructBody = StructBody [MemberDeclaration]
    deriving (Show)

-- | An enum body appears within an enum type declaration.
newtype EnumBody = EnumBody [EnumMemberDeclaration]
    deriving (Show)

-- | An enum member declaration appears within an enum body.
data EnumMemberDeclaration 
    -- | An enum member declaration containing the identifier and its value.
    = EnumMemberDeclaration Identifier (Maybe Expression)
    deriving (Show)

-- | An interface body appears within an interface type declaration.
newtype InterfaceBody = InterfaceBody [InterfaceMemberDeclaration]
    deriving (Show)

-- | An interface member declaration appears within an interface body.
data InterfaceMemberDeclaration
    -- | A method declaration containing its attributes, modifiers, return type
    -- identifier, type parameters, formal parameters and type parameter constraints.
    = InterfaceMethodMemberDeclaration [AttributeSection] [Modifier] (Maybe Type) Identifier [TypeParameter] FormalParams [TypeParameterConstraintClause]
    -- | A property declaration containing its attributes, modifiers, type
    -- identifier and accessor(s). At least one accessor is defined and they can
    -- appear in any order.
    | InterfacePropertyMemberDeclaration [AttributeSection] [Modifier] Type Identifier (Maybe InterfaceAccessor) (Maybe InterfaceAccessor)
    -- | An event declaration containing its attributes, modifiers, type and identifier.
    | InterfaceEventMemberDeclaration [AttributeSection] [Modifier] Type Identifier
    -- | An indexer declaration containing its attributes, modifiers, type,
    -- formal parameters and accessor(s). At least one accessor is defined and
    -- they can appear in any order.
    | InterfaceIndexerMemberDeclaration [AttributeSection] [Modifier] Type FormalParams (Maybe InterfaceAccessor) (Maybe InterfaceAccessor)
    deriving (Show)

-- | A member declaration appears within a class- or struct body.
data MemberDeclaration
    -- | A field declaration containing its attributes, modifiers, type and declarator(s).
    = FieldMemberDeclaration [AttributeSection] [Modifier] Type [VariableDeclarator]
    -- | A method declaration containing its attributes, modifiers, return type
    -- name, type parameters, formal parameters, type parameter constraints and
    -- body.
    | MethodMemberDeclaration [AttributeSection] [Modifier] (Maybe Type) Name [TypeParameter] FormalParams [TypeParameterConstraintClause] MethodBody
    -- | A property declaration containing its attributes, modifiers, type,
    -- name and body.
    | PropertyMemberDeclaration [AttributeSection] [Modifier] Type Name PropertyBody
    -- | An event declaration containing its attributes, modifiers, type and
    -- declarator(s).
    | EventVariableMemberDeclaration [AttributeSection] [Modifier] Type [VariableDeclarator]
    -- | An event declaration containing its attributes, modifiers, type, name
    -- and accessors. The accessors can appear in any order.
    | EventAccessorMemberDeclaration [AttributeSection] [Modifier] Type Name EventAccessor EventAccessor
    -- | An indexer declaration containing its attributes, modifiers, declarator
    -- and body.
    | IndexerMemberDeclaration [AttributeSection] [Modifier] IndexerDeclarator IndexerBody
    -- | An operator declaration containing its attributes, modifiers, declarator
    -- and body.
    | OperatorMemberDeclaration [AttributeSection] [Modifier] OperatorDeclarator OperatorBody
    -- | A constructor declaration containing its attributes, identifier, formal
    -- parameters, the base/this constructor call and body.
    | ConstructorMemberDeclaration [AttributeSection] [Modifier] Identifier FormalParams (Maybe ConstructorInitializer) ConstructorBody
    -- | A destructor declaration containing its attributes, modifiers, identifier
    -- and body.
    | DestructorMemberDeclaration [AttributeSection] [Modifier] Identifier DestructorBody
    -- | A nested type declaration.
    | TypeMemberDeclaration TypeDeclaration
    deriving (Show)

-- | A property accessor appears within an property declaration of an interface
-- declaration.
data InterfaceAccessor
    = GetInterfaceAccessor [AttributeSection]
    | SetInterfaceAccessor [AttributeSection]
    deriving (Show)

-- | An event acessor appears within an event declaration.
data EventAccessor
    -- | An add event accessor containing its attributes and statements.
    = AddEventAccessor [AttributeSection] [Statement]
    -- | A remove event accessor containing its attributes and statements.
    | RemoveEventAccessor [AttributeSection] [Statement]
    deriving (Show)

-- | A constructor initializer appears within a constructor declaration.
data ConstructorInitializer
    = ConstructorBaseCall [Argument]
    | ConstructorThisCall [Argument]
    deriving (Show)

-- | An argument appears within any kind of method invocation.
data Argument
    -- | An argument containing its identifier and expression.
    = Argument (Maybe Identifier) Expression
    -- | A ref argument containing its identifier and expression.
    | RefArgument (Maybe Identifier) Expression 
    -- | An out argument containing its identifier and expression.
    | OutArgument (Maybe Identifier) Expression
    deriving (Show)

-- | An operator declarator appears within a operator declaration.
data OperatorDeclarator 
    -- | An unary operator declaration containing its return type, operator, and
    -- formal parameter type and identifier.
    = UnaryOperatorDeclarator Type OverloadableUnaryOperator Type Identifier
    -- | A binary operator declaration containing its return type, operator,
    -- first formal parameter type and identifier, and second formal parameter
    -- type and identifier.
    | BinaryOperatorDeclarator Type BinaryOperator Type Identifier Type Identifier
    -- | An implicit conversion declarator containing its return type, and
    -- formal parameter type and identifier.
    | ImplicitConversionOperatorDeclarator Type Type Identifier
    -- | An explicit conversion declarator containing its return type, and
    -- formal parameter type and identifier.
    | ExplicitConversionOperatorDeclarator Type Type Identifier
    deriving (Show)

-- | An overloadable unary operator appears within a unary operator declarator.
data OverloadableUnaryOperator
    = OverloadableUnaryPlus     | OverloadableUnaryMinus
    | OverloadableUnaryNot      | OverloadableUnaryBitwiseNot
    | OverloadableUnaryPlusPlus | OverloadableUnaryMinusMinus
    | OverloadableUnaryTrue     | OverloadableUnaryFalse
    deriving (Show)

-- | An operator body appears within a operator declaration.
data OperatorBody 
    -- | An operator body containing its statements.
    = OperatorStatementBody  [Statement]
    -- | An operator body containing its expression.
    | OperatorExpressionBody Expression
    -- | An operator body without an implementation.
    | OperatorNoBody
    deriving (Show)

-- | A constructor body appears within a constructor declaration.
newtype ConstructorBody = ConstructorStatementBody [Statement]
    deriving (Show)

-- | A destructor body appears within a destructor declaration.
newtype DestructorBody = DestructorStatementBody [Statement]
    deriving (Show)

-- | A indexer body appears within an indexer declaration.
data IndexerBody 
    -- | An indexer body containing its accessor(s). At least one accessor is
    -- defined and they can appear in any order.
    = IndexerAccessor (Maybe AccessorDeclaration) (Maybe AccessorDeclaration)
    -- | An indexer body containing its expression.
    | IndexerLambda Expression
    deriving (Show)

-- | An indexer declarator appears within an indexer declaration.
data IndexerDeclarator 
    -- | An indexer declarator containing its return type and formal parameters.
    = IndexerDeclaratorThis Type FormalParams
    -- | An indexer declarator containing its return type, interface type and
    -- formal parameters.
    | IndexerDeclaratorInterface Type Type FormalParams
    deriving (Show)

-- | A property body appears within a property declaration.
data PropertyBody 
    -- | A property body containing its accessor(s) and variable initializer. 
    -- At least one accessor is defined an they can appear in any order.
    = PropertyBody (Maybe AccessorDeclaration) (Maybe AccessorDeclaration) (Maybe VariableInitializer)
    -- | A propety body containing its expression.
    | PropertyLambda Expression
    deriving (Show)

-- | An accessor declaration appears within a property body.
data AccessorDeclaration 
    -- | A get propety declaration containing its attributes, modifiers and body.
    = GetAccessorDeclaration [AttributeSection] [Modifier] (Maybe [Statement])
    -- | A set propety declaration containing its attributes, modifiers and body.
    | SetAccessorDeclaration [AttributeSection] [Modifier] (Maybe [Statement])
    deriving (Show)

-- | A method body appears within a method declaration.
data MethodBody
    -- | An method body containing its statements.
    = MethodStatementBody  [Statement]
    -- | An method body containing its expression.
    | MethodExpressionBody Expression
    -- | An method body without an implementation.
    | MethodNoBody
    deriving (Show)

--------------------------------------------------------------------------------
-- Formal parameters.
--------------------------------------------------------------------------------

-- | A formal parameters containing the formal parameters and a params array.
data FormalParams
    = FormalParams [FormalParam] (Maybe ParamArray)
    deriving (Show)

-- | A formal parameter containing its modifier ty identifier and default value.
data FormalParam
    = FormalParam (Maybe ParameterModifier) Type Identifier (Maybe Expression)
    deriving (Show)

-- | A formal params array containing its array type and identifier.
data ParamArray
    = ParamArray ArrayType Identifier
    deriving (Show)

-- | A formal parameter modifier.
data ParameterModifier = RefParam | OutParam | ThisParam
    deriving (Show)

--------------------------------------------------------------------------------
-- Statements.
--------------------------------------------------------------------------------

-- | A statement.
data Statement
    -- | A labeled statement containing its label and statement.
    = Labeled Identifier Statement
    -- | A declaration containing its variable declaration.
    | Declaration LocalVarDeclaration
    -- | A block statement.
    | Block [Statement] 
    -- | An empty statement.
    | Empty
    -- | An expression statement.
    | ExpressionStatement Expression
    -- | An if then else statement containing its guard, true body and false body.
    | IfThenElse Expression Statement (Maybe Statement)
    -- | A switch statement containing its guard and switch blocks.
    | Switch Expression [SwitchBlock]
    -- | A while statement containing its guard and body.
    | While Expression Statement
    -- | A do while statement containing its body and guard.
    | Do Statement Expression
    -- | A for statement containing its initializer, guard, increment expression and body.
    | For (Maybe ForInitializer) (Maybe Expression) (Maybe [Expression]) Statement
    -- | A foreach statement containing its variable type name and expression, and body.
    | ForEach LocalVarType Identifier Expression Statement
    -- | A break statement.
    | Break
    -- | A continue statement.
    | Continue
    -- | A goto statement.
    | Goto GotoTarget
    -- | A return statement.
    | Return (Maybe Expression)
    -- | A throw statement.
    | Throw (Maybe Expression)
    -- | A try catch finally statement containing the try body, catches and finally body.
    | Try [Statement] [Catch] [Statement]
    -- | A checked statement.
    | CheckedStatement [Statement]
    -- | An unchecked statement.
    | UncheckedStatement [Statement]
    -- | A lock statement containing its locking object and body.
    | Lock Expression Statement
    -- | An using statement containing its resource and body.
    | UsingStatement ResourceAcquisition Statement
    -- | A yield statement containing its return expression.
    | Yield (Maybe Expression)
    deriving (Show)

-- | A local variable declaration appears within a local declaration or for initializer.
data LocalVarDeclaration
    -- | A local variable declaration containing its type and declarators.
    = LocalVarDeclaration LocalVarType [VariableDeclarator]
    deriving (Show)

-- | A goto target appears within a goto statement.
data GotoTarget
    = GotoLabel   Identifier
    | GotoCase    Expression
    | GotoDefault
    deriving (Show)

-- | A switch block appears within a switch statement.
data SwitchBlock
    -- | A labeled block containing its label and body.
    = LabeledBlock Expression [Statement]
    -- | A default block containing its body.
    | DefaultBlock [Statement]
    deriving (Show)

-- | A resource acquisition appears within an using statement.
data ResourceAcquisition
    -- | A resource acquisition containing its variable declarators.
    = ResourceAcquisitionVariable [VariableDeclarator]
    -- | A resource acquisition containing its expression.
    | ResourceAcquisitionExpression Expression
    deriving (Show)

-- | A variable declarator containing its identifier and initializer.
data VariableDeclarator = VariableDeclarator Identifier (Maybe VariableInitializer)
    deriving (Show)

-- | A variable initializer.
data VariableInitializer
    -- | A variable initializer containing its expression.
    = VariableInitializerExpression Expression
    -- | A variable initializer containing its array initializer.
    | VariableInitializerArray      ArrayInitializer
    deriving (Show)

-- | A local variable type.
data LocalVarType = VarType Type | Var
    deriving (Show)

-- | An array initializer containing its variable initializers.
newtype ArrayInitializer = ArrayInitializer [VariableInitializer]
    deriving (Show)

-- | A for initializer appears within a for statement.
data ForInitializer
    -- | A for initializer containing its variable declaration.
    = ForInitializerDeclaration LocalVarDeclaration
    -- | A for initializer containing its expressions.
    | ForInitializerExpressions [Expression]
    deriving (Show)

-- | A catch block appears within a try statement.
data Catch
    -- | A catch block containing its exception specifier, exception filter and body.
   = Catch (Maybe ExceptionSpecifier) (Maybe Expression) [Statement]
   deriving (Show)

-- | An exception specifier containing its type and identifier.
data ExceptionSpecifier = ExceptionSpecifier Type (Maybe Identifier)
    deriving (Show)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- | An expression.
data Expression 
    -- | A literal.
    = Literal Literal
    -- | A named value containing its identifer and type arguments.
    | SimpleName Identifier [TypeArgument]
    -- | A parenthesized expression.
    | Parenthesized Expression
    -- | An assignment containing its left-hand side, operator and right-hand side..
    | Assign Expression AssignmentOperator Expression
    -- | An member access.
    | MemberAccess MemberAccess
    -- | An method invocation containing its method and arguments.
    | Invocation Expression [Argument]
    -- | An element access containing its array and indices.
    | ElementAccess Expression [Expression]
    -- | A this access expression.
    | This
    -- | A base access expression.
    | Base
    -- | An object creation containing its type, arguments and initializer.
    -- e.g. @new Object(a, b)@.
    | ObjectCreationExpression Type [Argument] (Maybe ObjectCreationInitializer)
    -- | An object creation containing its type and initializer.
    -- e.g. @new Object { A = a, B = b }@.
    | ObjectCreationTypeInitializer Type ObjectCreationInitializer
    -- | An array creation containing its type, rank specifiers and initialier.
    | ArrayCreationExpression Type [Expression] [RankSpecifier] (Maybe ArrayCreationInitializer)
    -- | An array creation containing its type and intializer.
    | ArrayCreationTypeInitializer Type ArrayCreationInitializer
    -- | An array creation containing its rank specifier and initializer.
    | ArrayCreationRankInitializer RankSpecifier ArrayCreationInitializer
    -- | A sizeof expression.
    | Sizeof Type
    -- | A typeof expression.
    | Typeof TypeOfExpression
    -- | A checked expression.
    | Checked Expression
    -- | An unchecked expression.
    | Unchecked Expression
    -- | A default expression.
    | Default Type
    -- | A binary operator containing its operator, left expression and right expression.
    | BinaryOperator BinaryOperator Expression Expression
    -- | A conditional expression containing its guard, first expression and second
    -- expression,
    | Conditional Expression Expression Expression
    -- | A nameof expression.
    | Nameof NameofEntity
    -- | A delegate expression containing its signature and body.
    | Delegate (Maybe AnonymousFunctionSignature) [Statement]
    -- | A lambda expression containing its signature and body.
    | Lambda AnonymousFunctionSignature AnonymousFunctionBody
    -- | An unary @+@ expression.
    | UnaryPlus Expression
    -- | An unary @-@ expression.
    | UnaryMinus Expression
    -- | An unary @!@ expression.
    | UnaryNot Expression
    -- | An unary @~@ expression.
    | UnaryBitwiseNot Expression
    -- | An unary @++@ expression appearing in front of its expression.
    | UnaryPreIncrement Expression
    -- | An unary @--@ expression appearing in front of its expression.
    | UnaryPreDecrement Expression
    -- | An unary @++@ expression appearing after its expression.
    | UnaryPostIncrement Expression
    -- | An unary @--@ expression appearing after its expression.
    | UnaryPostDecrement Expression
    -- | A cast containing its target type and expression.
    | UnaryCast Type Expression
    -- | An await expression.
    | UnaryAwait Expression
    deriving (Show)

-- | An anonymous function signature appears within a delegate- or lambda expression.
data AnonymousFunctionSignature
    -- | An explicit anonymous function signature containing its formal parameters.
    = ExplicitAnonymousFunctionSignature [AnonymousFunctionParameter]
    -- | An implicit anonymous function signature containing its formal parameters.
    | ImplicitAnonymousFunctionSignature [Identifier]
    deriving (Show)

-- | An anonymous function body appears within a lambda expression.
data AnonymousFunctionBody
    -- | A anonymous function body containing its statements.
    = AnonymousFunctionStatementBody  [Statement]
    -- | A anonymous function body containing its expression.
    | AnonymousFunctionExpressionBody Expression
    deriving (Show)
    
-- | An anonymous function parameter containing its modifier, type and identifier.
data AnonymousFunctionParameter
    = ExplicitAnonymousFunctionParameter (Maybe ParameterModifier) Type Identifier
    deriving (Show)

-- | A nameof entity appears within a nameof expression.
data NameofEntity
    -- | A nameof containing its identifier.
    = NameofIdentifier Identifier
    -- | A nameof this containing its identifier.
    | NameofThis Identifier
    -- | A nameof base containing its identifier.
    | NameofBase Identifier
    -- | A nameof containing its entity and identifier.
    | NameofEntity NameofEntity Identifier
    -- | A nameof containing its simple type and identifier.
    | NameofPredefinedType SimpleType Identifier
    deriving (Show)

-- | A typeof expression containing its type where 'Data.Maybe.Nothing' represents
-- void.
newtype TypeOfExpression = TypeofType (Maybe Type)
    deriving (Show)

-- | An array creation initializer appears withing an array creation expression.
data ArrayCreationInitializer
    -- | An array creation initializer containing its values.
    = ArrayCreationInitializerExpression   [Expression]
    -- | An array creation initializer containing its nested initializers.
    | ArrayCreationInitializerInitializers [ArrayCreationInitializer]
    deriving (Show)

-- | An object creation initializer appears within an object creation expression.
data ObjectCreationInitializer
    -- | An object initializer containing its member initializers.
    = ObjectInitializer     [MemberInitializer]
    -- | A collection initializer containing its initializers.
    | CollectionInitializer ArrayCreationInitializer
    deriving (Show)

-- | A member initializer containing its target and value.
data MemberInitializer = MemberInitializer InitializerTarget InitializerValue
    deriving (Show)

-- | An initialization target appears within a member initializer.
data InitializerTarget
    -- | An initializer target containing its identifier.
    = InitializerTargetIdentifier Identifier
    -- | An initializer target containing its values.
    | InitializerTargetList       [Argument]
    deriving (Show)

-- | An initialization value appears within a member initializer.
data InitializerValue
    -- | An initializer value containing its value.
    = InitializerValueExpression  Expression
    -- | An initializer value containing its nested values.
    | InitializerValueInitializer ObjectCreationInitializer
    deriving (Show)

-- | A member access appearing within an member access expression.
data MemberAccess
    -- | A member access containing its expression, identifier and type arguments.
    = PrimaryMemberAccess Expression Identifier [TypeArgument]
    -- | A member access containing its simple type, identifier and type arguments.
    | PredefinedMemberAccess SimpleType Identifier [TypeArgument]
    -- | A member access containing its namespace, identifier, and identifier.
    | QualifiedMemberAccess Identifier Identifier Identifier
    deriving (Show)

-- | A literal.
data Literal
    = BooleanLit            Bool
    | IntLit                Integer
    | UIntLit               Integer
    | LongLit               Integer
    | ULongLit              Integer
    | FloatLit              Double
    | DoubleLit             Double
    | DecimalLit            Double
    | CharLit               String
    | StringLit             String
--    | InterpolatedStringLit String
    | VerbatimStringLit     String
    | NullLit
    deriving (Show)

-- | An assignment operator.
data AssignmentOperator
    = OpAssign
    | OpAssignPlus
    | OpAssignMinus
    | OpAssignMultiply
    | OpAssignDivide
    | OpAssignModulo
    | OpAssignBitwiseAnd
    | OpAssignBitwiseOr
    | OpAssignBitwiseXor
    | OpAssignBitwiseLeftShift
    | OpAssignBitwiseRightShift
    deriving (Show)

-- | A binary operator.
data BinaryOperator
    = BinaryPlus
    | BinaryMinus
    | BinaryMultiply
    | BinaryDivide
    | BinaryModulo
    | BinaryShiftLeft
    | BinaryShiftRight
    | BinaryEquals
    | BinaryNotEquals
    | BinaryLessThan
    | BinaryLessThanEqual
    | BinaryGreaterThan
    | BinaryGreaterThanEqual
    | BinaryBitwiseAnd
    | BinaryBitwiseXor
    | BinaryBitwiseOr
    | BinaryAnd
    | BinaryOr
    | BinaryIs
    | BinaryAs
    | BinaryNullCoalescing
    deriving (Show)

--------------------------------------------------------------------------------
-- Type parameters.
--------------------------------------------------------------------------------

-- | A type parameter containing its identifier.
newtype TypeParameter = TypeParameter Identifier
    deriving (Show)

-- | A type parameter constraint clause containing its type parameter and constraints.
data TypeParameterConstraintClause
    = TypeParameterConstraintClause TypeParameter [TypeParameterConstraint]
    deriving (Show)

-- | A variant type parameter containing its variance and identifier.
data VariantTypeParameter 
    = VariantTypeParameter (Maybe Variance) Identifier
    deriving (Show)

-- | A variance appears within a variant type parameter.
data Variance = VarianceIn | VarianceOut
    deriving (Show)

-- | A type parameter constraint appears within a type parameter constraints clause.
data TypeParameterConstraint
    -- | A type constraint containing its type.
    = TypeConstraint Type
    -- | A class constraint.
    | ClassConstraint
    -- | A struct constraint.
    | StructConstraint
    -- | A new() constraint.
    | NewConstraint
    deriving (Show)

-- | A type argument containing its type.
newtype TypeArgument = TypeArgument Type
    deriving (Show)
    
--------------------------------------------------------------------------------
-- Type definitions.
--------------------------------------------------------------------------------

-- | A type.
data Type 
    -- | A named type containing its name.
    = TypeNamed    TypeName
    -- | An array type containing its array type.
    | TypeArray    ArrayType
    -- | A simple type containing its simple type.
    | TypeSimple   SimpleType
    -- | A dynamic type.
    | TypeDynamic
    -- | A nullable type containing its type.
    | TypeNullable Type
    deriving (Show)

-- | A simple type can be one of the standard types.
data SimpleType 
    = IntegralType IntegralType | FloatingPointType FloatingPointType
    | Char | Bool | Object | String
    deriving (Show)

-- | An intergral type.
data IntegralType
    = SByte | Byte | Short | UShort | Int | UInt | Long | ULong
    deriving (Show)

-- | A floating point type.
data FloatingPointType = Float | Double | Decimal
    deriving (Show)

-- | An array type containing its base type and rank specifiers.
data ArrayType
    = ArrayType Type [RankSpecifier]
    deriving (Show)

-- | A rank specifier containing the number of ranks.
newtype RankSpecifier = RankSpecifier Int
    deriving (Show)

-- | A type name. 
data TypeName
    -- | A type name containing its name and type arguments.
    = TypeName  Name [TypeArgument]
    -- | A type alias containing its namespace, identifier and type arguments.
    | TypeAlias Identifier Identifier [TypeArgument]
    deriving (Show)

--------------------------------------------------------------------------------
-- Attribute definitions.
--------------------------------------------------------------------------------

-- | An attribute section containing the attribute target and attributes.
data AttributeSection = AttributeSection (Maybe AttributeTarget) [Attribute]
    deriving (Show)

-- | A global attribute section containing the attribute target and attributes.
data GlobalAttributeSection 
    = GlobalAttributeSection (Maybe GlobalAttributeTarget) [Attribute]
    deriving (Show)

-- | A global attribute target appears within a global attribute section.
data GlobalAttributeTarget
    = AttributeTargetAssembly
    | AttributeTargetModule
    deriving (Show)

-- | An attribute target appears within a attribute section.
data AttributeTarget
    = AttributeTargetField
    | AttributeTargetEvent
    | AttributeTargetMethod
    | AttributeTargetParam
    | AttributeTargetProperty
    | AttributeTargetReturn
    | AttributeTargetType
    deriving (Show)

-- | An attribute containing its type name and attribute arguments.
data Attribute = Attribute TypeName [AttributeArgument]
    deriving (Show)

-- | An attribute argument appears within an attribute.
data AttributeArgument
    -- | An expression attribute argument containing its expression.
    = AttributeArgumentExpression Expression
    -- | An named attribute argument containing its name and expression.
    | AttributeArgumentNamed Identifier Expression
    deriving (Show)

--------------------------------------------------------------------------------
-- Auxiliary definitions.
--------------------------------------------------------------------------------
        
-- | A modifier specifying properties of declarations on different levels.
data Modifier
    = Public
    | Private
    | Internal
    | Protected
    | Abstract
    | Async
    | Const
    | Event
    | Extern
    | New
    | Override
    | Readonly
    | Sealed
    | Static
    | Unsafe
    | Virtual
    | Volatile
    | Partial
    deriving (Show)

-- | An identifier.
newtype Identifier = Identifier String
    deriving (Show)

-- | A name is a sequence of identifiers chained with periods.
newtype Name = Name [Identifier]
    deriving (Show)