module Language.CSharp.Parser.Statement where

import Text.Parsec                       hiding (Empty)
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility
import Language.CSharp.Parser.Expression
import Language.CSharp.Parser.Type

pStatements :: P [Statement]
pStatements = many pStatement <?> "a statement"

pStatement :: P Statement
pStatement = choice 
    [ pIfThenElseStatement, pWhileStatement      , pDoStatement         
    , pForEachStatement   , pLockStatement       , pBlockStatement    
    , pLabeledStatement   , pEmptyStatement      , pBreakStatement      
    , pContinueStatement  , pReturnStatement     , pThrowStatement    
    , pYieldStatement     , pDeclarationStatement, pCheckedStatement    
    , pUncheckedStatement , pExpressionStatement , pUsingStatement
    , pSwitchStatement    , pGotoStatement       , pTry               
    , pForStatement       ]
    
pLabeledStatement :: P Statement
pLabeledStatement = try (Labeled <$> pIdentifier <* pColon <*> pStatement)

pDeclarationStatement :: P Statement
pDeclarationStatement = try (Declaration <$> pLocalVarDeclaration <* pSemi)

pLocalVarDeclaration :: P LocalVarDeclaration
pLocalVarDeclaration = 
    LocalVarDeclaration <$> pLocalVarType <*> sepBy1 pVariableDeclarator pComma

pLocalVarType :: P LocalVarType
pLocalVarType = VarType <$> pType <|> Var <$ pIdentifierKeyword "var" 

pVariableDeclarator :: P VariableDeclarator
pVariableDeclarator = do
    name        <- pIdentifier
    initializer <- optionMaybe (pEqualSign *> pVariableInitializer)
    return $ VariableDeclarator name initializer

pVariableInitializer :: P VariableInitializer
pVariableInitializer = 
    VariableInitializerExpression <$> pExpression       <|>
    VariableInitializerArray      <$> pArrayInitializer

pArrayInitializer :: P ArrayInitializer
pArrayInitializer = 
    ArrayInitializer <$> betweenCurly (sepBy pVariableInitializer pComma)

pBlockStatement :: P Statement
pBlockStatement = Block <$> betweenCurly pStatements

pEmptyStatement :: P Statement
pEmptyStatement = Empty <$ pSemi

pExpressionStatement :: P Statement
pExpressionStatement = ExpressionStatement <$> pExpression <* pSemi

pIfThenElseStatement :: P Statement
pIfThenElseStatement = do
    pToken TKWif
    guard     <- betweenParens pExpression
    trueBody  <- pStatement
    falseBody <- optionMaybe (pToken TKWelse *> pStatement)
    return $ IfThenElse guard trueBody falseBody

pWhileStatement :: P Statement
pWhileStatement = do
    pToken TKWwhile
    guard <- betweenParens pExpression
    body  <- pStatement
    return $ While guard body

pDoStatement :: P Statement
pDoStatement = do
    pToken TKWdo
    body <- pStatement
    pToken TKWwhile
    guard <- betweenParens pExpression <* pSemi
    return $ Do body guard

pForStatement :: P Statement
pForStatement = do
    pToken TKWfor
    pOParens
    initializer <- optionMaybe pForInitializer
    pSemi
    guard <- optionMaybe pExpression
    pSemi
    iterator <- optionMaybe (sepBy1 pExpression pComma)
    pCParens
    body <- pStatement
    return $ For initializer guard iterator body

pForInitializer :: P ForInitializer
pForInitializer = 
    ForInitializerDeclaration <$> pLocalVarDeclaration      <|>
    ForInitializerExpressions <$> sepBy1 pExpression pComma

pForEachStatement :: P Statement
pForEachStatement = do
    pToken TKWforeach
    pOParens
    ty <- pLocalVarType
    name <- pIdentifier
    pToken TKWin
    expr <- pExpression
    pCParens
    body <- pStatement
    return $ ForEach ty name expr body

pSwitchStatement :: P Statement
pSwitchStatement = do
    pToken TKWswitch
    expression <- betweenParens pExpression
    cases      <- betweenCurly (many (pSwitchCaseBlock <|> pSwitchDefaultBlock))
    return $ Switch expression cases

pSwitchCaseBlock :: P SwitchBlock
pSwitchCaseBlock = 
    LabeledBlock <$ pToken TKWcase <*> pExpression <* pColon <*> many pStatement
    
pSwitchDefaultBlock :: P SwitchBlock
pSwitchDefaultBlock = DefaultBlock <$ pToken TKWdefault <* pColon <*> many pStatement

pTry :: P Statement
pTry = do
    pToken TKWtry
    statements <- betweenCurly pStatements
    (catches, finally) <- choice 
        [ try ((,)   <$> many1 pCatch <* pToken TKWfinally <*> betweenCurly pStatements)
        , try ((,[]) <$> many1 pCatch)
        , try (([],) <$ pToken TKWfinally <*> betweenCurly pStatements) ]
    return $ Try statements catches finally

pCatch :: P Catch
pCatch = do
    pToken TKWcatch
    specifier  <- optionMaybe pExceptionSpecifier
    when       <- optionMaybe (pIdentifierKeyword "when" *> betweenParens pExpression)
    statements <- betweenCurly pStatements
    return $ Catch specifier when statements
    where
        pExceptionSpecifier = betweenParens (ExceptionSpecifier <$> pType <*> optionMaybe pIdentifier)

pGotoStatement :: P Statement
pGotoStatement = Goto <$> pGotoTarget <* pSemi

pGotoTarget :: P GotoTarget
pGotoTarget = choice
    [ try (GotoLabel <$ pToken TKWgoto <*> pIdentifier)
    , try (GotoCase <$ pToken TKWgoto <* pToken TKWcase <*> pExpression)
    , try (GotoDefault <$ pToken TKWgoto <* pToken TKWdefault) ]

pBreakStatement :: P Statement
pBreakStatement = Break <$ pToken TKWbreak <* pSemi

pContinueStatement :: P Statement
pContinueStatement = Continue <$ pToken TKWcontinue <* pSemi

pReturnStatement :: P Statement
pReturnStatement = Return <$ pToken TKWreturn <*> pMaybeExpression <* pSemi

pThrowStatement :: P Statement
pThrowStatement = Throw <$ pToken TKWthrow <*> pMaybeExpression <* pSemi

pLockStatement :: P Statement
pLockStatement = do
    pToken TKWlock
    expr <- betweenParens pExpression
    body <- pStatement
    return $ Lock expr body

pUsingStatement :: P Statement
pUsingStatement = do
    pToken TKWusing 
    resource <- betweenParens pResourceAquisition 
    body     <- pStatement
    return $ UsingStatement resource body

pResourceAquisition :: P ResourceAcquisition
pResourceAquisition = 
    ResourceAcquisitionVariable   <$> sepBy1 pVariableDeclarator pComma <|>
    ResourceAcquisitionExpression <$> pExpression             

pCheckedStatement :: P Statement
pCheckedStatement = try $
    CheckedStatement <$ pToken TKWchecked <*> betweenCurly pStatements
    
pUncheckedStatement :: P Statement
pUncheckedStatement = try $
    UncheckedStatement <$ pToken TKWunchecked <*> betweenCurly pStatements

pYieldStatement :: P Statement
pYieldStatement = pIdentifierKeyword "yield" *> (pYieldReturn <|> pYieldBreak)
    where
        pYieldReturn = Yield . Just  <$ pToken TKWreturn <*> pExpression <* pSemi
        pYieldBreak  = Yield Nothing <$ pToken TKWbreak <* pSemi

