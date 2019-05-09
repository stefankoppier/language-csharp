{-|
Module      : Language.CSharp.Parser
Description : Parsing of C#

This module containg the parsing of C# resulting in the abstract syntax tree
defined in "Language.CSharp.Syntax". This module exports all other specific parsing
modules.
-}

module Language.CSharp.Parser(
      parser
    , pCompilationUnit
    , pUsing
    , module Language.CSharp.Parser.Utility
    , module Language.CSharp.Parser.Declaration
    , module Language.CSharp.Parser.Statement
    , module Language.CSharp.Parser.Expression
    , module Language.CSharp.Parser.Type
    , module Language.CSharp.Parser.Attribute
) where

import Text.Parsec                       hiding (Empty)
import Language.CSharp.Lexer
import Language.CSharp.Syntax
import Language.CSharp.Parser.Utility
import Language.CSharp.Parser.Declaration
import Language.CSharp.Parser.Statement
import Language.CSharp.Parser.Expression
import Language.CSharp.Parser.Type
import Language.CSharp.Parser.Attribute

-- | Parses the given list of tokens and returns either a parsing error or
-- the abstract syntax tree.
parser :: String -- ^ The original source file name.
       -> [Positioned Token]  -- ^ The list of tokens resulting from the lexer.
       -> Either ParseError CompilationUnit
parser = parse pCompilationUnit

--------------------------------------------------------------------------------
-- Compilation Unit and using.
--------------------------------------------------------------------------------

pCompilationUnit :: P CompilationUnit
pCompilationUnit = CompilationUnit <$> many pUsing <*> many pDeclaration <* eof

pUsing :: P Using
pUsing = do
    pToken TKWusing
    isStatic <- option False (True <$ pToken TKWstatic)
    name <- pName
    pSemi
    return $ Using name isStatic 
