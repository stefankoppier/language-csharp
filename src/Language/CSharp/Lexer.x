{
{-|
Module      : Language.CSharp.Lexer
Description : Lexing of C#

This module containg the lexing of C# resulting in the list of tokens with their
position in the original source file.
-}
module Language.CSharp.Lexer (lexer, Positioned(..), Token(..), AlexPosn(..)) where

import qualified Data.Text      as T  
import           Data.Text.Read
}

%wrapper "posn"

$any_character     = [\x00-\x10ffff]
$digit             = [0-9]
$hexdigit          = [0-9A-Fa-f]
$alpha             = [a-zA-Z]	
$newline_character = [\n\f\v\r]

--------------------------------------------------------------------------------
-- Comments.
--------------------------------------------------------------------------------

@not_slash_or_asterisk     = [.$white] # [\/\*]
@single_line_comment       = "//".*
@delimited_comment_section = "/" | (\* * @not_slash_or_asterisk)
@delimited_comment         = "/*" @delimited_comment_section* "*"+ "/"
@comment                   = @single_line_comment | @delimited_comment

--------------------------------------------------------------------------------
-- Character literals.
--------------------------------------------------------------------------------

@single_character            = (. # [\' \\ $newline_character])
@simple_escape_sequence      = \\\' | \\" | \\\\ | \\0 | \\a | \\b | \\f | \\n | \\r | \\t | \\v
@hexadecimal_escape_sequence = \\x $hexdigit $hexdigit? $hexdigit? $hexdigit?
@unicode_escape_sequence     = \\u $hexdigit $hexdigit $hexdigit $hexdigit 
                             | \\U  $hexdigit $hexdigit $hexdigit $hexdigit  $hexdigit $hexdigit $hexdigit $hexdigit
@character_literal           = "'" ( @single_character
                                   | @simple_escape_sequence
                                   | @hexadecimal_escape_sequence
                                   | @unicode_escape_sequence) "'"

--------------------------------------------------------------------------------
-- Real literals.
--------------------------------------------------------------------------------

$real_suffix   = [FfDdMm]
@real_exponent = (e|E) (\+|\-)? $digit+
@real_a        = $digit+ \. $digit+ @real_exponent? $real_suffix?
@real_b        = \. $digit+ @real_exponent? $real_suffix?
@real_c        = $digit+ @real_exponent $real_suffix?
@real_d        = $digit+ $real_suffix

@real_literal  = @real_a | @real_b | @real_c | @real_d

--------------------------------------------------------------------------------
-- String literals.
--------------------------------------------------------------------------------

@single_regular_string_literal_char = (. # [\" $newline_character])
@regular_string_literal_char = @single_regular_string_literal_char
                             | @simple_escape_sequence
                             | @hexadecimal_escape_sequence
                             | @unicode_escape_sequence
@string_literal              = \" @regular_string_literal_char* \"

@verbatim_string_literal = \@\" (([.\n] # \") | \"\")* \"

tokens :-
    @verbatim_string_literal 
        { \ p x -> Positioned p (TVerbatimStringLiteral (tail (tail (init x)))) }
    @string_literal 
        { \ p x -> Positioned p (TStringLiteral (tail (init x))) }

    $white+     ;
    @comment    ;

    "abstract"          { \ p _ -> Positioned p TKWabstract     }
    "as"                { \ p _ -> Positioned p TKWas           }
    "base"              { \ p _ -> Positioned p TKWbase         }
    "bool"              { \ p _ -> Positioned p TKWbool         }
    "break"             { \ p _ -> Positioned p TKWbreak        }
    "byte"              { \ p _ -> Positioned p TKWbyte         }
    "case"              { \ p _ -> Positioned p TKWcase         }
    "catch"             { \ p _ -> Positioned p TKWcatch        }
    "char"              { \ p _ -> Positioned p TKWchar         }
    "checked"           { \ p _ -> Positioned p TKWchecked      }
    "class"             { \ p _ -> Positioned p TKWclass        }
    "const"             { \ p _ -> Positioned p TKWconst        }
    "continue"          { \ p _ -> Positioned p TKWcontinue     }
    "decimal"           { \ p _ -> Positioned p TKWdecimal      }
    "default"           { \ p _ -> Positioned p TKWdefault      }
    "delegate"          { \ p _ -> Positioned p TKWdelegate     }
    "do"                { \ p _ -> Positioned p TKWdo           }
    "double"            { \ p _ -> Positioned p TKWdouble       }
    "else"              { \ p _ -> Positioned p TKWelse         }
    "enum"              { \ p _ -> Positioned p TKWenum         }
    "event"             { \ p _ -> Positioned p TKWevent        }
    "explicit"          { \ p _ -> Positioned p TKWexplicit     }
    "extern"            { \ p _ -> Positioned p TKWextern       }
    "false"             { \ p _ -> Positioned p TKWfalse        }
    "finally"           { \ p _ -> Positioned p TKWfinally      }
    "fixed"             { \ p _ -> Positioned p TKWfixed        }
    "float"             { \ p _ -> Positioned p TKWfloat        }
    "for"               { \ p _ -> Positioned p TKWfor          }
    "foreach"           { \ p _ -> Positioned p TKWforeach      }
    "goto"              { \ p _ -> Positioned p TKWgoto         }
    "if"                { \ p _ -> Positioned p TKWif           }
    "implicit"          { \ p _ -> Positioned p TKWimplicit     }
    "in"                { \ p _ -> Positioned p TKWin           }
    "int"               { \ p _ -> Positioned p TKWint          }
    "interface"         { \ p _ -> Positioned p TKWinterface    }
    "internal"          { \ p _ -> Positioned p TKWinternal     }
    "is"                { \ p _ -> Positioned p TKWis           }
    "lock"              { \ p _ -> Positioned p TKWlock         }
    "long"              { \ p _ -> Positioned p TKWlong         }
    "namespace"         { \ p _ -> Positioned p TKWnamespace    }
    "new"               { \ p _ -> Positioned p TKWnew          }
    "null"              { \ p _ -> Positioned p TKWnull         }
    "object"            { \ p _ -> Positioned p TKWobject       }
    "operator"          { \ p _ -> Positioned p TKWoperator     }
    "out"               { \ p _ -> Positioned p TKWout          }
    "override"          { \ p _ -> Positioned p TKWoverride     }
    "params"            { \ p _ -> Positioned p TKWparams       }
    "private"           { \ p _ -> Positioned p TKWprivate      }
    "protected"         { \ p _ -> Positioned p TKWprotected    }
    "public"            { \ p _ -> Positioned p TKWpublic       }
    "readonly"          { \ p _ -> Positioned p TKWreadonly     }
    "ref"               { \ p _ -> Positioned p TKWref          }
    "return"            { \ p _ -> Positioned p TKWreturn       }
    "byte"              { \ p _ -> Positioned p TKWsbyte        }
    "sealed"            { \ p _ -> Positioned p TKWsealed       }
    "short"             { \ p _ -> Positioned p TKWshort        }
    "sizeof"            { \ p _ -> Positioned p TKWsizeof       }
    "stackalloc"        { \ p _ -> Positioned p TKWstackalloc   }
    "static"            { \ p _ -> Positioned p TKWstatic       }
    "string"            { \ p _ -> Positioned p TKWstring       }
    "struct"            { \ p _ -> Positioned p TKWstruct       }
    "switch"            { \ p _ -> Positioned p TKWswitch       }
    "this"              { \ p _ -> Positioned p TKWthis         }
    "throw"             { \ p _ -> Positioned p TKWthrow        }
    "true"              { \ p _ -> Positioned p TKWtrue         }
    "try"               { \ p _ -> Positioned p TKWtry          }
    "typeof"            { \ p _ -> Positioned p TKWtypeof       }
    "uint"              { \ p _ -> Positioned p TKWuint         }
    "ulong"             { \ p _ -> Positioned p TKWulong        }
    "unchecked"         { \ p _ -> Positioned p TKWunchecked    }
    "unsafe"            { \ p _ -> Positioned p TKWunsafe       }
    "ushort"            { \ p _ -> Positioned p TKWushort       }
    "using"             { \ p _ -> Positioned p TKWusing        }
    "virtual"           { \ p _ -> Positioned p TKWvirtual      }
    "void"              { \ p _ -> Positioned p TKWvoid         }
    "volatile"          { \ p _ -> Positioned p TKWvolatile     }
    "while"             { \ p _ -> Positioned p TKWwhile        }
    
    "await"             { \ p _ -> Positioned p TOpAwait        }

    @character_literal { \ p x -> Positioned p (TCharLiteral (tail (init x))) }

    @real_literal { \ p x -> Positioned p (realLiteral x) }
        
    [$digit]+ | ("0x" | "0X")[$hexdigit]+
        { \ p x -> Positioned p (intLiteral x) }
        
    [$digit]+("u"|"U") | ("0x" | "0X")[$hexdigit]+("u"|"U")      
        { \ p x -> Positioned p (uIntLiteral x) }
        
    [$digit]+("L"|"l") | ("0x" | "0X")[$hexdigit]+("L"|"l")
        { \ p x -> Positioned p (longLiteral x) }
        
    [$digit]+("UL"|"Ul"|"uL"|"ul"|"LU"|"Lu"|"lU"|"lu") | ("0x" | "0X")[$hexdigit]+("UL"|"Ul"|"uL"|"ul"|"LU"|"Lu"|"lU"|"lu")                 
        { \ p x -> Positioned p (uLongLiteral x) }
    
    "??"                { \ p _ -> Positioned p TOpNullCoalescing }

    "("                 { \ p _ -> Positioned p TOParens     }
    ")"                 { \ p _ -> Positioned p TCParens     }
    "["                 { \ p _ -> Positioned p TOSquare     }
    "]"                 { \ p _ -> Positioned p TCSquare     }
    "{"                 { \ p _ -> Positioned p TOCurly      }
    "}"                 { \ p _ -> Positioned p TCCurly      }
    ";"                 { \ p _ -> Positioned p TSemicolon   }
    "::"                { \ p _ -> Positioned p TDoubleColon }
    ":"                 { \ p _ -> Positioned p TColon       }
    ","                 { \ p _ -> Positioned p TComma       }
    "."                 { \ p _ -> Positioned p TPeriod      }
    "?"                 { \ p _ -> Positioned p TQuestion    }
    "=>"                { \ p _ -> Positioned p TLambda      }

    "+="    { \ p _ -> Positioned p TOpAssignPlus                }
    "-="    { \ p _ -> Positioned p TOpAssignMinus               }
    "*="    { \ p _ -> Positioned p TOpAssignMultiply            }
    "/="    { \ p _ -> Positioned p TOpAssignDivide              }
    "%="    { \ p _ -> Positioned p TOpAssignModulo              }
    "&="    { \ p _ -> Positioned p TOpAssignBitwiseAnd          }
    "|="    { \ p _ -> Positioned p TOpAssignBitwiseOr           }
    "^="    { \ p _ -> Positioned p TOpAssignBitwiseXor          }
    "<<="   { \ p _ -> Positioned p TOpAssignBitwiseLeftShift    }
    ">>="   { \ p _ -> Positioned p TOpAssignBitwiseRightShift   }
    
    "++"    { \ p _ -> Positioned p TOpPlusPlus           }
    "--"    { \ p _ -> Positioned p TOpMinusMinus         }
    "+"     { \ p _ -> Positioned p TOpPlus               }
    "-"     { \ p _ -> Positioned p TOpMinus              }
    "*"     { \ p _ -> Positioned p TOpMultiply           }
    "/"     { \ p _ -> Positioned p TOpDivide             }
    "%"     { \ p _ -> Positioned p TOpModulo             }
    "&&"    { \ p _ -> Positioned p TOpAnd                }
    "||"    { \ p _ -> Positioned p TOpOr                 }
    "&"     { \ p _ -> Positioned p TOpBitwiseAnd         }
    "|"     { \ p _ -> Positioned p TOpBitwiseOr          }
    "^"     { \ p _ -> Positioned p TOpBitwiseXor         }
    "<<"    { \ p _ -> Positioned p TOpLeftShift          }
    ">>"    { \ p _ -> Positioned p TOpRightShift         }
    "=="    { \ p _ -> Positioned p TOpEqual              }
    "!="    { \ p _ -> Positioned p TOpNotEqual           }
    ">="    { \ p _ -> Positioned p TOpGreaterThanEqual   }
    "<="    { \ p _ -> Positioned p TOpLessThanEqual      }
    ">"     { \ p _ -> Positioned p TOpGreaterThan        }
    "<"     { \ p _ -> Positioned p TOpLessThan           }
    "!"     { \ p _ -> Positioned p TOpNot                }
    "~"     { \ p _ -> Positioned p TOpBitwiseNot         }
    "="     { \ p _ -> Positioned p TOpAssign             }

    [$alpha \_] [$alpha $digit \_]* { \ p s -> Positioned p (TIdentifier s) }

{
data Positioned a = Positioned AlexPosn a
                  deriving (Show)  

fromRight :: Either a b -> b
fromRight (Right value) = value
fromRight (Left _)      = error "fromRight of a Left"

parseIntegral :: String -> Integer
parseIntegral xs = fst $ fromRight $ 
    case take 2 xs of
        "0x" -> hexadecimal (T.pack xs); "0X" -> hexadecimal (T.pack xs)
        _    -> decimal (T.pack xs)

parseReal :: String -> Double
parseReal = fst . fromRight . rational . T.pack

realLiteral :: String -> Token
realLiteral xs
    | suffix `elem` "Ff"
        = TFloatLiteral $ parseReal ('0' : prefix)
    | suffix `elem` "Dd"
        = TDoubleLiteral $ parseReal ('0' : prefix)
    | suffix `elem` "Mm"
        = TDecimalLiteral $ parseReal ('0' : prefix)
    | otherwise
        = TDoubleLiteral $ parseReal ('0' : xs)
    where prefix = init xs
          suffix = last xs

intLiteral :: String -> Token
intLiteral xs
    | n <= 2147483647           = TIntLiteral n
    | n <= 4294967295           = TUIntLiteral n
    | n <= 9223372036854775807  = TLongLiteral n
    | n <= 18446744073709551615 = TULongLiteral n
    | otherwise                 = error "integer literal too big"
    where n = parseIntegral xs

uIntLiteral :: String -> Token
uIntLiteral xs
    | n <= 4294967295           = TUIntLiteral n
    | n <= 18446744073709551615 = TULongLiteral n
    | otherwise                 = error "integer literal too big"
    where n = parseIntegral xs

longLiteral :: String -> Token
longLiteral xs
    | n <= 9223372036854775807  = TLongLiteral n
    | n <= 18446744073709551615 = TULongLiteral n
    | otherwise                 = error "integer literal too big"
    where n = parseIntegral xs

uLongLiteral  :: String -> Token
uLongLiteral xs
    | n <= 18446744073709551615 = TULongLiteral n
    | otherwise                 = error "integer literal too big"
    where n = parseIntegral xs

data Token
    = TKWabstract
    | TKWas
    | TKWbase
    | TKWbool
    | TKWbreak
    | TKWbyte
    | TKWcase
    | TKWcatch
    | TKWchar
    | TKWchecked
    | TKWclass
    | TKWconst
    | TKWcontinue
    | TKWdecimal
    | TKWdefault
    | TKWdelegate
    | TKWdo
    | TKWdouble
    | TKWelse
    | TKWenum
    | TKWevent
    | TKWexplicit
    | TKWextern
    | TKWfalse
    | TKWfinally
    | TKWfixed
    | TKWfloat
    | TKWfor
    | TKWforeach
    | TKWgoto
    | TKWif
    | TKWimplicit
    | TKWin
    | TKWint
    | TKWinterface
    | TKWinternal
    | TKWis
    | TKWlock
    | TKWlong
    | TKWnamespace
    | TKWnew
    | TKWnull
    | TKWobject
    | TKWoperator
    | TKWout
    | TKWoverride
    | TKWparams
    | TKWprivate
    | TKWprotected
    | TKWpublic
    | TKWreadonly
    | TKWref
    | TKWreturn
    | TKWsbyte
    | TKWsealed
    | TKWshort
    | TKWsizeof
    | TKWstackalloc
    | TKWstatic
    | TKWstring
    | TKWstruct
    | TKWswitch
    | TKWthis
    | TKWthrow
    | TKWtrue
    | TKWtry
    | TKWtypeof
    | TKWuint
    | TKWulong
    | TKWunchecked
    | TKWunsafe
    | TKWushort
    | TKWusing
    | TKWvirtual
    | TKWvoid
    | TKWvolatile
    | TKWwhile

    | TOParens 
    | TCParens
    | TOSquare 
    | TCSquare
    | TOCurly  
    | TCCurly

    | TSemicolon
    | TDoubleColon
    | TColon
    | TComma
    | TPeriod
    | TQuestion
    | TLambda

    | TOpPlus
    | TOpMinus
    | TOpMultiply
    | TOpDivide
    | TOpModulo
    | TOpBitwiseAnd
    | TOpBitwiseOr
    | TOpBitwiseXor
    | TOpLeftShift
    | TOpRightShift
    | TOpEqual
    | TOpNotEqual
    | TOpGreaterThan
    | TOpLessThan
    | TOpGreaterThanEqual
    | TOpLessThanEqual
    | TOpAnd
    | TOpOr
    | TOpNot
    | TOpBitwiseNot
    | TOpPlusPlus
    | TOpMinusMinus
    | TOpNullCoalescing
    | TOpAwait

    | TOpAssign
    | TOpAssignPlus
    | TOpAssignMinus
    | TOpAssignMultiply
    | TOpAssignDivide
    | TOpAssignModulo
    | TOpAssignBitwiseAnd
    | TOpAssignBitwiseOr
    | TOpAssignBitwiseXor
    | TOpAssignBitwiseLeftShift
    | TOpAssignBitwiseRightShift

    | TCharLiteral               String
    | TStringLiteral             String
    | TVerbatimStringLiteral     String
    | TInterpolatedStringLiteral String
    | TFloatLiteral              Double
    | TDoubleLiteral             Double
    | TDecimalLiteral            Double
    | TIntLiteral                Integer
    | TUIntLiteral               Integer
    | TLongLiteral               Integer
    | TULongLiteral              Integer

    | TIdentifier String
    deriving (Show, Eq)

-- | Lexes the given string and returns the list of tokens with their position
-- in the original source file. When the lexing fails an exception will be thrown.
lexer :: String -> [Positioned Token]
lexer = alexScanTokens
}