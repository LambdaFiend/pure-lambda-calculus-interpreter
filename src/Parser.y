{
module Parser where

import Lexer
import Syntax
}

%monad { Either String } { (>>=) } { return }
%name parser
%tokentype { Token }
%error { parseError }

%token

"\\" { Token pos LAMBDA }
"."  { Token pos DOT }
"("  { Token pos LPAREN }
")"  { Token pos RPAREN }
id   { Token pos (ID s) }

%%

Term
  : "\\" Abs { $2 }
  | App      { $1 }

Abs
  : Name "." Term { ExtTermNode (fst $1) $ ExtTmAbs (snd $1) $3 }
  | Name Abs      { ExtTermNode (fst $1) $ ExtTmAbs (snd $1) $2 }    

App
  : App Atom  { ExtTermNode (getExtFI $1) $ ExtTmApp $1 $2 }
  | Atom      { $1 }

Atom
  : Var          { $1 }
  | "(" Term ")" { $2 }

Var : Name { ExtTermNode (fst $1) $ ExtTmVar (snd $1) }

Name : id { (tokenPos $1, (\(ID s) -> s) (tokenDat $1)) }

{
parseError :: [Token] -> Either String a
parseError []                    = Left ("Parsing error near the end of the file")
parseError ((Token fi _):tokens) = Left ("Parsing error at:" ++ showFileInfoHappy fi)
parseError (x:xs)                = Left "Parsing error"

showFileInfoHappy :: AlexPosn -> String
showFileInfoHappy (AlexPn p l c) =
  "\n" ++ "Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c
}
