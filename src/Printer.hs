module Printer where

import Definer
import Parser
import Reducer
import qualified Data.Monoid as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

unpackText :: T.Builder -> String
unpackText s = T.unpack $ T.toLazyText s

symbolBuild :: String -> T.Builder
symbolBuild "λ" = T.fromString "λ"
symbolBuild "(" = T.fromString "("
symbolBuild ")" = T.fromString ")"
symbolBuild "." = T.fromString "."
symbolBuild " " = T.fromString " "

-- shows all parentheses;
showTermFullParens :: Term -> T.Builder
showTermFullParens (Variable name index) = T.fromString name
showTermFullParens (Abstraction term1 term2) =
  symbolBuild "("
    `T.mappend` symbolBuild "λ"
    `T.mappend` showTermFullParens term1
    `T.mappend` symbolBuild "."
    `T.mappend` showTermFullParens term2
    `T.mappend` symbolBuild ")"
showTermFullParens (Application term1@(Abstraction _ _) term2) =
  symbolBuild "("
    `T.mappend` showTermFullParens term1
    `T.mappend` symbolBuild ")"
    `T.mappend` symbolBuild " "
    `T.mappend` showTermFullParens term2
showTermFullParens (Application term1 term2) =
  symbolBuild "("
    `T.mappend` showTermFullParens term1
    `T.mappend` symbolBuild " "
    `T.mappend` showTermFullParens term2
    `T.mappend` symbolBuild ")"

-- by default, it uses syntax sugar for showing the term
showTerm :: Term -> String
showTerm term = unpackText $ showTermSyntaxSugarStart term

-- adds syntactic sugar and attempts to minimize parentheses
-- by only adding them around applications on the right and
-- abstractions which do not encompass the whole term
showTermSyntaxSugarStart :: Term -> T.Builder
showTermSyntaxSugarStart term@(Abstraction term1 term2) =
  symbolBuild "λ" `T.mappend` getSyntaxSugar term
showTermSyntaxSugarStart term = showTermSyntaxSugar term

showTermSyntaxSugar :: Term -> T.Builder
showTermSyntaxSugar (Variable name index) = T.fromString name
showTermSyntaxSugar term@(Abstraction term1 term2) =
  parensSyntaxSugar $ (symbolBuild "λ" `T.mappend` getSyntaxSugar term)
showTermSyntaxSugar (Application term1 term2@(Application _ _)) =
  showTermSyntaxSugar term1 `T.mappend` (parensSyntaxSugar $ showTermSyntaxSugar term2)
showTermSyntaxSugar term@(Application (Application _ _) _) = getSyntaxSugar term
showTermSyntaxSugar (Application term1 term2) = showTermSyntaxSugar term1 `T.mappend` showTermSyntaxSugar term2

parensSyntaxSugar :: T.Builder -> T.Builder
parensSyntaxSugar text =
  symbolBuild "("
    `T.mappend` text
    `T.mappend` symbolBuild ")"

getSyntaxSugar :: Term -> T.Builder
getSyntaxSugar (Abstraction (Variable name _) term2@(Abstraction _ _)) =
  T.fromString name `T.mappend` getSyntaxSugar term2
getSyntaxSugar (Abstraction (Variable name _) term2) =
  T.fromString name
    `T.mappend` symbolBuild "."
    `T.mappend` showTermSyntaxSugar term2
getSyntaxSugar (Application term1@(Application _ _) term2) =
  getSyntaxSugar term1 `T.mappend` showTermSyntaxSugar term2
getSyntaxSugar term@(Application _ _) = showTermSyntaxSugar term
