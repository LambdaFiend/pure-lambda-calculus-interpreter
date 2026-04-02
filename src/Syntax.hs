module Syntax where

import           Data.List
import           Lexer

type Index = Int

type Name = String

type FileInfo = AlexPosn

type Context = [Name]

data ExtTermNode = ExtTermNode
  { getExtFI :: FileInfo,
    getExtTm :: ExtTerm
  }
  deriving (Eq, Show)

data ExtTerm
  = ExtTmVar Name
  | ExtTmAbs Name ExtTermNode
  | ExtTmApp ExtTermNode ExtTermNode
  deriving (Eq, Show)

data TermNode = TermNode
  { getFI :: FileInfo,
    getTm :: Term
  }

data VarStatus
  = Bound Index Index
  | Free
  deriving (Eq, Show)

data Term
  = TmVar VarStatus Name
  | TmAbs Name TermNode
  | TmApp TermNode TermNode

instance Show (TermNode) where
  show (TermNode _ t) = show t

instance Show (Term) where
  show (TmVar _ x) = x
  show (TmAbs x t1) = "(λ" ++ intercalate " " (x : xs) ++ "." ++ show t1' ++ ")"
    where
      (xs, t1') = collectCurry t1
  show (TmApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

instance Eq (TermNode) where
  (TermNode _ t1) == (TermNode _ t2) = t1 == t2

instance Eq (Term) where
  (TmApp t11 t12) == (TmApp t21 t22) = t11 == t21 && t12 == t22
  (TmAbs _ t1) == (TmAbs _ t2)       = t1 == t2
  (TmVar k1 _) == (TmVar k2 _)       = k1 == k2
  _ == _                             = False

collectCurry :: TermNode -> ([Name], TermNode)
collectCurry (TermNode _ (TmAbs x t1)) = (x : xs, t1')
  where
    (xs, t1') = collectCurry t1
collectCurry t = ([], t)
