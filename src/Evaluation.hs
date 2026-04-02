module Evaluation where

import           Helper
import           Syntax

data EvalStrat
  = NormalOrder
  | ApplicativeOrder

instance Show (EvalStrat) where
  show NormalOrder      = "normal order reduction"
  show ApplicativeOrder = "applicative order reduction"

type EvalFun = TermNode -> Either TermNode TermNode

hasRedex :: TermNode -> Bool
hasRedex t =
  case getTm t of
    TmApp (TermNode _ (TmAbs _ _)) _ -> True
    _                                -> False

evalAppOrder1 :: EvalFun
evalAppOrder1 t =
  applyCommonEither (TermNode (getFI t)) $
    case getTm t of
      TmApp t1 t2 ->
        case tryEvalTwo evalAppOrder1 (t1, t2) of
          Left _           -> applyCommonEither getTm (trySubst t)
          Right (t1', t2') -> Right (TmApp t1' t2')
      TmAbs x t1 -> applyCommonEither (TmAbs x) (evalAppOrder1 t1)
      t' -> Left t'

evalNormalOrder1 :: EvalFun
evalNormalOrder1 t =
  applyCommonEither (TermNode (getFI t)) $
    case tm of
      TmApp t1 t2 ->
        case trySubst t of
          Left _ ->
            case tryEvalTwo evalNormalOrder1 (t1, t2) of
              Left _           -> Left tm
              Right (t1', t2') -> Right (TmApp t1' t2')
          Right t' -> Right (getTm t')
      TmAbs x t1 -> applyCommonEither (TmAbs x) (evalNormalOrder1 t1)
      t' -> Left t'
  where
    tm = getTm t

applyCommonEither :: (t1 -> t2) -> Either t1 t1 -> Either t2 t2
applyCommonEither f e = case e of Left t -> Left $ f t; Right t -> Right $ f t

trySubst :: TermNode -> Either TermNode TermNode
trySubst t =
  case getTm t of
    TmApp (TermNode _ (TmAbs _ t11)) t2 -> Right (evalSubst t2 t11)
    _                                   -> Left t

tryEvalTwo :: EvalFun -> (TermNode, TermNode) -> Either () (TermNode, TermNode)
tryEvalTwo f (t1, t2) =
  case f t1 of
    Left _ ->
      case f t2 of
        Left _    -> Left ()
        Right t2' -> Right (t1, t2')
    Right t1' -> Right (t1', t2)

eval :: EvalStrat -> TermNode -> TermNode
eval evalStrat t = eval' (getEvalFun evalStrat) t

eval' :: EvalFun -> TermNode -> TermNode
eval' evalFun t =
  case evalFun t of
    Left _  -> t
    Right r -> eval' evalFun r

getEvalFun :: EvalStrat -> EvalFun
getEvalFun ApplicativeOrder = evalAppOrder1
getEvalFun NormalOrder      = evalNormalOrder1
