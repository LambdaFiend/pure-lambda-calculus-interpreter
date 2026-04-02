module Helper where

import           Debug.Trace
import           Syntax

tmMap :: (Term -> Term) -> TermNode -> TermNode
tmMap f t =
  TermNode (getFI t) $
    case f (getTm t) of
      TmVar s x   -> TmVar s x
      TmAbs x t1  -> TmAbs x (tmMap' f t1)
      TmApp t1 t2 -> TmApp (tmMap' f t1) (tmMap' f t2)
  where
    tmMap' = case getTm t of TmVar _ _ -> (\_ t1 -> t1); _ -> tmMap

ctxTmMap :: a -> (a -> TermNode -> (TermNode, a)) -> TermNode -> TermNode
ctxTmMap ctx f t =
  TermNode (getFI t) $
    case applyFst getTm (f ctx t) of
      (TmVar s x, _)      -> TmVar s x
      (TmAbs x t1, ctx')  -> TmAbs x (ctxTmMap' ctx' f t1)
      (TmApp t1 t2, ctx') -> TmApp (ctxTmMap' ctx' f t1) (ctxTmMap' ctx' f t2)
  where
    ctxTmMap' = case getTm t of TmVar _ _ -> (\_ _ t1 -> t1); _ -> ctxTmMap

applyFst :: (a -> c) -> (a, b) -> (c, b)
applyFst f (a, b) = (f a, b)

evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift 0 (-1) (subst 0 0 (shift 0 1 s) t)

shift :: Index -> Index -> TermNode -> TermNode
shift c d t = ctxTmMap (c, d) shift' t

shift' :: (Index, Index) -> TermNode -> (TermNode, (Index, Index))
shift' ctx@(c, d) (TermNode fi tm) =
  applyFst (TermNode fi) $
    case tm of
      TmVar (Bound k l) x -> (TmVar (Bound (if k < c then k else k + d) (l + d)) x, ctx)
      TmAbs _ _ -> (tm, (c + 1, d))
      _ -> (tm, ctx)

subst :: Index -> Index -> TermNode -> TermNode -> TermNode
subst c j s t = ctxTmMap (c, j, s) subst' t

subst' :: (Index, Index, TermNode) -> TermNode -> (TermNode, (Index, Index, TermNode))
subst' ctx@(c, j, s) t =
  case getTm t of
    TmVar (Bound k _) _ -> (if k == c + j then shift 0 (c + j) s else t, ctx)
    TmAbs _ _           -> (t, (c + 1, j, s))
    _                   -> (t, ctx)

genIndex :: ExtTermNode -> TermNode
genIndex t = genIndex' [] t

genIndex' :: Context -> ExtTermNode -> TermNode
genIndex' ctx t =
  TermNode (getExtFI t) $
    case getExtTm t of
      ExtTmVar x | elem x ctx -> TmVar (Bound (length $ takeWhile (x /=) ctx) (length ctx)) x
      ExtTmVar x -> TmVar Free x
      ExtTmAbs x t1 -> TmAbs x (genIndex' (x : ctx) t1)
      ExtTmApp t1 t2 -> TmApp (genIndex' ctx t1) (genIndex' ctx t2)

areIndexesBroken :: TermNode -> Bool
areIndexesBroken t = areIndexesBroken' [] 0 t

areIndexesBroken' :: Context -> Index -> TermNode -> Bool
areIndexesBroken' ctx n t =
  case getTm t of
    TmVar (Bound k l) x -> length (takeWhile (x /=) ctx) /= k || length ctx /= l
    TmAbs x t1 -> areIndexesBroken' (x : ctx) (n + 1) t1
    TmApp t1 t2 -> areIndexesBroken' ctx n t1 || areIndexesBroken' ctx n t2
    _ -> False
