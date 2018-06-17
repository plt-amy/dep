{-# LANGUAGE FlexibleContexts #-}
module Term.Unify where

import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict
import Control.Monad.Except

import Term.Context
import Term.Value
import Term

unify :: MonadTc m => Ctx Value -> Type -> Type -> m ()
unify _ a b | a == b = pure ()
unify c (Var s) t
  | Just s <- s `bound` c = unify c (reify s) t
  | s ∉ c = modify (Map.insert s t)
unify c t (Var s)
  | Just s <- s `bound` c = unify c t (reify s)
  | s ∉ c = modify (Map.insert s t)
unify c (Lam v k b) (Lam v' k' b') = do
  unify c k k'
  let b'' = subst (Map.singleton v' (Var v)) b'
   in unify (assume v k c) b b''
unify c (Pi v k b) (Pi v' k' b') = do
  unify c k k'
  let b'' = subst (Map.singleton v' (Var v)) b'
   in unify (assume v k c) b b''
unify c (f :$ x) (f' :$ x')
  | reduce c f == f, reduce c f' == f'
  = unify c f f' *> unify c x x'
unify c a@(_ :$ _) b@(_ :$ _) = unify c (reduce c a) (reduce c b)
unify _ (Type i) (Type j) | i == j = pure ()
unify _ a b = throwError $ "Couldn't match expected type " ++ show b ++ " and " ++ show a

