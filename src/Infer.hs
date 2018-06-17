{-# LANGUAGE ViewPatterns, FlexibleContexts #-}
module Infer where

import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict
import Control.Monad.Except

import Term.Context
import Term.Value
import Term.Unify
import Term

ambient :: MonadState Sub m => Term -> m Term
ambient t = do
  sub <- get
  pure (subst sub t)

withContext :: MonadTc m => m a -> String -> m a
withContext c s = c `catchError` \x -> throwError (x ++ "\n • " ++ s)

infer :: MonadTc m => Ctx Value -> Term -> m Type
infer ctx (Var s) =
  case s ∈ ctx of
    Just t -> ambient t
    Nothing -> throwError $ "Not in scope: " ++ s

infer ctx (f :$ x) = do
  tau <- infer ctx f
  case reduce ctx tau of
    Pi v t s -> do
      check ctx x t
        `withContext` ("In the first argument of " ++ show f
                    ++ ", namely " ++ show x)
        `withContext` ("  which is expected to have type " ++ show t)
        `withContext` ("Note: " ++ show f ++ " has type " ++ show tau)
      ambient (subst (Map.singleton v x) s)
    x -> throwError $ "Expected a function type, but " ++ show f
                   ++ " has type " ++ show x

infer ctx (Lam s k bd) = do
  _ <- inferLevel ctx k
  sigma <- infer (assume s k ctx) bd
  ty <- ambient (Pi s k sigma)
  Type{} <- infer ctx ty
  pure ty

infer _ (Type i) = pure . Type . succ $ i

infer ctx (t ::: ty) = do
  check ctx t ty
  ambient ty

infer ctx (Pi v k s) = do
  i <- inferLevel ctx k
  j <- inferLevel (assume v k ctx) s
  pure (Type (max i j))

inferLevel :: MonadTc m => Ctx Value -> Term -> m Int
inferLevel ctx ty = do
  ki <- infer ctx ty
  case ki of
    Type i -> pure i
    _ -> throwError $ "Expected a kind, but got " ++ show ty

check :: MonadTc m => Ctx Value -> Term -> Type -> m ()
check ctx ex (reduce ctx -> Type j) = do
  i <- inferLevel ctx (reduce ctx ex)
  if i <= j
     then pure ()
     else throwError $ "Expected a type that fits in universe "
                    ++ show j ++ ", but got " ++ show (Type i)
check ctx ex tau = do
  sigma <- infer ctx ex
  let sigma' = reduce ctx sigma
      tau' = reduce ctx tau
  unify ctx sigma' tau'
    `withContext` ("While matching the types "
                ++ show sigma' ++ " and " ++ show tau')

