{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Term.Context where

import qualified Data.Map.Strict as Map

import Control.Monad.State.Strict
import Control.Monad.Except

import Term

data Ctx a =
  Ctx { typings :: Sub
      , bindings :: Map.Map String a }
  deriving (Show)

type MonadTc m = ( MonadState Sub m, MonadError String m )

(∈) :: String -> Ctx a -> Maybe Type
s ∈ ctx = Map.lookup s (typings ctx)

bound :: String -> Ctx a -> Maybe a
s `bound` ctx = Map.lookup s (bindings ctx)

(∉) :: String -> Ctx a -> Bool
s ∉ ctx
  | Just{} <- s ∈ ctx = False
  | otherwise = otherwise

assume :: String -> Type -> Ctx a -> Ctx a
assume v t (Ctx ts bs) = Ctx (Map.insert v t ts) bs

runTc :: StateT (Map.Map String Term) (Except String) a -> Either String a
runTc = runExcept . fmap fst . flip runStateT mempty
