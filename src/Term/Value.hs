module Term.Value where

import qualified Data.Map.Strict as Map

import Term.Context
import Term

data Value
  = FreeVar String
  | App Value Value
  | Pi' String Value (Value -> Value)
  | Lam' String Value (Value -> Value)
  | Level Int

instance Show Value where
  show (Lam' v t k) = "\\(" ++ show v ++ " : " ++ show t ++ "). " ++ show (k (FreeVar v))
  show (Pi' v t k) = "forall (" ++ show v ++ " : " ++ show t ++ "). " ++ show (k (FreeVar v))
  show (App f x) = show f ++ " " ++ show x
  show (FreeVar v) = v
  show (Level i) = show i

reduce :: Ctx Value -> Term -> Term
reduce ct = reify . eval (bindings ct)

eval :: Map.Map String Value -> Term -> Value
eval ct (Lam v t e) = Lam' v (eval ct t) (\x -> eval (Map.insert v x ct) e)
eval ct (Pi v t e) = Pi' v (eval ct t) (\x -> eval (Map.insert v x ct) e)
eval ct (Var v) = case v `Map.lookup` ct of
  Just x -> x
  _ -> FreeVar v
eval ct (f :$ x) = app (eval ct f) (eval ct x) where
  app :: Value -> Value -> Value
  app (Lam' _ _ k) v = k v
  app f x = App f x
eval ct (t ::: _) = eval ct t
eval _ (Type i) = Level i

reify :: Value -> Term
reify (Lam' v t k) = Lam v (reify t) (reify (k (FreeVar v)))
reify (Pi' v t k) = Pi v (reify t) (reify (k (FreeVar v)))
reify (FreeVar v) = Var v
reify (App f x) = reify f :$ reify x
reify (Level i) = Type i
