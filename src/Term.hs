module Term where

import qualified Data.Map.Strict as Map

type Sub = Map.Map String Term

data Term
  = Var String
  | Lam String Term Term
  | Pi String Term Term
  | Term :$ Term
  | Term ::: Term
  | Type Int
  deriving (Eq, Ord)

instance Show Term where
  showsPrec _ (Var x) = showString x

  showsPrec d (Lam v k t) = showParen (d > app_prec)
    $ showString "\x1b[1m\\(\x1b[0m" . showString v
    . showString "\x1b[1m :\x1b[0m " . showsPrec 0 k
    . showString "\x1b[1m).\x1b[0m " . showsPrec 0 t
      where app_prec = 10

  showsPrec d (Pi v k t)
    | v `happensIn` t
    = showParen (d > app_prec)
    $ showString "\x1b[33mforall\x1b[0m\x1b[1m (\x1b[0m"
    . showString v . showString " \x1b[1m:\x1b[0m "
    . showsPrec 0 k . showString "\x1b[1m).\x1b[0m "
    . showsPrec d t

    | otherwise = showParen (d > app_prec)
    $ showsPrec (d + 1 + app_prec) k
    . showString " \x1b[1m->\x1b[0m "
    . showsPrec 0 t
    where app_prec = 10

  showsPrec d (Type i) = showParen (d >= app_prec) $
      showString "\x1b[31mtype\x1b[0m " . showsPrec d i
    where app_prec = 10

  showsPrec d (f ::: x) = showParen (d > app_prec) $
      showsPrec d f . showString " : " . showsPrec d x
    where app_prec = 10
  showsPrec d (f :$ x) = showParen (d >= app_prec) $
      showsPrec 0 f . showString " "
    . showsPrec (app_prec + 1 + d) x
    where app_prec = 10

infixr 0 :$

data Stmt
  = Define String (Maybe Type) Term
  | Postulate String Type
  | Infer Term

type Type = Term

happensIn :: String -> Term -> Bool
happensIn v (Var v') = v == v'
happensIn v (Lam v' k t) = happensIn v k || if v == v' then False else happensIn v t
happensIn v (Pi v' k t) = happensIn v k || if v == v' then False else happensIn v t
happensIn v (f :$ x) = happensIn v f || happensIn v x
happensIn v (f ::: x) = happensIn v f || happensIn v x
happensIn _ Type{} = False

subst :: Sub -> Term -> Term
subst m (Var s) = Map.findWithDefault (Var s) s m
subst m (Lam s k t) = Lam s (subst m k) (subst (Map.delete s m) t)
subst m (Pi s k t) = Pi s (subst m k) (subst (Map.delete s m) t)
subst m (f :$ x) = subst m f :$ subst m x
subst m (f ::: x) = subst m f ::: subst m x
subst _ (Type i) = Type i
