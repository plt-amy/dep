dep
===

A small implementation of the Calculus of Constructions, a pure type
system (PTS) corresponding to the λ<sub>Π</sub> corner of Barendgret's
lambda cube. That is, it has:

  - terms depending on terms  (functions)
  - types depending on types  (type operators)
  - types depending on terms  (dependent functions)
  - values depending on types (polymorphism)

While simply-typed languages have distinct "type level" and "value
level" grammars, the CoC does away with the stratification by merging
these levels into a single language, expressed by the following Haskell
data type.

```haskell
data Term
  = Var String -- x
  | Lam String Term Term -- \(x : t). e
  | Pi String Term Term -- forall (x : k). t
  | Term :$ Term -- f x
  | Term ::: Term -- e : t
  | Type Int -- type i
  deriving (Eq, Ord)
```
