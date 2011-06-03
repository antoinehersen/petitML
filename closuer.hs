module Closure where

import Debug.Trace

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import PetitML
import Knormal as Kn


data Clos = Lit Int
          | Var Name
          | Let Name Clos Clos
          | If0 Name Clos Clos
          | MakeClos Name Name [Name] Clos Clos
          | ApplyClos Name Name
          --- OP
          | Plus Name Name
            deriving ( Show)


closure :: Kn.Knorm -> Clos
colsure (Kn.Lit i) = Lit i
closure (Kn.Var x1) = Var x1
closure (Kn.Let x1 k1 k2) = Let x1 (closure k1) (closure k2)
closure (Kn.If0 x1 k1 k2) = If0 x1 (closure k1) (closure k2)
closure (Kn.Lambda f1 x1 k1 k2)
    = let fv = Kn.freeVar k1
          fv' = Set.delete x1 (Set.delete f1 fv)
      in
        MakeClos f1 x1 (Set.toList fv') (closure k1) (closure k2)

closure (Kn.App x1 x2) = ApplyClos x1 x2
closure (Kn.Plus x1 x2) = Plus x1 x2
