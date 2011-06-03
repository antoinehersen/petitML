module Closure where

import Debug.Trace

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import PetitML
import qualified Knormal as Kn

-- [hoisted] main
type ClosProgram = ( [ClosureDef], Clos)

-- ClosureDef FunctionName ArgName [FreeVars] Definition
data ClosureDef = ClosureDef Name Name [Name] Clos
                deriving (Show)

data Clos = Lit Int
          | Var Name
          | Let Name Clos Clos
          | If0 Name Clos Clos
          | MakeClos ClosureDef Clos
          | ApplyClos Name Name
          --- OP
          | Plus Name Name
            deriving ( Show)

-- define function closure
closure :: Kn.Knorm -> Clos
closure (Kn.Lit i) = Lit i
closure (Kn.Var x1) = Var x1
closure (Kn.Let x1 k1 k2) = Let x1 (closure k1) (closure k2)
closure (Kn.If0 x1 k1 k2) = If0 x1 (closure k1) (closure k2)
closure (Kn.Lambda f1 x1 k1 k2)
    = let fv = Kn.freeVar k1
          fv' = Set.delete x1 (Set.delete f1 fv)
          fclos = ClosureDef f1 x1 (Set.toList fv') (closure k1)
      in MakeClos fclos (closure k2)
closure (Kn.App x1 x2) = ApplyClos x1 x2
closure (Kn.Plus x1 x2) = Plus x1 x2


-- hoist function to top lrvel
hoisting :: Clos -> [ClosureDef]
hoisting (Let _ c1 c2) = (hoisting c1) ++ (hoisting c2)
hoisting ( If0 _ c1 c2) = (hoisting c1) ++ (hoisting c2)
hoisting (MakeClos cdef c1) = cdef : ( hoisting c1)
hoisting  _ = []


closureProg :: Kn.Knorm -> ClosProgram
closureProg a = let clos = closure a
                    hoist = hoisting clos
                in ( hoist, clos)


