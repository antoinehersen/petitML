--- K normalisation

module Knormal where

import PetitML
import qualified Syntax as Syntax

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace


data Knorm = Lit Int
           | Var Name
           | Let Name Knorm Knorm -- Let x = e1 in e2
           | If0 Name Knorm Knorm
           | Lambda Name Name Knorm Knorm
 -- fun a (x) = e1 in e2 Closure creation has to have a name
           | App Name Name
--- OP
           | Plus Name Name
           deriving ( Show)


kNewName :: NameM Name
kNewName = newName' (createName "kx_") "0new_Kname"


ktransM :: Syntax.Exp -> NameM Knorm
ktransM (Syntax.Lit i ) = return (Lit i)
ktransM (Syntax.Var x)  = return (Var x)
ktransM (Syntax.Plus e1 e2)
    = do kx1 <- kNewName
         kx2 <- kNewName
         i1 <- ktransM e1
         i2 <- ktransM e2
         return (Let kx1 i1 (Let kx2 i2 (Plus kx1 kx2)))
ktransM (Syntax.Let x e1 e2)
    = do i1 <- ktransM e1
         i2 <- ktransM e2
         return (Let x i1 i2)
ktransM (Syntax.If0 e1 e2 e3)
    = do i1 <- ktransM e1
         kx1 <- kNewName
         i2 <- ktransM e2
         i3 <- ktransM e3
         return (Let kx1 i1 (If0 kx1 i2 i3))
ktransM (Syntax.Lambda f1 x1 e1 e2)
    = do i1 <- ktransM e1
         i2 <- ktransM e2
         return (Lambda f1 x1 i1 i2)
ktransM (Syntax.App e1 e2)
    = do i1 <- ktransM e1
         i2 <- ktransM e2
         kx1 <- kNewName
         kx2 <- kNewName
         return (Let kx1 i1 (Let kx2 i2 (App kx1 kx2)))


ktrans :: Syntax.Exp -> Knorm
ktrans a = let used = Syntax.usedName a
           in evalState (ktransM a) used



-- Calculate free varaible

freeVar :: Knorm -> Set.Set Name
freeVar (Lit i) = Set.empty
freeVar (Var x1) = Set.singleton x1
freeVar (Let x1 k1 k2) = let fv1 = freeVar k1
                             fv2 = freeVar k2
                             fv2' = Set.delete x1 (trace (show (Set.toList fv2)) fv2)
                         in Set.union fv1 fv2'
freeVar (If0 x1 k1 k2) = Set.unions [ Set.singleton x1, freeVar k1, freeVar k2 ]
freeVar (Lambda f1 x1 k1 k2)
    = let fk1 = freeVar k1
          fk1' = Set.delete x1 fk1
          fk2 = freeVar k2
      in Set.delete f1 (Set.union fk1' fk2)
freeVar (App x1 x2) = Set.fromList [x1,x2]
freeVar (Plus x1 x2) = Set.fromList [x1,x2]