module Eta (eta) where

import Debug.Trace

import Control.Monad.State
import qualified Data.Map as Map

import PetitML
import Knormal


-- take the old name append something in front
eNewName :: Name -> NameM Name
eNewName oldname = newName createName' oldname
    where
      createName' i = "e_" ++ ( show i ) ++ "__" ++ oldname



-- nomadic eta convertion
etaM :: Knorm -> NameM Knorm
etaM (Lit i) = return (Lit i)
etaM (Var x1) = do ex1 <- updateName x1
                   return (Var ex1)
etaM (Let x1 k1 k2) = do ek1 <- etaM k1
                         ex1 <- eNewName x1
                         ek2 <- etaM k2
                         return (Let ex1 ek1 ek2)
etaM (If0 x1 k1 k2) = do ex1 <- updateName x1
                         ek1 <- etaM k1
                         ek2 <- etaM k2
                         return (If0 ex1 ek1 ek2)
etaM (Lambda f1 x1 k1 k2)
    = do ef1 <- eNewName f1
         ek2 <- etaM k2
         ex1 <- eNewName x1
         ek1 <- etaM k1
         return (Lambda ef1 ex1 ek1 ek2)
etaM (App x1 x2) = do ex1 <- updateName x1
                      ex2 <- updateName x2
                      return (App ex1 ex2)
etaM (Plus x1 x2) = do ex1 <- updateName x1
                       ex2 <- updateName x2
                       return (Plus ex1 ex2)

eta :: Knorm -> Knorm
eta exp = evalState (etaM exp) emptyNameState

