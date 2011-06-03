module Kinterp (keval) where
-- An evaluator for K-normal form

import PetitML
import Knormal

import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

import Debug.Trace


type Env = Map.Map Name Value

data Value = IntVal Int
           | FunVal Env Name Knorm
             deriving (Show)


type EvalM a = State (Map.Map Name Value) a

getVal :: Name -> EvalM Value
getVal a = do env <- get
              Map.lookup a env

addVal :: Name -> Value -> EvalM ()
addVal name val = do env <- get
                     put (Map.insert name val env)

kinterpM :: Knorm -> EvalM Value
kinterpM (Lit i) = return (IntVal i)
kinterpM (Var x1) = getVal x1
kinterpM (Let x1 k1 k2) = do v1 <- kinterpM k1
                             addVal x1 v1
                             kinterpM k2
kinterpM (If0 x1 k1 k2) = do (IntVal i)<- getVal x1
                             (if i == 0
                              then kinterpM k1
                              else kinterpM k2)
kinterpM (Lambda f1 x1 k1 k2)
    = do env <- get
         addVal f1 (FunVal env x1 k1)
         kinterpM k2
kinterpM (App x1 x2) = do currenv <- get
                          v2 <- getVal x2
                          (FunVal funenv xf k1) <- getVal x1
                          put funenv
                          addVal xf v2
                          res <- kinterpM k1
                          put currenv
                          return res
kinterpM (Plus x1 x2) = do (IntVal v1) <- getVal x1
                           (IntVal v2) <- getVal x2
                           return (IntVal (v1+v2))


keval :: Knorm -> Int
keval x = case evalState (kinterpM x) Map.empty of
            (IntVal r) -> r
            _ -> -9999