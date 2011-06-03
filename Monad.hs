module Toto where

import Debug.Trace

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map


type Name = String

data Exp = Lit Int
         | Var Name
         | Plus Exp Exp
         | Let Name Exp
         | App Exp Exp
           deriving ( Show)

data Value = IntVal Int
           | FunVal Env Name Exp
             deriving (Show)

type Env = Map.Map Name Value


eval0                  :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1+i2)
eval0 env (Let n e)    = FunVal env n e
eval0 env (App e1 e2)  = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body


--- Eta transformation ---
type EtaTr a = State (Int, Map.Map Name Bool, Map.Map Name Name) a

incCount :: EtaTr Int
incCount = do (i, a, b) <- get
              put (i+1, a, b)
              return i

isUsed :: Name -> EtaTr Bool
isUsed a = do (i, used, trans) <- get
              return $ Map.member a used

addName :: Name -> Name -> EtaTr ()
addName old new = do (i, used, trans) <- get
                     put (i, Map.insert new True used, Map.insert old new trans )

newName :: Name -> EtaTr Name
newName oldName = do b <- isUsed oldName
                     (if b
                      then newName' oldName
                      else do addName oldName oldName
                              return oldName)

-- Always generate a new name
newName' :: Name -> EtaTr Name
newName' oldName = do i <- incCount
                      let new = "x_" ++ ( show i )
                      b <- isUsed new
                      (if (trace (show b) b)
                       then newName' oldName
                       else do addName oldName new
                               return new)


updateName :: Name -> EtaTr Name
updateName a = do (i, used, trans) <- get
                  Map.lookup a trans


eta              :: Exp -> EtaTr Exp
eta (Let n e)    = do new <- newName n
                      e1 <- eta e
                      return (Let new e1)
eta (Lit i)      = return (Lit i)
eta (Var n)      = do n1 <- updateName n
                      return (Var n1)
eta (Plus e1 e2) = do i1 <- eta e1
                      i2 <- eta e2
                      return (Plus i1 i2)
eta (App e1 e2)  = do i1 <- eta e1
                      i2 <- eta e2
                      return (App  i1 i2)




--checkName oldName = do (i, used, trans) <- get
--                       (if (Map.member new used)


--- Add basic identity nomad ---

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1                  :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1+i2)
eval1 env (Let n e)    = return $ FunVal env n e
eval1 env (App e1 e2)  = do val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                              FunVal env' n body ->
                                  eval1 (Map.insert n val2 env') body

-- Add error gestion ---
type Eval2 a = ErrorT String Identity a

runEval2    :: Eval2  a -> Either String a
runEval2 ev = runIdentity ( runErrorT ev )

eval2                  :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)      = return $ IntVal i
eval2 env (Var n)      = Map.lookup n env
eval2 env (Plus e1 e2) = do IntVal i1 <- eval2 env e1
                            IntVal i2 <- eval2 env e2
                            return $ IntVal (i1+i2)
eval2 env (Let n e)    = return $ FunVal env n e
eval2 env (App e1 e2)  = do val1 <- eval2 env e1
                            val2 <- eval2 env e2
                            case val1 of
                              FunVal env' n body ->
                                  eval2 (Map.insert n val2 env') body


----------------------------------------------------------
-- Examples ----
---------------------------------------------------------


exampleExp1 = Lit 12 `Plus` (App (Let "x" (Var "x")) (Lit 4 `Plus` Lit 2))

exampleExp2 = Lit 12 `Plus` (App (Let "x" ( (Var "x") `Plus` ( Let "x" ( (Var "x") `Plus` (Var "x"))))) (Lit 4 `Plus` Lit 2))


example3 = (Let "x" ( Let "x" ( Let "x" ( ( Var "x") `Plus` (Var "x")))))
