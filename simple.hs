-- Simple lambda calculi
module Main where

import qualified Data.Map as Map

data Exp = Val Value
         | Let Var Exp Exp -- (let (x M0) in M1)
         | If0 Exp Exp Exp
         | App Exp [ Exp ]
         | AppOp PrimOp [ Exp ]
           deriving (Show)

data Value = Cst Const
           | Var Var
           | Fun [ Var ] Exp deriving (Show)

data PrimOp = AddInt32
            | SubInt32
            | Return deriving (Show)

type Var = String

type Const = Int


example1 :: Exp
example1 = Let "a" ( Val (Cst 12)) ( AppOp Return [(Val (Var "a"))] )


-- CEQ machine
-- type CEQEnv = Map.Map
-- K normalisation


type Var = String

type Const = Int

data Exp = Val Value
         | Let Var Exp Exp -- (let (x M0) in M1)
         | If0 Exp Exp Exp
         | App Exp [ Exp ]
         | AppOp PrimOp [ Exp ]
           deriving (Show)

data Value = Cst Const
           | Var Var
           | Fun [ Var ] Exp deriving (Show)

data PrimOp = AddInt32
            | SubInt32
            | Return deriving (Show)






main :: IO ()
main = do putStrLn "Hello"
          putStrLn $ show example1
