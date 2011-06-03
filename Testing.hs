module Main where

import qualified Data.Set as Set

--import PetitML
import qualified Syntax as Syn
import qualified Knormal as Kn
import qualified Eta as Eta
import qualified Closure as Clos
import qualified PPCasm as Asm

import Kinterp


example1 :: Exp
example1 = ( Let "x" (Lit 12) ( Plus (Lit 1) ( App ( Lambda "f1" "y" (Var "x") (Var "f1")) (Let "x" (Lit 2) (Var "x")))))

example2 = (Lit 200)

example3 = (Let "b" (Let "a" (Lit 0) (If0 (Var "a") example1 example2 )) (Plus (Lit 3) (Var "b")))

main :: IO ()
main = do putStrLn "Potatoes are funny"
          let t1 = Kn.ktrans example3
          putStrLn $ show t1
          let t2 = Eta.eta t1
          putStrLn $ show t2
          putStrLn $ show (keval t2)
          let c3 = Clos.closureProg t1
--          putStrLn $ show c3
          let a3 = Asm.generateAsmProg c3
--          putStrLn $ show a3
          let a4 = Asm.generateString a3
          putStrLn A4
--          putStrLn $ show (Set.toList ( Kn.freeVar t1))
--          putStrLn $ show (Set.toList ( Kn.freeVar t2))
-- --           putStrLn $ show (Clos.closure t1)
--           let c2 = Clos.closure t2
--           putStrLn $ show c2
-- --           putStrLn $ show (Clos.hoisting c2)
--           let a2 = Asm.testGenerate c2
--           putStrLn $ show a2
--           let a3 = Asm.progLabelIf a2
--           putStrLn $ show a3
--           let a4 = Asm.asmClosureConvert a3
--           putStrLn $ show a411