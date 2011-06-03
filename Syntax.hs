-- Syntax

module Syntax where

import PetitML

import Control.Monad.State
import qualified Data.Map as Map

data Exp = Lit Int
         | Var Name
         | Plus Exp Exp
         | Let Name Exp Exp -- Let x = e1 in e2
         | If0 Exp Exp Exp
         | Lambda Name Name Exp Exp
-- fun a (x) = e1 in e2 Closure creation has to have a name
         | App Exp Exp
           deriving ( Show)


usedNameM :: Exp -> NameM ()
usedNameM (Lit _ ) = return ()
usedNameM (Var x)  = return ()
usedNameM (Plus e1 e2) = do usedNameM e1
                            usedNameM e2
usedNameM (Let x e1 e2) = do usedNameM e1
                             addName x x
                             usedNameM e2
usedNameM (If0 e1 e2 e3) = do usedNameM e1
                              usedNameM e2
                              usedNameM e3
usedNameM (Lambda f1 x1 e1 e2) = do addName f1 f1
                                    addName x1 x1
                                    usedNameM e1
                                    usedNameM e2
usedNameM (App e1 e2) = do usedNameM e1
                           usedNameM e2

usedName exp = execState (usedNameM exp) emptyNameState