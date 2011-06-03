--- General definition
module PetitML where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Variable name
type Name = String


-- Variable manipulation
type NameGeneratState = (Int, Set.Set Name)
type GenNameM a = State NameGeneratState a

gincCount :: GenNameM Int
gincCount = do (i, a) <- get
               put (i+1, a)
               return i

-- test if name is in the used tanle
gisUsed :: Name -> GenNameM Bool
gisUsed a = do (i, used) <- get
               return $ Set.member a used

-- Add a name to the used list with a renaming rule
gaddName :: Name -> GenNameM ()
gaddName a = do (i, used ) <- get
                put (i, Set.insert a used)



-- Create a new name IF already used
gnewName :: (Int -> Name ) -> GenNameM Name
gnewName fc = do i <- gincCount
                 let new = fc i
                 b <- gisUsed new
                 (if b
                  then gnewName fc
                  else do gaddName new
                          return new)


-- (Count, Used, Rename)
type NameState = (Int, Set.Set Name, Map.Map Name Name)


emptyNameState :: NameState
emptyNameState =  (0, Set.empty, Map.empty)

type NameM a = State NameState a

-- Counter to generate new name
incCount ::  NameM Int
incCount = do (i, a, b) <- get
              put (i+1, a, b)
              return i

-- test if name is in the used tanle
isUsed :: Name -> NameM Bool
isUsed a = do (i, used, trans) <- get
              return $ Set.member a used

-- Add a name to the used list with a renaming rule
addName :: Name -> Name -> NameM ()
addName old new = do (i, used, trans) <- get
                     put (i, Set.insert new used, Map.insert old new trans )


-- generate a name
createName :: String -> Int -> Name
createName str i = str ++ ( show i )

-- Create a new name IF already used
newName :: (Int -> Name ) -> Name -> NameM Name
newName fc oldName = do b <- isUsed oldName
                        (if b
                         then newName' fc oldName
                         else do addName oldName oldName
                                 return oldName)

-- Always generate a new name
newName' :: (Int -> Name) -> Name -> NameM Name
newName' fc oldName = do i <- incCount
                         let new = fc i
                         b <- isUsed new
                         (if b
                          then newName' fc oldName
                          else do addName oldName new
                                  return new)


-- update a name using the renaming table
updateName :: Name -> NameM Name
updateName a = do (i, used, trans) <- get
                  Map.lookup a trans


