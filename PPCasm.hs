module PPCasm where
-- generate ppc asm code

import Data.Maybe
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

import PetitML
import qualified Closure as Clos

type Reg = Int

data Const = CInt Int
           | CName Name
             deriving (Show)

-- How to load large Integer ??
--	addis r2,r31,ha16(_do_addition-"L00000000001$pb")
--	la r0,lo16(_do_addition-"L00000000001$pb")(r2)


data Asm = Li Reg Const -- Load immedate
         | Lis Reg Const -- Load immediate shifted
         | Mr Reg Reg -- Mover register mnemonic
         | Stmw Reg Const Reg -- Store Multiple Word used to save register, stall pipeline !
         | Stw Reg Const Reg -- Store Word
         | Stwu Reg Const Reg -- Store Word and update
         | Lwz Reg Const Reg -- Load Word
         | Lmw Reg Const Reg -- Load multiple word
         | La Reg Const Reg
         | B Name -- branch to label
         | Bl Const -- Branch Link bl _toto ( call a function)
         | Blr -- Branch Link Register return from fcall
         | Mflr Reg -- Save link register
         | Mtlr Reg -- Restore link register
         | Mtctr Reg -- Load into CTR
         | Bctrl -- Branch to control register
         | Add Reg Reg Reg
         | Addi Reg Reg Const -- add imediate
         | If0 Reg [Asm] [Asm]
         | Cmpwi Int Reg Const -- Compare Word Immediate CR R Val
         | Bne Int Const -- Branch not Eaual CR Label
         | CallClos Reg Reg Reg -- result closure arg
         | MakeClos Reg Name [Reg] -- Result Fun Lable Closure to save
         | Label Name
         | Info String -- idented info
           deriving (Show)


type RegMap = Map.Map Name Reg

-- Target Env Cout Clos [Asm]
generateCode :: Reg -> RegMap -> Int -> Clos.Clos -> [Asm]
generateCode target _   _ (Clos.Lit i) =
    [Li target (CInt i)]
generateCode target env _ (Clos.Plus x y) =
    let rx = fromJust $ Map.lookup x env
        ry = fromJust $ Map.lookup y env
    in [Add target rx ry]
generateCode target env _ (Clos.Var x) =
    let rx = fromJust $ Map.lookup x env
    in [ Mr target rx ]
generateCode target env i (Clos.If0 x c1 c2) =
    let ac1 = generateCode target env i c1
        ac2 = generateCode target env i c2
        rx = fromJust $ Map.lookup x env
    in [If0 rx ac1 ac2]
generateCode target env i (Clos.MakeClos (Clos.ClosureDef fx _ xL _ ) c1) =
    let newEnv = Map.insert fx i env
        ip = i+1
        regList = mapList env xL
        ac1 = generateCode target newEnv ip c1
    in (MakeClos i fx regList ) : ac1
generateCode target env i (Clos.ApplyClos fx x1) =
    let rfx = fromJust $ Map.lookup fx env
        rx1 = fromJust $ Map.lookup x1 env
    in
      [CallClos target rfx rx1]
generateCode target env i (Clos.Let x c1 c2) =
    let newEnv = Map.insert x i env
        ip = i+1
        rx = i
        ac1 = generateCode rx env ip c1
        ac2 = generateCode target newEnv ip c2
    in ac1 ++ ac2



mapList :: RegMap -> [Name] -> [Reg]
mapList env a = map (fmap env) a
    where fmap env a = fromJust $ Map.lookup a env


testGenerate :: Clos.Clos -> [Asm]
testGenerate a = generateCode 3 Map.empty 4 a


data AsmFun = AsmFun Name Name [Name] RegMap [Asm]
              deriving (Show)


resultR = 5 -- where to put function result
closureR = 6
arg0R = 7
freeV1R = 8

-- ClosureDef FunctionName ArgName [FreeVars] Definition
-- data ClosureDef = ClosureDef Name Name [Name] Clos
funcASM :: Clos.ClosureDef -> AsmFun
funcASM (Clos.ClosureDef fname x0 freeVars clos) =
    let freeVarsL = zip freeVars (iterate (1+) freeV1R)
        env = Map.fromList ( [(fname, closureR), (x0,arg0R ) ] ++ freeVarsL )
        regfree = freeV1R + (length freeVars) -- first free register
        asmClos = generateCode resultR env regfree clos
    in AsmFun fname x0 freeVars env asmClos

generateAsmFun :: Clos.ClosureDef -> [Asm]
generateAsmFun a =
    let (AsmFun fName _ freeVars _ code ) = funcASM a
        (junkBegining, junkClose) = randomJumk fName
        fullCode = asmClosureConvert code
        funP = prologueFun freeVars
    in junkBegining ++ [Label fName] ++ prologue ++ funP ++ fullCode ++ epilogue ++ junkClose


generateAsmMain :: Clos.Clos -> [Asm]
generateAsmMain clos =
    let fName = "petitML_entry"
        asmCode = generateCode resultR Map.empty freeV1R clos
        fullCode = asmClosureConvert asmCode
        (junkBegining, junkClose) = randomJumk fName
    in junkBegining ++ [Label fName] ++ prologue ++ prologueMain ++ fullCode ++ epilogue ++ junkClose


stackSize = 8 + 112 --( SP+LK+ 5-31 )

prologue :: [Asm]
prologue = [ Stwu 1 (CInt (-stackSize)) 1, -- First so interupt dont screw anything
             Mflr 0, -- get ling
             Stw 0 (CInt (4+stackSize)) 1] -- save link in caller frane

-- r3 arg0 closure
-- r4 arg1 first argument
-- Assume that we are going to put arg1 in r5 and closure in r6 and so on
prologueFun :: [Name] -> [Asm]
prologueFun freeVars =
    let a = [ Mr closureR 3, Mr arg0R 4 ]
        b = getClosureFV (length freeVars)
    in a ++ b


-- We just take the heap pointer from r3
prologueMain :: [Asm]
prologueMain = [Mr 31 3]

-- put in register 7 and so
-- displacement in regard to r3
getClosureFV :: Int -> [Asm]
getClosureFV 0 = []
getClosureFV i = let disp = CInt (i * 4)
                 in (Lwz (i + arg0R) disp 3 ) : ( getClosureFV (i-1))


epilogue :: [Asm]
epilogue = [ Mr 3 resultR,   -- Get result
             Lwz 2 (CInt 0) 1, -- get prev frame sp
             Lwz 0 (CInt 4) 2, -- get lr from prev stack
             Mtlr 0,    -- load link register
            -- !!!!  restore register
             Mr 1 2,    -- restore stack pointer
             Blr ]      -- return

asmClosureConvert :: [Asm] -> [Asm]
asmClosureConvert a = concat $ map asmClosureConvertF a
    where
      asmClosureConvertF :: Asm -> [Asm]
      asmClosureConvertF (CallClos res clos arg) =
          -- we need to save the registers
          [ Stmw 5 (CInt 8) 1 ,-- Not very subtil not very fast
            Lwz 2 (CInt 0) clos, -- get function adress
            Mtctr 2, -- put it in the control reister
            Mr 4 arg, -- put the argument where it should be
            Mr 3 clos, -- the closure is pas as an argument to be able to grab the freevar
            Bctrl, -- call
            Mr 0 31, -- save the heap position
            Lmw 5 (CInt 8) 1, -- Not sure at all
            Mr 31 0,
            Mr res 3 ]
      asmClosureConvertF (MakeClos res fname freeVars) =
          let a = [ Mr res 31, -- 31 hold the heap pointer
                               -- we assume position dependance
                    Lis 2 (CName (fname ++ "@ha")),
                    La 0 (CName (fname ++ "@l")) 2,
                    Stw 0 (CInt 0) 31 ] -- Save the closure address in the heap
              b = putClosureFV freeVars
          in a ++ b
      asmClosureConvertF (If0 r0 a1 a2) =
          let c1 = asmClosureConvert a1
              c2 = asmClosureConvert a2
          in [If0 r0 c1 c2]
      asmClosureConvertF a = [a]

-- put free vars on the heap and increase heappointer
putClosureFV :: [Reg] -> [Asm]
putClosureFV freeVars = let dispHeapPt = CInt (4*(1 + (length freeVars)))
                        in (putClosureFV' 4 freeVars) ++ [Addi 0 31 dispHeapPt ]
    where putClosureFV' :: Int -> [Reg] -> [Asm]
          putClosureFV' _ [] = []
          putClosureFV' i (r:xs) = (Stw r (CInt i) 31) : putClosureFV' (i+4) xs


-- get if nabel and so
-- labelingM :: Clos.ClosureProgram -> NameM [Asm]
-- labelingM ( [functions], main) =
--     do let mainName = "petitml_entry"
--        addName mainName mainName

progLabelIf :: [Asm] -> [Asm]
progLabelIf a = evalState (progLabelIfM a) (1, Set.empty )

progLabelIfM :: [Asm] -> GenNameM [Asm]
progLabelIfM a = let b = mapM labelIfM a
                 in (liftM concat) b

labelIfM :: Asm -> GenNameM [Asm]
labelIfM (If0 ro asmThen asmElse) =
    do lElse <- newLabelName
       lCon  <- newLabelName
       let head = [ Cmpwi 3 ro (CInt 0), Bne 3 (CName lElse) ]
       cAsmThen <- progLabelIfM asmThen
       cAsmElse  <- progLabelIfM asmElse
       let fThen = cAsmThen ++ [ B lCon]
       let fElse = [Label lElse] ++ cAsmElse ++ [Label lCon]
       return ( head ++ fThen ++ fElse)
    where
      newLabelName = gnewName (createName ".L")
labelIfM a = return [a]


randomJumk :: Name -> ([Asm],[Asm])
randomJumk name = ([Info ".align 2", Info (".global " ++ name), Info (".type  " ++ ", @function")] , [Info (".size  " ++ name ++ ",.-" ++ name )])



generateAsmProg :: Clos.ClosProgram -> [Asm]
generateAsmProg (funs, main) =
    let pfuns = map generateAsmFun funs
        pmain = generateAsmMain main
        fullProg = (concat pfuns) ++ pmain
        fullProgIf = progLabelIf fullProg
    in fullProgIf


pCst :: Const -> String
pCst (CName a) = a
pCst (CInt a) = show a

ident :: String -> String
ident a = "   " ++ a

oneReg :: Reg -> String
oneReg a = " " ++ show a

twoReg :: Reg -> Reg -> String
twoReg a b = " " ++ (show a) ++ "," ++ (show b)

threeReg :: Reg -> Reg -> Reg -> String
threeReg a b c= " " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)

displacment :: Reg -> Const -> Reg -> String
displacment r0 c r1 = " " ++ (show r0) ++ "," ++ (pCst c) ++ "(" ++ (show r1) ++ ")"

intialAsm :: [Asm]
intialAsm = [Info ".file   \"petitML.pml\"",
             Info ".section        \".text\""]

finalAsm :: [Asm]
finalAsm = [Info ".ident  \"Super petit Ml compiler\"",
            Info ".section        .note.GNU-stack,\"\",@progbits"]

generateString :: [Asm] -> String
generateString a =
    let a' = intialAsm ++ a ++ finalAsm
        b = map generateString' a'
        c = map (\x -> x ++ "\n") b
    in concat c
    where
      generateString' :: Asm -> String
      generateString' (Li r1 val ) =  ident ( "li " ++ (oneReg r1) ++ "," ++ (pCst val))
      generateString' (Lis r1 val ) =  ident ( "lis " ++ (oneReg r1) ++ "," ++ (pCst val))
      generateString' (Mr r1 r2) = ident ( "mr" ++ (twoReg r1 r2))
      generateString' (Stmw r1 c r2) = ident ( "stmw" ++ (displacment r1 c r2))
      generateString' (Stw r1 c r2) = ident ( "stw" ++ (displacment r1 c r2))
      generateString' (Stwu r1 c r2) = ident ( "stwu" ++ (displacment r1 c r2))
      generateString' (Lwz r1 c r2) = ident ( "lwz" ++ (displacment r1 c r2))
      generateString' (Lmw r1 c r2) = ident ( "lmw" ++ (displacment r1 c r2))
      generateString' (La r1 c r2) = ident ( "la" ++ (displacment r1 c r2))
      generateString' (Bl c) = ident ( "bl" ++ (pCst c))
      generateString' (Blr) = ident "blr"
      generateString' (B name) = ident ("b " ++ name)
      generateString' (Mflr r1) = ident ( "mflr" ++ (oneReg r1))
      generateString' (Mtlr r1) = ident ( "mtlr" ++ (oneReg r1))
      generateString' (Mtctr r1) = ident ( "mtctr" ++ (oneReg r1))
      generateString' (Bctrl) = ident "bctrl"
      generateString' (Add r1 r2 r3) = ident ("add" ++ (threeReg r1 r2 r3))
      generateString' (Addi r1 r2 c) = ident ("addi" ++ (twoReg r1 r2) ++ "," ++ (pCst c))
      generateString' (Cmpwi cr r1 cv) = ident ("cmpwi " ++ (show cr) ++ "," ++ (show r1) ++ "," ++ (pCst cv))
      generateString' (Bne cr clabel) = ident ( "bne" ++ (oneReg cr) ++ "," ++ (pCst clabel))
      generateString' (Label name) = name ++ ":"
      generateString' (Info name) = ident name

