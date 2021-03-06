module Example5_sim where
import CLaSH.Prelude

--
-- Configuration
--
type Value      = Signed 16
type SizeInBits = 3
type StackDepth = 2^SizeInBits
type SP         = Unsigned SizeInBits
type SMem       = Vec StackDepth Value

data SInstr = Push Value
            | Pop
            | PopPush Value
            deriving (Show)

--
-- Logic
--
stack5 (mem, sp) instr = ((mem', sp'), o)
  where
    (mem', sp') = case instr of
      Push val    -> (replace sp val mem, sp + 1)
      Pop         -> (mem, sp - 1)
      PopPush val -> (replace (sp - 1) val mem, sp)
    o = case instr of
      Pop       -> mem !! sp'
      PopPush _ -> mem !! (sp - 1)
      _         -> 0

topEntity
  :: (SMem, SP)
  -> SInstr
  -> ((SMem, SP), Value)
topEntity = stack5

--
-- Simulation
--
instrs = [Push 3, Push 5, PopPush 6, Pop, Pop]

-- Simulation function that just stores the output
sim_o f s [] = []
sim_o f s (x:xs) = o:sim_o f s' xs
  where
    (s', o) = f s x

-- Simulation function that stores the internal state and the output
sim_full f s [] = []
sim_full f s (x:xs) = (s', o):sim_full f s' xs
  where
    (s', o) = f s x

-- Simulation function that only stores the state of the memory
sim_mem f s [] = []
sim_mem f s (x:xs) = mem:sim_mem f s' xs
  where
    (s'@(mem, sp), o) = f s x

-- Simulation function that only stores the state of the stack pointer
sim_sp f s [] = []
sim_sp f s (x:xs) = sp:sim_sp f s' xs
  where
    (s'@(_, sp), o) = f s x

-- Will contain just the output
test_o = sim_o topEntity (repeat 0 :: SMem, 0 :: SP) instrs

-- Will also contain the internal state
test_full = sim_full topEntity (repeat 0 :: SMem, 0 :: SP) instrs

-- Will only contain the memory state
test_mem = sim_mem topEntity (repeat 0 :: SMem, 0 :: SP) instrs

-- Will only contain the stack pointer state
test_sp = sim_sp topEntity (repeat 0 :: SMem, 0 :: SP) instrs
