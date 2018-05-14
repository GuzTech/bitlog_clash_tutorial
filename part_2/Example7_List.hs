module Example7_List where
import CLaSH.Prelude
import Types
import Stimuli
import Simulation

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
  :: Signal SInstr
  -> Signal Value
topEntity instr = mealy stack5 (repeat 0 :: SMem, 0 :: SP) instr

--
-- Simulation
--

-- Synchronous logic simulation and testbench generation

test_sync = sampleN 5 $ expectedOutput $ topEntity testInput

-- Combinatorial logic simulation

-- Will contain just the output
test_o = sim_o stack5 (repeat 0 :: SMem, 0 :: SP) instrs

-- Will also contain the internal state
test_full = sim_full stack5 (repeat 0 :: SMem, 0 :: SP) instrs

-- Will only contain the memory state
test_mem = sim_mem stack5 (repeat 0 :: SMem, 0 :: SP) instrs

-- Will only contain the stack pointer state
test_sp = sim_sp stack5 (repeat 0 :: SMem, 0 :: SP) instrs
