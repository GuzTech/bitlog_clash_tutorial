module Simulation where
import CLaSH.Prelude
import Types
import Stimuli

--
-- Simulation
--

-- Stimuli


-- Synchronous logic simulation and testbench generation
testInput = stimuliGenerator $(listToVecTH instrs)
expectedOutput = outputVerifier expResults

-- Combinatorial logic simulation

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
