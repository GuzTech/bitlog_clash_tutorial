module Example4 where
import CLaSH.Prelude

data SInstr = Push (Signed 16)
            | Pop
            | PopPush (Signed 16)
            deriving (Show)

stack4 (mem, sp) instr = ((mem', sp'), o)
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
  :: (Vec 8 (Signed 16), Unsigned 3)
  -> SInstr
  -> ((Vec 8 (Signed 16), Unsigned 3), Signed 16)
topEntity = stack4

-- Push 3 on the stack
x = topEntity (repeat 0, 0) (Push 3)

-- Push 5 on the stack
y = topEntity (fst x) (Push 5)

-- Pop the last value off of the stack
z = topEntity (fst y) Pop

-- Pop the last value off of the stack and replace it with 42
w = topEntity (fst z) (PopPush 42)
