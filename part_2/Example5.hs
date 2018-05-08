module Example5 where
import CLaSH.Prelude

type Value      = Signed 16
type SizeInBits = 3
type StackDepth = 2^SizeInBits
type SP         = Unsigned SizeInBits
type SMem       = Vec StackDepth Value

data SInstr = Push Value
            | Pop
            | PopPush Value
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
  :: (SMem, SP)
  -> SInstr
  -> ((SMem, SP), Value)
topEntity = stack4

-- Push 3 on the stack
x = topEntity (repeat 0, 0) (Push 3)

-- Push 5 on the stack
y = topEntity (fst x) (Push 5)

-- Pop the last value off of the stack
z = topEntity (fst y) Pop

-- Pop the last value off of the stack and replace it with 42
w = topEntity (fst z) (PopPush 42)
