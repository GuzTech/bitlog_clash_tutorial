module Example3 where
import CLaSH.Prelude

data SInstr = Push
            | Pop
            | PopPush
            deriving (Show)

stack3 (mem, sp) instr value = ((mem', sp'), o)
  where
    (mem', sp') = case instr of
      Push    -> (replace sp value mem, sp + 1)
      Pop     -> (mem, sp - 1)
      PopPush -> (replace (sp - 1) value mem, sp)
    o = case instr of
      Pop     -> mem !! sp'
      PopPush -> mem !! (sp - 1)
      _       -> 0

topEntity
  :: (Vec 8 (Signed 16), Unsigned 3)
  -> SInstr
  -> Signed 16
  -> ((Vec 8 (Signed 16), Unsigned 3), Signed 16)
topEntity = stack3

-- Push 3 on the stack
x = topEntity (repeat 0, 0) Push 3

-- Push 5 on the stack
y = topEntity (fst x) Push 5

-- Pop the last value off of the stack
z = topEntity (fst y) Pop 0

-- Pop the last value off of the stack and replace it with 42
w = topEntity (fst z) PopPush 42
