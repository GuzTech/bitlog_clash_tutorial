module Example1 where
import CLaSH.Prelude

stack2 (mem, sp) push pop value = ((mem', sp'), o)
  where
    (mem', sp') = case push of
      True  -> case pop of
        True  -> (replace sp value mem, sp)
        False -> (replace sp value mem, sp + 1)
      False -> case pop of
        True  -> (mem, sp - 1)
        False -> (mem, sp)
    o = case pop of
      True -> mem !! sp'
      _    -> 0

topEntity
  :: (Vec 8 (Signed 16), Unsigned 3)
  -> Bool 
  -> Bool
  -> Signed 16
  -> ((Vec 8 (Signed 16), Unsigned 3), Signed 16)
topEntity = stack2

-- Push 3 on the stack
x = topEntity (repeat 0, 0) True False 3

-- Push 5 on the stack
y = topEntity (fst x) True False 5

-- Pop the last value off of the stack
z = topEntity (fst y) False True 0

-- Pop the last value off of the stack and replace it with 42
w = topEntity (fst z) True True 42
