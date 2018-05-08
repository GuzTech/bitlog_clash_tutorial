-- Check out https://bitlog.it/hardware/a-basic-introduction-to-clash-for-fpga-development/
-- for the blog post in which we create this file.

module Example1 where
import CLaSH.Prelude

counter val = val + 1
counter2 val = adder val 1

counter3 val enable = o
  where
    o = case enable of
      True  -> val + 1
      False -> val

counter4 val enable = o
  where
    o = case enable of
      True  -> adder val 1
      False -> val

adder val1 val2 = val1 + val2

addsub val1 val2 a_ns = o
  where
    res_add = val1 + val2
    res_sub = val1 - val2
    o = case a_ns of
      True  -> res_add
      False -> res_sub

stack1 (mem, sp) push pop value = ((mem', sp'), o)
  where
    sp' = case push of
      True  -> case pop of
        True  -> sp
        False -> sp + 1
      False -> case pop of
        True  -> sp - 1
        False -> sp
    mem' = case push of
      True  -> replace sp value mem
      False -> mem
    o = case pop of
      True  -> mem !! sp'
      _     -> 0

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

topEntity :: (Vec 8 (Signed 16), Unsigned 3) -> Bool -> Bool -> Signed 16 -> ((Vec 8 (Signed 16), Unsigned 3), Signed 16)
topEntity = stack1

-- Push 3 on the stack
x = topEntity (repeat 0, 0) True False 3

-- Pop one value off of the stack
y = topEntity (fst x) False True 0

-- Push and pop at the same time
z = topEntity (fst x) True True 31
