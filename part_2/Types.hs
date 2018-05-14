module Types where
import CLaSH.Prelude

type Value      = Signed 16
type SizeInBits = 3
type StackDepth = 2^SizeInBits
type SP         = Unsigned SizeInBits
type SMem       = Vec StackDepth Value

data SInstr = Push Value
            | Pop
            | PopPush Value
            deriving (Show, Lift)

