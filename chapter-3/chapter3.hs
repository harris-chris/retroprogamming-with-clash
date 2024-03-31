-- This is the Clash equivalent of the Haskell Prelude
import Clash.Prelude
-- What `main` is for a Haskell program, `topEntity` is for Clash.
topEntity :: Signal System Bit -> Signal System Bit
topEntity = id
