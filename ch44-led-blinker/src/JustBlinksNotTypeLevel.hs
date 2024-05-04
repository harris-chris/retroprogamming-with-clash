{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module JustBlinksNotTypeLevel where
import Clash.Prelude
import CommonLib.Utils (succIdx)
import CommonLib.Clock
import Data.Either
import Data.Maybe

-- What are the differences between JustBlinksNotTypeLevel and JustBlinks?
-- Asides from `rst`, there is a single register in both programs, called `r`.
-- If you look at the compiled verilog, there is a register r which has a role in the branching
-- In JustBlinks, `r` is `reg [3:0]`, initially set to 10xx
-- In JustBlinksNotTypeLevel, `r` is `reg [128:0]`, initially set to `{1'b1,64'sd0,64'sd5}`
-- in both of these, there are associated wires (`c$case_alt` and `result` which have the same width
-- and are there to store the r value.
-- and `r` is the register which stores both curr1 (in 127:64) and max1 (in 63:0)

-- This is the equivalent of JustBlinks, but where the clock ticks are
-- measured as a normal variable, rather than at the type level. This makes it possible to have,
-- say, a vector of `OnOff`, because this variable has no type parameters. Possibly it requires more
-- registers? Might be worth looking at the difference in VHDL

-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

data OnOff
    = On { curr::Integer, max::Integer }
    | Off { curr::Integer, max::Integer }
    deriving (Generic, NFDataX)

isOn :: OnOff -> Bool
isOn On{} = True
isOn Off{} = False

incrementOnOff :: OnOff -> OnOff
incrementOnOff (On curr max) =
    if curr == max
        then Off 0 max
        else On (curr + 1) max
incrementOnOff (Off curr max) =
    if curr == max
        then On 0 max
        else Off (curr + 1) max

topEntity
    :: "CLK100MHZ" ::: Clock Dom100
    -> "LEDS" ::: Signal Dom100 Bit
topEntity clk =
    withClockResetEnable clk resetGen enableGen blinker

getClockPeriod :: Integer -> Integer
getClockPeriod nanos = let
    clockPNat = clockPeriod @Dom100
    clockP::Integer = fromInteger $ natVal $ clockPNat
    targetP::Integer = nanos * 1_000
    in targetP `div` clockP

blinker
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom Bit
blinker = boolToBit . isOn <$> r
    where
        r :: Signal dom OnOff
        r = register (Off 0 period) $ incrementOnOff <$> r
        period = getClockPeriod 50

