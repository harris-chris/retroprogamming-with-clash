{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module JustBlinks where
import Clash.Prelude
import CommonLib.Utils (succIdx)
import CommonLib.Clock
import Data.Either
import Data.Maybe

-- Just blinks, with the on-time and off-time being different. Try this with:
-- sampleN 30 $ topEntity clockGen

-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

-- on and off are type parameters!
data OnOff on off
    = On (Index on) -- On takes a single argument, of type (Index on)
    | Off (Index off)
    deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On{} = True
isOn Off{} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On (succIdx x)
countOnOff (Off x) = maybe (On 0) Off (succIdx x)

topEntity
    :: "CLK100MHZ" ::: Clock Dom100
    -> "LED" ::: Signal Dom100 Bit
topEntity clk =
    withClockResetEnable clk resetGen enableGen blinker

blinker
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom Bit
blinker = boolToBit . isOn <$> r
    where
        r :: Signal dom
              (OnOff
                  (ClockDivider dom (Nanoseconds 50))
                  (ClockDivider dom (Nanoseconds 20)))
        r = register (Off 0) $ countOnOff <$> r


