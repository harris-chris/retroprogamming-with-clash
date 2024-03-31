{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Blinker where
import Clash.Prelude
import CommonLib.Utils (succIdx)
import CommonLib.Clock
import Data.Either
import Data.Maybe
-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

-- on and off are type parameters!
data OnOff on off
    = On Nat (Index on)
    | Off Nat (Index off)
    deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On{} = True
isOn Off{} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On 0 x) = Off 0 0
countOnOff (Off 0 x) = Off 0 x
countOnOff (On blinks x) = maybe (Off (blinks - 1) 0) (On blinks) (succIdx x)
countOnOff (Off blinks y) = maybe (On blinks 0) (Off blinks) (succIdx y)

topEntity
    :: "CLK100MHZ" ::: Clock Dom100
    -> "LED" ::: Signal Dom100 Bit
topEntity clk =
    withClockResetEnable clk resetGen enableGen blinkingSecond

blinkCount :: Nat
blinkCount = 10

blinkingSecond
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom Bit
blinkingSecond = boolToBit . isOn <$> r
    where
        r :: Signal dom
              (OnOff
                  (ClockDivider dom (Microseconds 1))
                  (ClockDivider dom (Microseconds 1)))
        r = register (Off blinkCount 0) $ countOnOff <$> r


