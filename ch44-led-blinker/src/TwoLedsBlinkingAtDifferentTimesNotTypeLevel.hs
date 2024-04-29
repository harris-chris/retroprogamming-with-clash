{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module TwoLedsBlinkingAtDifferentTimesNotTypeLevel where
import Clash.Prelude
import CommonLib.Utils (succIdx)
import CommonLib.Clock
import Data.Either
import Data.Maybe

-- Test this, with, say
-- clashi -isrc
-- :load BlinkMultipleLedsAtDifferentSpeeds
-- sampleN 10 $ topEntity clockGen

-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

-- on and off are type parameters!
data OnOff
    = On { curr::Natural, max::Natural }
    | Off { curr::Natural, max::Natural }
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
    -> "LEDS" ::: Signal Dom100 (Bit, Bit)
topEntity clk =
    withClockResetEnable clk resetGen enableGen multiLeds

getClockPeriod :: Natural -> Natural
getClockPeriod millis = let
    clockPNat = clockPeriod @Dom100
    clockP::Natural = fromInteger $ natVal $ clockPNat
    targetP::Natural = millis * 1_000_000_000
    in targetP `div` clockP

initialState :: (OnOff, OnOff)
initialState = (_1, _2)
    where
        _1 = Off 0 5
        _2 = Off 0 10

incrementState :: (OnOff, OnOff) -> (OnOff, OnOff)
incrementState (_1, _2) = (incrementOnOff _1, incrementOnOff _2)

stateToBit :: (OnOff, OnOff) -> (Bit, Bit)
stateToBit (_1, _2) = (boolToBit $ isOn _1, boolToBit $ isOn _2)

multiLeds
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom (Bit, Bit)
multiLeds = stateToBit <$> r
    where
        r = register initialState $ incrementState <$> r

