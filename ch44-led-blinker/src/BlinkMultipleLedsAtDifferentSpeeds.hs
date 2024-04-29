{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module BlinkMultipleLedsAtDifferentSpeeds where
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
data OnOff period
    = On (Index period)
    | Off (Index period)
    deriving (Generic, NFDataX)

-- We need the this counter to tick up to a number of ticks corresponding to our desired period.
-- So the question is, how many ticks in x time?
-- The clock period will be in picoseconds, x is also expressed in picoseconds
type OnOffFromPeriod dom ps = OnOff (ps `Div` (DomainPeriod dom))

isOn :: OnOff period -> Bool
isOn On{} = True
isOn Off{} = False

incrementOnOff :: KnownNat period => OnOff period -> OnOff period
incrementOnOff (On x) = maybe (Off 0) On (succIdx x)
incrementOnOff (Off y) = maybe (On 0) Off (succIdx y)

topEntity
    :: "CLK100MHZ" ::: Clock Dom100
    -> "LEDS" ::: Signal Dom100 (Bit, Bit)
topEntity clk =
    withClockResetEnable clk resetGen enableGen multiLeds

initialState
    :: forall dom. (HiddenClockResetEnable dom, _) =>
    (OnOffFromPeriod dom (Picoseconds 50_000), OnOffFromPeriod dom (Picoseconds 100_000))
initialState = (_1, _2)
    where
        _1 = Off 0
        _2 = Off 0

incrementState :: (OnOff _1, OnOff _2) -> (OnOff _1, OnOff _2)
incrementState (_1, _2) = (incrementOnOff _1, incrementOnOff _2)

stateToBit :: (OnOff _1, OnOff _2) -> (Bit, Bit)
stateToBit (_1, _2) = (boolToBit $ isOn _1, boolToBit $ isOn _2)

multiLeds
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom (Bit, Bit)
multiLeds = stateToBit <$> r
    where
        r = register initialState $ incrementState <$> r

