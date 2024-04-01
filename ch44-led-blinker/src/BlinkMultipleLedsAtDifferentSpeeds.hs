{-# LANGUAGE NumericUnderscores, PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module BlinkMultipleLedsAtDifferentSpeeds where
import Clash.Prelude
import CommonLib.Utils (succIdx)
import CommonLib.Clock
import Data.Either
import Data.Maybe
-- Change this to the raw clock rate of the FPGA board you are targeting
createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

-- on and off are type parameters!
data OnOff on off
    = On (Index on)
    | Off (Index off)
    deriving (Generic, NFDataX)

isOn :: OnOff on off -> Bool
isOn On{} = True
isOn Off{} = False

countOnOff :: (KnownNat on, KnownNat off) => OnOff on off -> OnOff on off
countOnOff (On x) = maybe (Off 0) On (succIdx x)
countOnOff (Off y) = maybe (On 0) Off (succIdx y)

topEntity
    :: "CLK100MHZ" ::: Clock Dom100
    -> "LEDS" ::: Signal Dom100 (Bit, Bit)
topEntity clk =
    withClockResetEnable clk resetGen enableGen multiLeds

blinkCount :: Nat
blinkCount = 10

data LedState dom p1 p2 = LedState
    (OnOff (ClockDivider dom p1) (ClockDivider dom p1))
    (OnOff (ClockDivider dom p2) (ClockDivider dom p2))

initialState
    :: (
        KnownNat (ClockDivider dom (Microseconds 1))
        , KnownNat (ClockDivider dom (Microseconds 2))
        , forall dom. (HiddenClockResetEnable dom, _)
    ) => LedState dom (Microseconds 1) (Microseconds 2)
initialState = LedState _1 _2
    where
        _1 :: (OnOff (ClockDivider dom (Microseconds 1)) (ClockDivider dom (Microseconds 1)))
        _1 = Off 0
        _2 :: (OnOff (ClockDivider dom (Microseconds 2)) (ClockDivider dom (Microseconds 2)))
        _2 = Off 0

multiLeds
    :: forall dom. (HiddenClockResetEnable dom, _)
    => Signal dom (Bit, Bit)
-- boolToBit is being lifted to work with Signal dom (Bit, Bit)
multiLeds = (fmap . fmap) (boolToBit . isOn) r
    where
        r :: LedState dom (Microseconds 1) (Microseconds 2)
        r = register initialState $ (fmap . fmap) countOnOff r


