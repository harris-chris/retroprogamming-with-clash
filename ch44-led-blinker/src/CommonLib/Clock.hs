module CommonLib.Clock
where

import Clash.Prelude

type Seconds (s :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds (1_000 * us)
type Nanoseconds (ns :: Nat) = Picoseconds (1_000 * ns)
type Picoseconds (ps :: Nat) = ps

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq

-- We want something that takes a real-life amount of time, regardless of the clock
-- Let's say the period is 0.01 seconds
-- DomainPeriod always resolves to picoseconds.
-- So the period (in picoseconds) is 1_000_000_000_000 * 0.01
-- ClockDivider dom (Milliseconds 500) = 0.5 seconds
-- Because 500_000_000_000 / (1 000 000 000 000 * 0.01) =
type ClockDivider dom ps = ps `Div` DomainPeriod dom
-- This is just telling us how many clock run-throughs `ps` will take

