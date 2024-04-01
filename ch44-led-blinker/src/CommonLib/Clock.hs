module CommonLib.Clock
where

import Clash.Prelude

type Seconds (s :: Nat) = Milliseconds (1_000 * s)
type Milliseconds (ms :: Nat) = Microseconds (1_000 * ms)
type Microseconds (us :: Nat) = Nanoseconds (1_000 * us)
type Nanoseconds (ns :: Nat) = Picoseconds (1_000 * ns)
type Picoseconds (ps :: Nat) = ps

type HzToPeriod (freq :: Nat) = 1_000_000_000_000 `Div` freq

type ClockDivider dom ps = ps `Div` DomainPeriod dom
