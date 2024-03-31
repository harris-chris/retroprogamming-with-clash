import Clash.Explicit.Prelude

flippy
    :: Clock System -> Reset System -> Enable System -> Signal System Bool
flippy clk rst en = r
    where r = register clk rst en True (register clk rst en True (not <$> r))
