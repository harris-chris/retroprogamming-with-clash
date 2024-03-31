import Clash.Explicit.Prelude

helloRegister
		:: Clock System -> Reset System -> Enable System -> Signal System Bool
helloRegister clk rst en = register clk rst en True (pure False)
