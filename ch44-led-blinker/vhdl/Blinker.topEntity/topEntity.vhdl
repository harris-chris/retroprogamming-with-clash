-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.Blinker_topEntity_types.all;

entity topEntity is
  port(-- clock
       clk    : in Blinker_topEntity_types.clk_Dom100;
       result : out std_logic);
end;

architecture structural of topEntity is
  signal \c$bindCsr\                  : Blinker_topEntity_types.rst_Dom100;
  -- src/Blinker.hs:21:1-73
  signal y                            : Blinker_topEntity_types.index_50000000;
  signal \c$case_alt\                 : Blinker_topEntity_types.OnOff;
  -- src/Blinker.hs:21:1-73
  signal x                            : Blinker_topEntity_types.index_50000000;
  signal \c$case_alt_0\               : Blinker_topEntity_types.OnOff;
  signal result_1                     : Blinker_topEntity_types.OnOff;
  signal \c$app_arg\                  : std_logic_vector(0 downto 0);
  -- src/Blinker.hs:32:15-17
  signal r                            : Blinker_topEntity_types.OnOff := std_logic_vector'("1" & (std_logic_vector(to_unsigned(0,26))));
  signal \c$case_alt_selection_res\   : boolean;
  signal \c$case_alt_selection_res_0\ : boolean;
  signal \c$bv\                       : std_logic_vector(0 downto 0);

begin
  -- resetGen begin
  resetGen : block
    constant reset_delay : time := 100000 ps - 1 ps + (integer'(1) * 10000 ps);
  begin
  -- pragma translate_off
  \c$bindCsr\
    <= '1',
       '0' after reset_delay;
  -- pragma translate_on
  end block;
  -- resetGen end

  y <= Blinker_topEntity_types.index_50000000'(blinker_topentity_types.fromSLV(r(25 downto 0)));

  \c$case_alt_selection_res\ <= y = to_unsigned(49999999,26);

  \c$case_alt\ <= std_logic_vector'("0" & (std_logic_vector(to_unsigned(0,26)))) when \c$case_alt_selection_res\ else
                  std_logic_vector'("1" & (std_logic_vector(y + to_unsigned(1,26))));

  x <= Blinker_topEntity_types.index_50000000'(blinker_topentity_types.fromSLV(r(25 downto 0)));

  \c$case_alt_selection_res_0\ <= x = to_unsigned(49999999,26);

  \c$case_alt_0\ <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(0,26)))) when \c$case_alt_selection_res_0\ else
                    std_logic_vector'("0" & (std_logic_vector(x + to_unsigned(1,26))));

  with (r(26 downto 26)) select
    result_1 <= \c$case_alt_0\ when "0",
                \c$case_alt\ when others;

  with (r(26 downto 26)) select
    \c$app_arg\ <= std_logic_vector'("1") when "0",
                   std_logic_vector'("0") when others;

  \c$bv\ <= (\c$app_arg\);

  result <= \c$bv\(0);

  -- register begin
  r_register : process(clk,\c$bindCsr\)
  begin
    if \c$bindCsr\ =  '1'  then
      r <= std_logic_vector'("1" & (std_logic_vector(to_unsigned(0,26))));
    elsif rising_edge(clk) then
      r <= result_1;
    end if;
  end process;
  -- register end


end;

