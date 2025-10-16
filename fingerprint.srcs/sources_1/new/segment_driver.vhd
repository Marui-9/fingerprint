----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 05.10.2025 09:16:12
-- Design Name: 
-- Module Name: segment_driver - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
use ieee.numeric_std.all;

entity seven_segment_driver is
  generic (
    CNT_WIDTH : integer := 18  -- refresh rate ~190 Hz overall at 50 MHz
  );
  port (
    clk   : in  std_logic;
    rst   : in  std_logic;

    score_bin : in unsigned(15 downto 0);  -- binary score (0-65535)
    CA, CB, CC, CD, CE, CF, CG, DP : OUT STD_LOGIC;
    AN : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)           --active low
  );
end entity;

architecture rtl of seven_segment_driver is

  -- Refresh timing
  signal flick_counter : unsigned(CNT_WIDTH-1 downto 0) := (others => '0');

  -- BCD digits
  signal bcd0, bcd1, bcd2, bcd3 : std_logic_vector(3 downto 0) := (others => '0');
  signal digit_sel               : std_logic_vector(1 downto 0);
  signal digit_val               : std_logic_vector(3 downto 0);
  signal cathodes                : std_logic_vector(7 downto 0);

begin

  ------------------------------------------------------------------
  -- Clock divider (≈190 Hz multiplex rate)
  ------------------------------------------------------------------
  process(clk, rst)
  begin
    if rst = '1' then
      flick_counter <= (others => '0');
    elsif rising_edge(clk) then
      flick_counter <= flick_counter + 1;
    end if;
  end process;

  digit_sel <= std_logic_vector(flick_counter(CNT_WIDTH-1 downto CNT_WIDTH-2));

  ------------------------------------------------------------------
  -- Binary-to-BCD (double-dabble algorithm)
  -- Converts score_bin into 4 decimal digits (0-9999)
  ------------------------------------------------------------------
  process(score_bin)
    variable shift_reg : unsigned(15+16 downto 0);
    variable i         : integer;
  begin
    -- initialize shift register (upper 16 bits for BCD nibbles)
    shift_reg := (others => '0');
    shift_reg(15 downto 0) := score_bin;

    for i in 0 to 15 loop
      -- adjust each nibble if >= 5 before next shift
      if shift_reg(19 downto 16) > "0100" then
        shift_reg(19 downto 16) := shift_reg(19 downto 16) + 3;
      end if;
      if shift_reg(23 downto 20) > "0100" then
        shift_reg(23 downto 20) := shift_reg(23 downto 20) + 3;
      end if;
      if shift_reg(27 downto 24) > "0100" then
        shift_reg(27 downto 24) := shift_reg(27 downto 24) + 3;
      end if;
      if shift_reg(31 downto 28) > "0100" then
        shift_reg(31 downto 28) := shift_reg(31 downto 28) + 3;
      end if;
      shift_reg := shift_reg(30 downto 0) & '0';
    end loop;

    bcd3 <= std_logic_vector(shift_reg(31 downto 28)); -- thousands
    bcd2 <= std_logic_vector(shift_reg(27 downto 24)); -- hundreds
    bcd1 <= std_logic_vector(shift_reg(23 downto 20)); -- tens
    bcd0 <= std_logic_vector(shift_reg(19 downto 16)); -- ones
  end process;

  ------------------------------------------------------------------
  -- Multiplex digits (one at a time)
  ------------------------------------------------------------------
  with digit_sel select
    AN <= "1110" when "00",  -- rightmost
          "1101" when "01",
          "1011" when "10",
          "0111" when others; -- leftmost
   
  with digit_sel select
    digit_val <= bcd0 when "00",
                 bcd1 when "01",
                 bcd2 when "10",
                 bcd3 when others;

  ------------------------------------------------------------------
  -- 7-segment decoder (active low)
  -- Order: DP CG CF CE CD CC CB CA
  ------------------------------------------------------------------
  with digit_val select
    cathodes <=
      "11000000" when "0000", -- 0
      "11111001" when "0001", -- 1
      "10100100" when "0010", -- 2
      "10110000" when "0011", -- 3
      "10011001" when "0100", -- 4
      "10010010" when "0101", -- 5
      "10000010" when "0110", -- 6
      "11111000" when "0111", -- 7
      "10000000" when "1000", -- 8
      "10010000" when "1001", -- 9
      "11111111" when others; -- blank

  CA <= cathodes(0);
CB <= cathodes(1);
CC <= cathodes(2);
CD <= cathodes(3);
CE <= cathodes(4);
CF <= cathodes(5);
CG <= cathodes(6);
DP <= cathodes(7);  -- dot point, optional

end architecture;