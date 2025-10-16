----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 17:21:59
-- Design Name: 
-- Module Name: minutiae_bram - Behavioral
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


-- minutiae_bram.vhd
-- VHDL-93
-- True dual-port BRAM for 512x32 minutiae records
-- Port A: write (from minutiae_extractor)
-- Port B: read (CPU/VGA/debug)

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minutiae_bram is
  generic (
    DATA_WIDTH : integer := 32;
    ADDR_WIDTH : integer := 9    -- 2^9 = 512 entries
  );
  port (
    -- Port A (write side: minutiae_extractor)
    clkA  : in  std_logic;
    weA   : in  std_logic;
    addrA : in  unsigned(ADDR_WIDTH-1 downto 0);
    dinA  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    doutA : out std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Port B (read side: CPU/VGA/debug)
    clkB  : in  std_logic;
    enB   : in  std_logic;
    addrB : in  unsigned(ADDR_WIDTH-1 downto 0);
    doutB : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of minutiae_bram is
  type ram_t is array (0 to 2**ADDR_WIDTH - 1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal ram : ram_t := (others => (others => '0'));

  signal addrA_q : unsigned(ADDR_WIDTH-1 downto 0);
  signal addrB_q : unsigned(ADDR_WIDTH-1 downto 0);
  signal enB_q   : std_logic := '0';
begin

  -- Port A (write-first)
  process(clkA)
  begin
    if rising_edge(clkA) then
      addrA_q <= addrA;
      if weA = '1' then
        ram(to_integer(addrA)) <= dinA;
      end if;
      doutA <= ram(to_integer(addrA_q));  -- synchronous read
    end if;
  end process;

  -- Port B (read-only, synchronous)
  process(clkB)
  begin
    if rising_edge(clkB) then
      addrB_q <= addrB;
      enB_q   <= enB;
      if enB_q = '1' then
        doutB <= ram(to_integer(addrB_q));
      end if;
    end if;
  end process;

end architecture;

