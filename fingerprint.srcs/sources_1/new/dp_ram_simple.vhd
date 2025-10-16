----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 17:37:21
-- Design Name: 
-- Module Name: dp_ram_simple - Behavioral
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
use IEEE.NUMERIC_STD.ALL;

entity dp_ram_simple is
  generic (
    ADDR_W : integer := 13;
    DATA_W : integer := 8
  );
  port (
    clk    : in  std_logic;

    -- Port A (write/read)
    a_en   : in  std_logic;
    a_we   : in  std_logic;
    a_addr : in  std_logic_vector(ADDR_W-1 downto 0);
    a_din  : in  std_logic_vector(DATA_W-1 downto 0);
    a_dout : out std_logic_vector(DATA_W-1 downto 0);

    -- Port B (read-only)
    b_en   : in  std_logic;
    b_addr : in  std_logic_vector(ADDR_W-1 downto 0);
    b_dout : out std_logic_vector(DATA_W-1 downto 0)
  );
end dp_ram_simple;

architecture rtl of dp_ram_simple is
  type ram_t is array (0 to (2**ADDR_W)-1) of std_logic_vector(DATA_W-1 downto 0);
  signal ram : ram_t := (others => (others => '0'));
  signal a_addr_i : integer range 0 to (2**ADDR_W)-1;
  signal b_addr_i : integer range 0 to (2**ADDR_W)-1;
  signal a_dout_i, b_dout_i : std_logic_vector(DATA_W-1 downto 0) := (others=>'0');
begin
  a_addr_i <= to_integer(unsigned(a_addr));
  b_addr_i <= to_integer(unsigned(b_addr));

  process(clk)
  begin
    if rising_edge(clk) then
      -- Port A
      if a_en = '1' then
        if a_we = '1' then
          ram(a_addr_i) <= a_din;
        end if;
        a_dout_i <= ram(a_addr_i);
      end if;

      -- Port B
      if b_en = '1' then
        b_dout_i <= ram(b_addr_i);
      end if;
    end if;
  end process;

  a_dout <= a_dout_i;
  b_dout <= b_dout_i;
end rtl;
