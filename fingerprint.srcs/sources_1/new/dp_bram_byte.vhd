----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 23.09.2025 15:22:11
-- Design Name: 
-- Module Name: dp_bram_byte - Behavioral
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


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dp_bram_byte is
  generic (
    ADDR_WIDTH : integer := 13  -- depth = 2^ADDR_WIDTH 
  );
  port (
    -- Port A (proc domain @ rd_clk)
    a_clk  : in  std_logic;
    a_en   : in  std_logic;
    a_we   : in  std_logic;
    a_addr : in  unsigned(ADDR_WIDTH-1 downto 0);
    a_din  : in  std_logic_vector(7 downto 0);
    a_dout : out std_logic_vector(7 downto 0);

    -- Port B (proc domain @ rd_clk, or same as Port A; you'll use this for thinning reads)
    b_clk  : in  std_logic;
    b_en   : in  std_logic;
    b_we   : in  std_logic; -- tie '0' for read-only usage
    b_addr : in  unsigned(ADDR_WIDTH-1 downto 0);
    b_din  : in  std_logic_vector(7 downto 0);
    b_dout : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of dp_bram_byte is
  constant DEPTH : integer := 2**ADDR_WIDTH;
  type ram_t is array (0 to DEPTH-1) of std_logic_vector(7 downto 0);
  signal ram : ram_t := (others => (others => '0'));
  attribute ram_style : string;
attribute ram_style of ram : signal is "block";
begin

  -- Port A
  process(a_clk)
  begin
    if rising_edge(a_clk) then
      if a_en = '1' then
        if a_we = '1' then
          ram(to_integer(a_addr)) <= a_din;
        end if;
        a_dout <= ram(to_integer(a_addr));
      end if;
    end if;
  end process;

  -- Port B
  process(b_clk)
  begin
    if rising_edge(b_clk) then
      if b_en = '1' then
        if b_we = '1' then
          ram(to_integer(b_addr)) <= b_din;
        end if;
        b_dout <= ram(to_integer(b_addr));
      end if;
    end if;
  end process;

end architecture;

