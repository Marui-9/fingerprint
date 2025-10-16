----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 16.09.2025 10:32:07
-- Design Name: 
-- Module Name: utils_pkg - Behavioral
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

entity binary_bram is
  generic (
    DATA_WIDTH : integer := 8;    -- store 0x00 or 0xFF per pixel for simplicity
    ADDR_WIDTH : integer := 13    -- must cover IMG_WIDTH*IMG_HEIGHT
  );
  port (
    -- Port A (writer)
    a_clk   : in  std_logic;
    a_en    : in  std_logic;
    a_we    : in  std_logic;
    a_addr  : in  unsigned(ADDR_WIDTH-1 downto 0);
    a_din   : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Port B (reader, e.g., VGA)
    b_clk   : in  std_logic;
    b_en    : in  std_logic;
    b_addr  : in  unsigned(ADDR_WIDTH-1 downto 0);
    b_dout  : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of binary_bram is
  type ram_t is array (0 to (2**ADDR_WIDTH)-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal mem : ram_t := (others => (others => '0'));
  signal b_en_q : std_logic := '0';
  signal b_addr_q : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
begin
  -- Port A
  process(a_clk)
  begin
    if rising_edge(a_clk) then
      if a_en = '1' and a_we = '1' then
        mem(to_integer(a_addr)) <= a_din;
      end if;
    end if;
  end process;

  -- Port B (registered read, 1-cycle latency)
  process(b_clk)
  begin
    if rising_edge(b_clk) then
      b_en_q   <= b_en;
      b_addr_q <= b_addr;
      if b_en_q = '1' then
        b_dout <= mem(to_integer(b_addr_q));
      end if;
    end if;
  end process;
end architecture;