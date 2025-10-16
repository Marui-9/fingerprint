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

entity thr_bram is
  generic (
    DATA_WIDTH : integer := 8;   -- threshold per block (0..255)
    ADDR_WIDTH : integer := 8    -- enough for up to 256 blocks (135 needed here)
  );
  port (
    clk       : in  std_logic;
    -- write port (builder)
    wr_en     : in  std_logic;
    wr_addr   : in  unsigned(ADDR_WIDTH-1 downto 0);
    wr_data   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    -- read port (binarizer)
    rd_en     : in  std_logic;
    rd_addr   : in  unsigned(ADDR_WIDTH-1 downto 0);
    rd_data   : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of thr_bram is
  type ram_t is array (0 to (2**ADDR_WIDTH)-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal mem : ram_t := (others => (others => '0'));
  signal rd_en_q : std_logic := '0';
  signal rd_addr_q : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
begin
  process(clk)
  begin
    if rising_edge(clk) then
      -- write
      if wr_en = '1' then
        mem(to_integer(wr_addr)) <= wr_data;
      end if;
      -- registered read (1-cycle latency)
      rd_en_q   <= rd_en;
      rd_addr_q <= rd_addr;
      if rd_en_q = '1' then
        rd_data <= mem(to_integer(rd_addr_q));
      end if;
    end if;
  end process;
end architecture;