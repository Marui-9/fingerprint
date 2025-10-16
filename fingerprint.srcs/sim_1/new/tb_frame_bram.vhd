----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 08:44:30
-- Design Name: 
-- Module Name: tb_frame_bram - Behavioral
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
use std.textio.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity tb_frame_bram is
  generic ( 
    DATA_WIDTH : integer := 8;
    ADDR_WIDTH : integer := 13
  );
  port (
    clk   : in  std_logic;

    -- Write port
    we    : in  std_logic;
    waddr : in  unsigned(ADDR_WIDTH-1 downto 0);
    din   : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Read port 0 (to DUT)
    rd0_en   : in  std_logic;
    rd0_addr : in  unsigned(ADDR_WIDTH-1 downto 0);
    rd0_dout : out std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Read port 1 (monitor / TB)
    rd1_en   : in  std_logic;
    rd1_addr : in  unsigned(ADDR_WIDTH-1 downto 0);
    rd1_dout : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture behav_ram of tb_frame_bram is
  type ram_t is array (0 to (2**ADDR_WIDTH)-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal mem : ram_t := (others => (others => '0'));

  signal rd0_en_q   : std_logic := '0';
  signal rd0_addr_q : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal rd1_en_q   : std_logic := '0';
  signal rd1_addr_q : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
begin
  process(clk)
  begin
    if rising_edge(clk) then
      -- write
      if we = '1' then
        mem(to_integer(waddr)) <= din;
      end if;

      -- pipeline read ports
      rd0_en_q   <= rd0_en;
      rd0_addr_q <= rd0_addr;
      rd1_en_q   <= rd1_en;
      rd1_addr_q <= rd1_addr;

      if rd0_en_q = '1' then
        rd0_dout <= mem(to_integer(rd0_addr_q));
      end if;
      if rd1_en_q = '1' then
        rd1_dout <= mem(to_integer(rd1_addr_q));
      end if;
    end if;
  end process;
end architecture;

-- =============================================
-- Testbench for thinning_subsystem
-- =========================
