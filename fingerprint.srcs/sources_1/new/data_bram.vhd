----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 15.09.2025 14:09:10
-- Design Name: 
-- Module Name: data_bram - Behavioral
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



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity data_bram is
  generic (
    DATA_WIDTH : integer := 8;
    ADDR_WIDTH : integer := 13;  -- 2^13 = 8192 > 8024 bytes
    IMG_WIDTH  : integer := 68;
    IMG_HEIGHT : integer := 118
  );
  port (
    clk    : in  std_logic;
    rst    : in  std_logic;

    -- UART receiver outputs
    data_reg       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    done_reg       : in  std_logic;
    parity_err_reg : in  std_logic;
    frame_err_reg  : in  std_logic;

    -- Control
    frame_start : in  std_logic;  -- pulse to reset address counter
    frame_done  : out std_logic;

    -- Port B (reader)
    rd_clk   : in  std_logic;
    rd_en    : in  std_logic;
    rd_addr  : in  unsigned(ADDR_WIDTH-1 downto 0);
    rd_dout  : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of data_bram is

  constant FRAME_SIZE : integer := IMG_WIDTH * IMG_HEIGHT;

  -- Address & counter
  signal wr_addr   : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal byte_cnt  : integer range 0 to FRAME_SIZE := 0;

  -- Write control
  signal bram_we_a   : std_logic := '0';
  signal bram_addr_a : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bram_din_a  : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal armed :std_logic := '0';  
  -- Frame complete flag (sticky)
  signal frame_done_i : std_logic := '0';

  -------------------------------------------------------------------
  -- True Dual-Port BRAM (inferred)
  -------------------------------------------------------------------
  type ram_t is array (0 to (2**ADDR_WIDTH)-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal ram : ram_t;

  attribute ram_style : string;
  attribute ram_style of ram : signal is "block";

  signal q_a, q_b : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

begin

  -------------------------------------------------------------------
  -- UART -> BRAM writer (Port A) + frame_done control
  -------------------------------------------------------------------
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        wr_addr      <= (others => '0');
        byte_cnt     <= 0;
        bram_we_a    <= '0';
        frame_done_i <= '0';
        armed        <= '0';
      else
        bram_we_a <= '0';  -- default no write

        -- Clear frame_done and reset counter/address when CU signals new frame
        if frame_start = '1' then
          wr_addr      <= (others => '0');
          byte_cnt     <= 0;
          frame_done_i <= '0';
            armed        <= '1';
        -- Write incoming UART byte
        elsif (armed = '1' and done_reg = '1' 
        and parity_err_reg = '0' and frame_err_reg = '0') then
          bram_we_a   <= '1';
          bram_din_a  <= data_reg;
          bram_addr_a <= wr_addr;

          wr_addr  <= wr_addr + 1;
          byte_cnt <= byte_cnt + 1;

          -- If frame is complete, latch frame_done until cleared
          if byte_cnt = FRAME_SIZE-1 then
            frame_done_i <= '1';
            wr_addr      <= (others => '0');
            byte_cnt     <= 0;
            armed        <= '0';
            
          end if;
        end if;
      end if;
    end if;
  end process;

  frame_done <= frame_done_i;

  -------------------------------------------------------------------
  -- BRAM Port A (write port, UART domain)
  -------------------------------------------------------------------
  process(clk)
    variable idx_a : integer;
  begin
    if rising_edge(clk) then
      idx_a := to_integer(bram_addr_a);
      if bram_we_a = '1' then
        ram(idx_a) <= bram_din_a;
        q_a        <= bram_din_a;  -- write-first
      else
        q_a <= ram(idx_a);
      end if;
    end if;
  end process;

  -------------------------------------------------------------------
  -- BRAM Port B (read port, independent clock)
  -------------------------------------------------------------------
  process(rd_clk)
    variable idx_b : integer;
  begin
    if rising_edge(rd_clk) then
      if rd_en = '1' then
        idx_b := to_integer(rd_addr);
        q_b   <= ram(idx_b);
      end if;
    end if;
  end process;

  rd_dout <= q_b;

end architecture;



