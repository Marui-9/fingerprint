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
use work.utils_pkg.all;

entity binarizer_stream is
  generic (
    IMG_WIDTH  : integer := 68;
    IMG_HEIGHT : integer := 118;
    BLOCK_SIZE : integer := 8;
    ADDR_WIDTH : integer := 13
  );
  port (
    clk       : in  std_logic;
    rst       : in  std_logic;

    -- pixel stream from image_window_reader (interior pixels)
    pixel_valid : in  std_logic;
    center_x    : in  std_logic_vector(7 downto 0);
    center_y    : in  std_logic_vector(7 downto 0);
    center_pix  : in  std_logic_vector(7 downto 0);

    -- THR_BRAM read port
    thr_rd_en   : out std_logic;
    thr_rd_addr : out unsigned;
    thr_rd_data : in  std_logic_vector(7 downto 0);

    -- BINARY_BRAM write port (1 byte/pixel)
    bin_wr_en   : out std_logic;
    bin_wr_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    bin_wr_data : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of binarizer_stream is
  constant NBX     : integer := ceil_div(IMG_WIDTH,  BLOCK_SIZE);
  constant NBY     : integer := ceil_div(IMG_HEIGHT, BLOCK_SIZE);
  constant NBLOCKS : integer := NBX * NBY;
  constant THR_AW  : integer := clog2(NBLOCKS);

  -- pipeline registers to align with THR_BRAM 1-cycle latency
  signal v_d     : std_logic := '0';
  signal x_d, y_d: unsigned(7 downto 0) := (others => '0');
  signal pix_d   : unsigned(7 downto 0) := (others => '0');

  signal thr_en_r : std_logic := '0';
  signal thr_addr_r : unsigned(THR_AW-1 downto 0) := (others => '0');

  signal bin_we_r : std_logic := '0';
  signal bin_addr_r : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bin_data_r : std_logic_vector(7 downto 0) := (others => '0');

begin
  thr_rd_en   <= thr_en_r;
  thr_rd_addr <= resize(thr_addr_r, thr_rd_addr'length);

  bin_wr_en   <= bin_we_r;
  bin_wr_addr <= bin_addr_r;
  bin_wr_data <= bin_data_r;

  process(clk)
    variable bx, by, bidx : integer;
    variable addr_i       : integer;
  begin
    if rising_edge(clk) then
      if rst = '1' then
        v_d       <= '0';
        x_d       <= (others => '0');
        y_d       <= (others => '0');
        pix_d     <= (others => '0');
        thr_en_r  <= '0';
        thr_addr_r<= (others => '0');
        bin_we_r  <= '0';
        bin_addr_r<= (others => '0');
        bin_data_r<= (others => '0');
      else
        -- default
        bin_we_r <= '0';

        -- stage 0: issue threshold read when a pixel is valid
        if pixel_valid = '1' then
          bx := to_integer(unsigned(center_x)) / BLOCK_SIZE;
          by := to_integer(unsigned(center_y)) / BLOCK_SIZE;
          bidx := by * NBX + bx;
          thr_en_r   <= '1';
          thr_addr_r <= to_unsigned(bidx, THR_AW); -- ✅ fixed
        
          -- delay pixel for 1 cycle to align with THR_BRAM output
          v_d   <= '1';
          x_d   <= unsigned(center_x);
          y_d   <= unsigned(center_y);
          pix_d <= unsigned(center_pix);
        else
          thr_en_r <= '0';
          v_d      <= '0';
        end if; 

        -- stage 1: compare and write binary
        if v_d = '1' then
          if pix_d >= unsigned(thr_rd_data) then
            bin_data_r <= (others => '1'); -- 0xFF
          else
            bin_data_r <= (others => '0'); -- 0x00
          end if;
          addr_i      := to_integer(y_d) * IMG_WIDTH + to_integer(x_d);
          bin_addr_r  <= to_unsigned(addr_i, ADDR_WIDTH);
          bin_we_r    <= '1';
        end if;
      end if;
    end if;
  end process;

end architecture;
