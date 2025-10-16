----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 16.09.2025 11:33:07
-- Design Name: 
-- Module Name: tb_binarizer_subsystem - Behavioral
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

-- tb_binarizer_subsystem.vhd

-- - Drives a synthetic 68x118 grayscale frame into data_bram via its UART-like interface
-- - Runs the 2-pass binarizer_subsystem (threshold builder + stream binarizer)
-- - Reads the binary image via the VGA-style port and writes a PGM file (binary_out.pgm)
--
-- NOTE: This file includes a simple behavioral architecture for 'data_bram' at the end
--       so you can run immediately. If you already have your own 'data_bram' implementation,
--       remove/comment that architecture and use your compiled one.

library ieee;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;
use std.textio.all;

entity tb_binarizer_subsystem is
end entity;

architecture sim of tb_binarizer_subsystem is
  ---------------------------------------------------------------------------
  -- Generics / parameters
  ---------------------------------------------------------------------------
  constant DATA_WIDTH : integer := 8;
  constant ADDR_WIDTH : integer := 13;  -- 2^13 = 8192 > 8024
  constant IMG_WIDTH  : integer := 68;
  constant IMG_HEIGHT : integer := 118;
  constant TOTAL_PIX  : integer := IMG_WIDTH * IMG_HEIGHT;

  ---------------------------------------------------------------------------
  -- Writer (UART-like) clock domain
  ---------------------------------------------------------------------------
  signal clk_wr  : std_logic := '0';
  signal rst_wr  : std_logic := '1';

  -- data_bram UART-side inputs
  signal data_reg_s       : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal done_reg_s       : std_logic := '0';
  signal parity_err_s     : std_logic := '0';
  signal frame_err_s      : std_logic := '0';
  signal frame_start_s    : std_logic := '0';
  signal frame_done_s     : std_logic;

  ---------------------------------------------------------------------------
  -- Reader/processing clock domain
  ---------------------------------------------------------------------------
  signal clk_rd  : std_logic := '0';
  signal rst_rd  : std_logic := '1';

  -- data_bram Port-B (to binarizer_subsystem)
  signal db_rd_en_s   : std_logic;
  signal db_rd_addr_s : unsigned(ADDR_WIDTH-1 downto 0);
  signal db_rd_dout_s : std_logic_vector(DATA_WIDTH-1 downto 0);

  ---------------------------------------------------------------------------
  -- Binarizer subsystem <-> VGA port (for reading output image)
  ---------------------------------------------------------------------------
  signal bin_vga_en_s   : std_logic := '0';
  signal bin_vga_addr_s : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bin_vga_dout_s : std_logic_vector(7 downto 0);

  -- Subsystem status
  signal bin_busy_s : std_logic;
  signal bin_done_s : std_logic;

  ---------------------------------------------------------------------------
  -- File I/O for writing PGM of the binary result
  ---------------------------------------------------------------------------
  file pgm_file : text open write_mode is "binary_out.pgm";

begin
  ---------------------------------------------------------------------------
  -- Clocks (50 MHz each, independent domains)
  ---------------------------------------------------------------------------
  clk_wr <= not clk_wr after 10 ns;  -- 20 ns period
  clk_rd <= not clk_rd after 10 ns;  -- 20 ns period

  ---------------------------------------------------------------------------
  -- Resets
  ---------------------------------------------------------------------------
  process
  begin
    rst_wr <= '1';
    rst_rd <= '1';
    wait for 200 ns;
    rst_wr <= '0';
    rst_rd <= '0';
    wait;
  end process;

  ---------------------------------------------------------------------------
  -- DUT: data_bram (behavioral architecture provided later in this file)
  ---------------------------------------------------------------------------
  data_bram_inst : entity work.data_bram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT
    )
    port map (
      clk            => clk_wr,
      rst            => rst_wr,
      data_reg       => data_reg_s,
      done_reg       => done_reg_s,
      parity_err_reg => parity_err_s,
      frame_err_reg  => frame_err_s,
      frame_start    => frame_start_s,
      frame_done     => frame_done_s,
      rd_clk         => clk_rd,
      rd_en          => db_rd_en_s,
      rd_addr        => db_rd_addr_s,
      rd_dout        => db_rd_dout_s
    );

  ---------------------------------------------------------------------------
  -- DUT: binarizer_subsystem (thresholds + stream binarizer + binary BRAM)
  ---------------------------------------------------------------------------
  binarizer_ss_inst : entity work.binarizer_subsystem
    generic map (
      DATA_WIDTH    => DATA_WIDTH,
      ADDR_WIDTH    => ADDR_WIDTH,
      IMG_WIDTH     => IMG_WIDTH,
      IMG_HEIGHT    => IMG_HEIGHT,
      BLOCK_SIZE    => 8,
      THRESH_OFFSET => 12
    )
    port map (
      rd_clk       => clk_rd,
      rst          => rst_rd,
      start        => frame_done_s,     -- kick pass1 after frame is loaded
      db_rd_en     => db_rd_en_s,
      db_rd_addr   => db_rd_addr_s,
      db_rd_dout   => db_rd_dout_s,
      bin_vga_clk  => clk_rd,
      bin_vga_en   => bin_vga_en_s,
      bin_vga_addr => bin_vga_addr_s,
      bin_vga_dout => bin_vga_dout_s,
      busy         => bin_busy_s,
      done_pulse   => bin_done_s
    );

  ---------------------------------------------------------------------------
  -- Stimulus: generate a synthetic frame and "send" it via UART-like inputs
  -- Pattern: grayscale gradient with gentle modulation to exercise thresholds.
  -- Order: row-major. We assert frame_start, then pulse done_reg per pixel.
  ---------------------------------------------------------------------------
  writer_proc : process
    variable x, y : integer;
    impure function pix_val(xx, yy : integer) return integer is
      -- Simple pattern: (4*x + 2*y) mod 256, with small stripe to vary blocks
      variable v : integer := (4*xx + 2*yy) mod 256;
    begin
      if ((xx/8) mod 2) = 1 then
        v := (v + 24) mod 256;
      end if;
      return v;
    end function;
  begin
    -- Wait until resets deassert
    wait until rst_wr = '0';
    wait for 50 ns;

    -- Start a new frame
    frame_start_s <= '1';
    wait until rising_edge(clk_wr);
    frame_start_s <= '0';

    -- Stream all pixels (row-major) using done_reg pulses
    for yy in 0 to IMG_HEIGHT-1 loop
      for xx in 0 to IMG_WIDTH-1 loop
        data_reg_s <= std_logic_vector(to_unsigned(pix_val(xx, yy), DATA_WIDTH));
        done_reg_s <= '1';
        wait until rising_edge(clk_wr);
        done_reg_s <= '0';
        -- idle a cycle between bytes (optional)
        wait until rising_edge(clk_wr);
      end loop;
    end loop;

    -- Wait for frame_done from data_bram to ripple to binarizer (start)
    wait until frame_done_s = '1';
    -- Wait a bit so subsystem picks up the start
    wait for 200 ns;

    -- Wait for binarization to complete
    wait until bin_done_s = '1';
    report "Binarization done.";

  

    -- One extra cycle to flush the very last sample (optional)
    wait until rising_edge(clk_rd);

    report "PGM write complete. Simulation will stop.";
    wait for 500 ns;
    std.env.stop;
    wait;
  end process;

end architecture;
