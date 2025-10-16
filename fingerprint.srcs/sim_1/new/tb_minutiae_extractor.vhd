----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 18:15:19
-- Design Name: 
-- Module Name: tb_minutiae_extractor - Behavioral
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


-- tb_minutiae_extractor.vhd
-- VHDL-93 testbench for minutiae_extractor
-- Vivado 2024.1
--
-- - Reuses project's image_window_reader (instantiated inside minutiae_extractor)
-- - THIN_A/THIN_B provided by a simple behavioral BRAM model (tb_frame_bram)
-- - Feature output written into the real minutiae_bram (512x32)
-- - Creates a thin '+' plus a 'Y' junction; expects >=1 bifurcation and >=2 endings
-- - Dumps decoded records to minutiae_dump.txt
--
-- NOTE: Add 'minutiae_extractor.vhd', 'minutiae_bram.vhd', 'image_window_reader.vhd'
--       to your simulation sources alongside this testbench.
-- tb_minutiae_extractor.vhd
-- VHDL-93 testbench for minutiae_extractor
-- Vivado 2024.1

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

--------------------------------------------------------------------
-- Testbench first (ordering can help Vivado parsers)
--------------------------------------------------------------------
entity tb_minutiae_extractor is
end entity;

architecture sim of tb_minutiae_extractor is
  ----------------------------------------------------------------
  -- Image / address params
  ----------------------------------------------------------------
  constant DATA_W : integer := 8;
  constant ADDR_W : integer := 13;  -- 2^13 >= 68*118
  constant IMG_W  : integer := 68;
  constant IMG_H  : integer := 118;
  constant TOTAL  : integer := IMG_W * IMG_H;

  ----------------------------------------------------------------
  -- Clocks / reset
  ----------------------------------------------------------------
  signal clk : std_logic := '0';
  signal rst : std_logic := '1';

  ----------------------------------------------------------------
  -- DUT handshakes
  ----------------------------------------------------------------
  signal s_min_start   : std_logic := '0';
  signal s_min_busy    : std_logic;
  signal s_min_done    : std_logic;
  signal src_is_a      : std_logic := '1';  -- we preload THIN_A, so select A

  ----------------------------------------------------------------
  -- THIN_A (behavioral): write by TB, read by DUT, monitor by TB
  ----------------------------------------------------------------
  signal thinA_we       : std_logic := '0';
  signal thinA_waddr    : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinA_din      : std_logic_vector(DATA_W-1 downto 0) := (others => '0');

  signal thinA_rd_en    : std_logic;
  signal thinA_rd_addr  : unsigned(ADDR_W-1 downto 0);
  signal thinA_rd_dout  : std_logic_vector(DATA_W-1 downto 0);

  signal thinA_mon_en   : std_logic := '0';
  signal thinA_mon_addr : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinA_mon_dout : std_logic_vector(DATA_W-1 downto 0);

  ----------------------------------------------------------------
  -- THIN_B (behavioral): present for completeness (kept zero)
  ----------------------------------------------------------------
  signal thinB_we       : std_logic := '0';
  signal thinB_waddr    : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinB_din      : std_logic_vector(DATA_W-1 downto 0) := (others => '0');

  signal thinB_rd_en    : std_logic;
  signal thinB_rd_addr  : unsigned(ADDR_W-1 downto 0);
  signal thinB_rd_dout  : std_logic_vector(DATA_W-1 downto 0);

  signal thinB_mon_en   : std_logic := '0';
  signal thinB_mon_addr : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinB_mon_dout : std_logic_vector(DATA_W-1 downto 0);

  ----------------------------------------------------------------
  -- Feature BRAM (real minutiae_bram)
  ----------------------------------------------------------------
  signal feat_we      : std_logic;
  signal feat_addr    : unsigned(8 downto 0);
  signal feat_din     : std_logic_vector(31 downto 0);

  signal feat_doutA   : std_logic_vector(31 downto 0);  -- optional
  signal feat_doutB   : std_logic_vector(31 downto 0);

  signal feat_count   : unsigned(9 downto 0);
  signal feat_overflow: std_logic;

  -- TB-side read of feature BRAM (Port-B)
  signal feat_enB     : std_logic := '0';
  signal feat_addrB   : unsigned(8 downto 0) := (others => '0');

  ----------------------------------------------------------------
  -- TB flags
  ----------------------------------------------------------------
  signal tb_failed : std_logic := '0';
  signal tb_done   : std_logic := '0';

  ----------------------------------------------------------------
  -- Text outputs
  ----------------------------------------------------------------
  file dump_file : text open write_mode is "minutiae_dump.txt";

  ----------------------------------------------------------------
  -- Helpers
  ----------------------------------------------------------------
  -- linear address
  function at(x, y : integer) return integer is
  begin
    return y*IMG_W + x;
  end function;

begin
  ----------------------------------------------------------------
  -- Clock
  ----------------------------------------------------------------
  clk <= not clk after 10 ns;  -- 50 MHz

  ----------------------------------------------------------------
  -- THIN_A / THIN_B BRAM models
  ----------------------------------------------------------------
  thinA_bram : entity work.tb_frame_bram
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W
    )
    port map (
      clk      => clk,
      we       => thinA_we,
      waddr    => thinA_waddr,
      din      => thinA_din,
      rd0_en   => thinA_rd_en,
      rd0_addr => thinA_rd_addr,
      rd0_dout => thinA_rd_dout,
      rd1_en   => thinA_mon_en,
      rd1_addr => thinA_mon_addr,
      rd1_dout => thinA_mon_dout
    );

  thinB_bram : entity work.tb_frame_bram
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W
    )
    port map (
      clk      => clk,
      we       => thinB_we,
      waddr    => thinB_waddr,
      din      => thinB_din,
      rd0_en   => thinB_rd_en,
      rd0_addr => thinB_rd_addr,
      rd0_dout => thinB_rd_dout,
      rd1_en   => thinB_mon_en,
      rd1_addr => thinB_mon_addr,
      rd1_dout => thinB_mon_dout
    );

  ----------------------------------------------------------------
  -- Feature BRAM (synthesizable dual-port)
  ----------------------------------------------------------------
  feat_bram : entity work.minutiae_bram
    generic map (
      DATA_WIDTH => 32,
      ADDR_WIDTH => 9  -- 512 deep
    )
    port map (
      -- Port A: written by extractor
      clkA  => clk,
      weA   => feat_we,
      addrA => feat_addr,
      dinA  => feat_din,
      doutA => feat_doutA,

      -- Port B: read by TB
      clkB  => clk,
      enB   => feat_enB,
      addrB => feat_addrB,
      doutB => feat_doutB
    );

  ----------------------------------------------------------------
  -- DUT: minutiae_extractor
  ----------------------------------------------------------------
  dut : entity work.minutiae_extractor
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W,
      IMG_WIDTH  => IMG_W,
      IMG_HEIGHT => IMG_H,
      FEAT_DEPTH => 512
    )
    port map (
      clk           => clk,
      rst           => rst,
      s_min_start   => s_min_start,
      s_min_busy    => s_min_busy,
      s_min_done    => s_min_done,
      src_is_a      => src_is_a,           -- we use THIN_A in this TB

      -- THIN_A / THIN_B Port-B (read)
      thinA_rd_en   => thinA_rd_en,
      thinA_rd_addr => thinA_rd_addr,
      thinA_rd_dout => thinA_rd_dout,
      thinB_rd_en   => thinB_rd_en,
      thinB_rd_addr => thinB_rd_addr,
      thinB_rd_dout => thinB_rd_dout,

      -- Feature output
      feat_we       => feat_we,
      feat_addr     => feat_addr,
      feat_din      => feat_din,
      feat_count    => feat_count,
      feat_overflow => feat_overflow
    );

  ----------------------------------------------------------------
  -- Stimulus
  ----------------------------------------------------------------
  stimulus : process
    variable L  : line;
    variable xx, yy : integer;

    -- Decode helpers
    variable rec      : std_logic_vector(31 downto 0);
    variable v_valid  : std_logic;
    variable v_type   : std_logic_vector(1 downto 0);
    variable v_x      : unsigned(9 downto 0);
    variable v_y      : unsigned(8 downto 0);
    variable v_cn     : unsigned(3 downto 0);
    variable v_n      : unsigned(3 downto 0);

    -- Counters
    variable cnt_total : integer := 0;
    variable cnt_end   : integer := 0;
    variable cnt_bif   : integer := 0;

    -- Center for the 'Y'
    variable x0 : integer := IMG_W/3;
    variable y0 : integer := IMG_H/3;

    -- synchronous BRAM read (2-cycle)
    procedure feat_read(
      signal enB   : out std_logic;
      signal addrB : out unsigned(8 downto 0);
      signal doutB : in  std_logic_vector(31 downto 0);
      constant a   : in  integer;
      variable rec : out std_logic_vector(31 downto 0)
    ) is
    begin
      enB   <= '1';
      addrB <= to_unsigned(a, 9);
      wait until rising_edge(clk);
      enB   <= '1';
      addrB <= to_unsigned(a, 9);
      wait until rising_edge(clk);
      rec := doutB;
      enB <= '0';
    end procedure;

    -- write a single skeleton pixel into THIN_A
    procedure write_pix_A(constant x, y : in integer) is
    begin
      thinA_din   <= (others => '1');
      thinA_waddr <= to_unsigned(at(x,y), ADDR_W);
      thinA_we    <= '1';
      wait until rising_edge(clk);
      thinA_we    <= '0';
      wait until rising_edge(clk);
    end procedure;

  begin
    -- Reset
    rst <= '1';
    wait for 200 ns;
    rst <= '0';
    wait for 100 ns;

    ----------------------------------------------------------------
    -- Build a thin '+' centered, and a 'Y' junction off-center
    -- All 1-pixel wide, far from borders (reader streams interior only)
    ----------------------------------------------------------------
    -- '+'
    for yy in 20 to IMG_H-21 loop
      write_pix_A(IMG_W/2, yy);
    end loop;
    for xx in 10 to IMG_W-11 loop
      write_pix_A(xx, IMG_H/2);
    end loop;

    -- 'Y' junction centered around (x0,y0)
    -- vertical stem downward
    for yy in y0 to y0+15 loop
      write_pix_A(x0, yy);
    end loop;
    -- arm 1: up-right diagonal
    for ii in 0 to 14 loop
      write_pix_A(x0+ii, y0-ii);
    end loop;
    -- arm 2: up-left diagonal
    for ii in 0 to 14 loop
      write_pix_A(x0-ii, y0-ii);
    end loop;

    -- Done writing skeleton; select A as source
    src_is_a <= '1';

    ----------------------------------------------------------------
    -- Start extraction
    ----------------------------------------------------------------
    s_min_start <= '1';
    wait until rising_edge(clk);
    s_min_start <= '0';

    -- Wait for completion
    wait until s_min_done = '1';
    wait until rising_edge(clk);

    ----------------------------------------------------------------
    -- Read back all features and check
    ----------------------------------------------------------------
    cnt_total := to_integer(feat_count);
    if cnt_total > 512 then
      cnt_total := 512;  -- safety (shouldn't happen; feat_count saturates)
    end if;

    -- Header for dump
    write(L, string'("# idx  valid  type  x    y    CN  N")); writeline(dump_file, L);

    for i in 0 to cnt_total-1 loop
      feat_read(feat_enB, feat_addrB, feat_doutB, i, rec);

      -- parse fields
      v_valid := rec(31);
      v_type  := rec(30 downto 29);
      v_x     := unsigned(rec(28 downto 19));
      v_y     := unsigned(rec(18 downto 10));
      v_cn    := unsigned(rec(9 downto 6));
      v_n     := unsigned(rec(5 downto 2));

      -- dump row (simple formatting for VHDL-93)
      write(L, string'("  "));
      write(L, i, right, 4);
      if v_valid = '1' then
        write(L, string'("    1     "));
      else
        write(L, string'("    0     "));
      end if;

      if v_type = "00" then
        write(L, string'("END  "));
        cnt_end := cnt_end + 1;
      elsif v_type = "01" then
        write(L, string'("BIF  "));
        cnt_bif := cnt_bif + 1;
      elsif v_type = "10" then
        write(L, string'("OTR  "));
      else
        write(L, string'("RES  "));
      end if;

      write(L, to_integer(v_x), right, 5);
      write(L, to_integer(v_y), right, 6);
      write(L, to_integer(v_cn), right, 5);
      write(L, to_integer(v_n), right, 4);
      writeline(dump_file, L);

      -- validity checks
      if v_valid /= '1' then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] valid bit is 0" severity error;
      end if;
      if to_integer(v_x) > IMG_W-1 then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] x out of range" severity error;
      end if;
      if to_integer(v_y) > IMG_H-1 then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] y out of range" severity error;
      end if;
      if to_integer(v_cn) > 8 then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] CN out of [0..8]" severity error;
      end if;
      if to_integer(v_n) > 8 then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] N out of [0..8]" severity error;
      end if;
      if not ( (v_type="00") or (v_type="01") or (v_type="10") or (v_type="11") ) then
        tb_failed <= '1';
        report "FAIL: record[" & integer'image(i) & "] type invalid" severity error;
      end if;
    end loop;

    -- High-level expectations:
    if cnt_bif < 1 then
      tb_failed <= '1';
      report "FAIL: expected at least 1 bifurcation, found " & integer'image(cnt_bif) severity error;
    end if;
    if cnt_end < 2 then
      tb_failed <= '1';
      report "FAIL: expected at least 2 endings, found " & integer'image(cnt_end) severity error;
    end if;

    -- Overflow should be false for this small pattern
    if feat_overflow = '1' then
      tb_failed <= '1';
      report "FAIL: feat_overflow asserted unexpectedly" severity error;
    end if;

    -- Final status
    if tb_failed = '0' then
      tb_done <= '1';
      report "PASS: minutiae_extractor test OK (total=" & integer'image(cnt_total) &
             ", end=" & integer'image(cnt_end) & ", bif=" & integer'image(cnt_bif) & ")" severity note;
    else
      report "FAILED: minutiae_extractor test" severity failure;
    end if;

    wait;
  end process;

end architecture;

--------------------------------------------------------------------
-- Simple dual-port BRAM model (1 write + 2 read ports)
-- Put AFTER the TB (ordering helps Vivado in some setups)
--------------------------------------------------------------------
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

      -- pipeline read addresses/enables (1-cycle latency)
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

