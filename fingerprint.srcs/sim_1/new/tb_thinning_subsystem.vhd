----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 20.09.2025 20:22:39
-- Design Name: 
-- Module Name: tb_thinning_subsystem - Behavioral
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

entity tb_thinning_subsystem is 
end entity;

architecture sim of tb_thinning_subsystem is
  constant DATA_W : integer := 8;
  constant ADDR_W : integer := 13;
  constant IMG_W  : integer := 68;
  constant IMG_H  : integer := 118;
  constant TOTAL  : integer := IMG_W * IMG_H;

  -- clock & reset 
  signal clk : std_logic := '0';
  signal rst : std_logic := '1';

  -- DUT handshakes
  signal s_thin_start : std_logic := '0';
  signal s_thin_busy  : std_logic;
  signal s_thin_done  : std_logic;

  -- Binary BRAM ports
  signal bin_we      : std_logic := '0';
  signal bin_waddr   : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal bin_din     : std_logic_vector(DATA_W-1 downto 0) := (others => '0');
  signal bin_rd_en   : std_logic;
  signal bin_rd_addr : unsigned(ADDR_W-1 downto 0);
  signal bin_rd_dout : std_logic_vector(DATA_W-1 downto 0);
  signal bin_mon_en   : std_logic := '0';
  signal bin_mon_addr : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal bin_mon_dout : std_logic_vector(DATA_W-1 downto 0);

  -- THIN_A BRAM ports
  signal thinA_we      : std_logic;
  signal thinA_addr    : unsigned(ADDR_W-1 downto 0);
  signal thinA_din     : std_logic_vector(DATA_W-1 downto 0);
  signal thinA_rd_en   : std_logic;
  signal thinA_rd_addr : unsigned(ADDR_W-1 downto 0);
  signal thinA_rd_dout : std_logic_vector(DATA_W-1 downto 0);
  signal thinA_mon_en   : std_logic := '0';
  signal thinA_mon_addr : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinA_mon_dout : std_logic_vector(DATA_W-1 downto 0);

  -- THIN_B BRAM ports
  signal thinB_we      : std_logic;
  signal thinB_addr    : unsigned(ADDR_W-1 downto 0);
  signal thinB_din     : std_logic_vector(DATA_W-1 downto 0);
  signal thinB_rd_en   : std_logic;
  signal thinB_rd_addr : unsigned(ADDR_W-1 downto 0);
  signal thinB_rd_dout : std_logic_vector(DATA_W-1 downto 0);
  signal thinB_mon_en   : std_logic := '0';
  signal thinB_mon_addr : unsigned(ADDR_W-1 downto 0) := (others => '0');
  signal thinB_mon_dout : std_logic_vector(DATA_W-1 downto 0);

  -- TB status
  signal tb_failed : std_logic := '0';
  signal tb_done   : std_logic := '0';

  file pgm_file : text open write_mode is "thin_out.pgm";

  -- helper: linear address
  function at(x, y : integer) return integer is
  begin
    return y*IMG_W + x;
  end function;

begin
  -- clock gen
  clk <= not clk after 10 ns;

  -- BRAM instances with named port maps
  bin_bram : entity work.tb_frame_bram
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W
    )
    port map (
      clk      => clk,
      we       => bin_we,
      waddr    => bin_waddr,
      din      => bin_din,
      rd0_en   => bin_rd_en,
      rd0_addr => bin_rd_addr,
      rd0_dout => bin_rd_dout,
      rd1_en   => bin_mon_en,
      rd1_addr => bin_mon_addr,
      rd1_dout => bin_mon_dout
    );

  thinA_bram : entity work.tb_frame_bram
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W
    )
    port map (
      clk      => clk,
      we       => thinA_we,
      waddr    => thinA_addr,
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
      waddr    => thinB_addr,
      din      => thinB_din,
      rd0_en   => thinB_rd_en,
      rd0_addr => thinB_rd_addr,
      rd0_dout => thinB_rd_dout,
      rd1_en   => thinB_mon_en,
      rd1_addr => thinB_mon_addr,
      rd1_dout => thinB_mon_dout
    );

  -- DUT
  dut : entity work.thinning_subsystem
    generic map (
      DATA_WIDTH => DATA_W,
      ADDR_WIDTH => ADDR_W,
      IMG_WIDTH  => IMG_W,
      IMG_HEIGHT => IMG_H,
      MAX_ITERS  => 8
    )
    port map (
      clk           => clk,
      rst           => rst,
      s_thin_start  => s_thin_start,
      s_thin_busy   => s_thin_busy,
      s_thin_done   => s_thin_done,
      bin_rd_en     => bin_rd_en,
      bin_rd_addr   => bin_rd_addr,
      bin_rd_dout   => bin_rd_dout,
      thinA_rd_en   => thinA_rd_en,
      thinA_rd_addr => thinA_rd_addr,
      thinA_rd_dout => thinA_rd_dout,
      thinB_rd_en   => thinB_rd_en,
      thinB_rd_addr => thinB_rd_addr,
      thinB_rd_dout => thinB_rd_dout,
      thinA_we      => thinA_we,
      thinA_addr    => thinA_addr,
      thinA_din     => thinA_din,
      thinB_we      => thinB_we,
      thinB_addr    => thinB_addr,
      thinB_din     => thinB_din
    );

  -- Stimulus process
  stimulus : process
    variable L : line;
    variable xx, yy, ones : integer;
    type u8arr is array (0 to TOTAL-1) of std_logic_vector(DATA_W-1 downto 0);
    variable snap_bin, snap_thn : u8arr;
    variable v : std_logic_vector(DATA_W-1 downto 0);

    procedure mon_read(
      signal en   : out std_logic;
      signal addr : out unsigned(ADDR_W-1 downto 0);
      signal dout : in  std_logic_vector(DATA_W-1 downto 0);
      constant a  : in  integer;
      variable v  : out std_logic_vector(DATA_W-1 downto 0)
    ) is
    begin
      en <= '1';
      addr <= to_unsigned(a, ADDR_W);
      wait until rising_edge(clk);
      en <= '1';
      addr <= to_unsigned(a, ADDR_W);
      wait until rising_edge(clk);
      v := dout;
      en <= '0';
    end procedure;

  begin
    -- reset
    rst <= '1';
    wait for 200 ns;
    rst <= '0';
    wait for 100 ns;

    -- build test image
    for yy in 0 to IMG_H-1 loop
      for xx in 0 to IMG_W-1 loop
        bin_din <= (others => '0');
        if (abs(xx - IMG_W/2) <= 2) or (abs(yy - IMG_H/2) <= 2) then
          bin_din <= (others => '1');
        end if;
        if abs(yy - (xx*IMG_H)/IMG_W) <= 1 then
          bin_din <= (others => '1');
        end if;
        bin_waddr <= to_unsigned(at(xx,yy), ADDR_W);
        bin_we    <= '1';
        wait until rising_edge(clk);
        bin_we    <= '0';
        wait until rising_edge(clk);
      end loop;
    end loop;

    -- snapshot original
    for i in 0 to TOTAL-1 loop
      mon_read(bin_mon_en, bin_mon_addr, bin_mon_dout, i, v);
      snap_bin(i) := v;
    end loop;

    -- start thinning
    s_thin_start <= '1';
    wait until rising_edge(clk);
    s_thin_start <= '0';

    -- wait done
    wait until s_thin_done = '1';
    wait until rising_edge(clk);

    -- read final skeleton (ends in THIN_B)
    for i in 0 to TOTAL-1 loop
      mon_read(thinB_mon_en, thinB_mon_addr, thinB_mon_dout, i, v);
      snap_thn(i) := v;
    end loop;

    -- write PGM
    write(L, string'("P2")); writeline(pgm_file, L);
    write(L, string'("# thinning result")); writeline(pgm_file, L);
    write(L, IMG_W); write(L, string'(" ")); write(L, IMG_H); writeline(pgm_file, L);
    write(L, 255); writeline(pgm_file, L);
    for yy in 0 to IMG_H-1 loop
      for xx in 0 to IMG_W-1 loop
        if snap_thn(at(xx,yy)) = (DATA_W-1 downto 0 => '0') then
          write(L, 0);
        else
          write(L, 255);
        end if;
        if xx < IMG_W-1 then
          write(L, string'(" "));
        end if;
      end loop;
      writeline(pgm_file, L);
    end loop;

    -- checks
    ones := 0;
    for i in 0 to TOTAL-1 loop
      if (snap_thn(i) /= (DATA_W-1 downto 0 => '0')) and
         (snap_bin(i) = (DATA_W-1 downto 0 => '0')) then
        tb_failed <= '1';
        report "FAIL: skeleton has 1 where input had 0 at " & integer'image(i) severity error;
      end if;
      if snap_thn(i) /= (DATA_W-1 downto 0 => '0') then
        ones := ones + 1;
      end if;
    end loop;

    if ones < 20 then
      tb_failed <= '1';
      report "FAIL: skeleton empty (ones=" & integer'image(ones) & ")" severity error;
    end if;

    if tb_failed = '0' then
      tb_done <= '1';
      report "PASS: thinning_subsystem test OK (ones=" & integer'image(ones) & ")" severity note;
    else
      report "FAILED: thinning_subsystem test" severity failure;
    end if;

    wait;
  end process;
end architecture;








