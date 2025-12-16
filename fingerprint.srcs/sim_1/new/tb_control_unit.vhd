----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.09.2025 23:42:28
-- Design Name: 
-- Module Name: tb_control_unit - Behavioral
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

--------------------------------------------------------------------------------
-- Testbench for control_unit
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_control_unit is
end entity;

architecture sim of tb_control_unit is
  constant CLK_PERIOD : time := 20 ns;

  -- DUT I/O
  signal clk, rst_n       : std_logic := '0';

  -- UI
  signal sw_mode          : std_logic := '0'; -- 0=enroll, 1=match
  signal sw_vga_sel       : std_logic_vector(1 downto 0) := (others => '0');
  signal sw_tmpl_id       : std_logic_vector(5 downto 0) := (others => '0');
  signal sw_single        : std_logic := '0';
  signal btn_start        : std_logic := '0';
  signal btn_pause        : std_logic := '0';
  signal btn_step         : std_logic := '0';

  signal frame_ready      : std_logic := '1';

  -- pipeline stage handshakes
  signal bin_busy, bin_done_p : std_logic := '0';
  signal thin_busy, thin_done_p : std_logic := '0';
  signal min_busy, min_done_p : std_logic := '0';

  -- from minutiae
  signal min_feat_count   : unsigned(9 downto 0) := (others => '0');
  signal min_overflow     : std_logic := '0';

  -- outputs
  signal proc_ce          : std_logic;
  signal bin_start_p      : std_logic;
  signal thin_start_p     : std_logic;
  signal min_start_p      : std_logic;

  -- CPU mailbox
  signal cu_cmd_wr        : std_logic;
  signal cu_cmd           : std_logic_vector(7 downto 0);
  signal cu_cmd_arg0_wr   : std_logic;
  signal cu_cmd_arg0      : std_logic_vector(31 downto 0);
  signal cu_probe_len_wr  : std_logic;
  signal cu_probe_len     : std_logic_vector(31 downto 0);
  signal cu_irq_set       : std_logic;

  signal cu_status        : std_logic_vector(31 downto 0) := (others => '0');
  signal cu_result_slot   : std_logic_vector(31 downto 0) := (others => '0');
  signal cu_result_score  : std_logic_vector(31 downto 0) := (others => '0');

  -- bookkeeping
  signal tb_failed : boolean := false;

begin
  --------------------------------------------------------------------
  -- Clock
  --------------------------------------------------------------------
  clk <= not clk after CLK_PERIOD/2;

  --------------------------------------------------------------------
  -- DUT
  --------------------------------------------------------------------
  dut : entity work.control_unit
    generic map (
      IMG_WIDTH => 68,
      CPU_STATUS_DONE_BIT => 0
    )
    port map (
      clk              => clk,
      rst_n            => rst_n,
      sw_mode          => sw_mode,
      sw_vga_sel       => sw_vga_sel,
      sw_tmpl_id       => sw_tmpl_id,
      btn_start        => btn_start,
      btn_pause        => btn_pause,
      btn_step         => btn_step,
      frame_ready      => frame_ready,
      sw_single        => sw_single,
      bin_busy         => bin_busy,
      bin_done_p       => bin_done_p,
      thin_busy        => thin_busy,
      thin_done_p      => thin_done_p,
      min_busy         => min_busy,
      min_done_p       => min_done_p,
      min_feat_count   => min_feat_count,
      min_overflow     => min_overflow,
      proc_ce          => proc_ce,
      bin_start_p      => bin_start_p,
      thin_start_p     => thin_start_p,
      min_start_p      => min_start_p,
      cu_cmd_wr        => cu_cmd_wr,
      cu_cmd           => cu_cmd,
      cu_cmd_arg0_wr   => cu_cmd_arg0_wr,
      cu_cmd_arg0      => cu_cmd_arg0,
      cu_probe_len_wr  => cu_probe_len_wr,
      cu_probe_len     => cu_probe_len,
      cu_irq_set       => cu_irq_set,
      cu_status        => cu_status
      --cu_result_slot   => cu_result_slot,
      --cu_result_score  => cu_result_score
    );

  --------------------------------------------------------------------
  -- Stimulus
  --------------------------------------------------------------------
  stim : process
  begin
    -- Reset
    rst_n <= '0';
    wait for 5*CLK_PERIOD;
    rst_n <= '1';
    wait for 5*CLK_PERIOD;

    ----------------------------------------------------------------
    -- Enroll mode
    ----------------------------------------------------------------
    report "TEST: ENROLL mode" severity note;
    sw_mode <= '0'; -- enroll
    sw_tmpl_id <= "000101"; -- slot 5
    btn_start <= '1'; wait for CLK_PERIOD; btn_start <= '0';

    -- Stage 1: BINARIZER
    wait for 2*CLK_PERIOD;
    assert bin_start_p = '1'
      report "FAIL: bin_start_p not asserted at start of ENROLL" severity error;
    bin_busy <= '1'; wait for 5*CLK_PERIOD;
    bin_busy <= '0'; bin_done_p <= '1'; wait for CLK_PERIOD; bin_done_p <= '0';

    -- Stage 2: THINNING
    wait for 2*CLK_PERIOD;
    assert thin_start_p = '1'
      report "FAIL: thin_start_p not asserted after bin_done_p" severity error;
    thin_busy <= '1'; wait for 5*CLK_PERIOD;
    thin_busy <= '0'; thin_done_p <= '1'; wait for CLK_PERIOD; thin_done_p <= '0';

    -- Stage 3: MINUTIAE
    wait for 2*CLK_PERIOD;
    assert min_start_p = '1'
      report "FAIL: min_start_p not asserted after thin_done_p" severity error;
    min_busy <= '1'; min_feat_count <= to_unsigned(123,10); wait for 5*CLK_PERIOD;
    min_busy <= '0'; min_done_p <= '1'; wait for CLK_PERIOD; min_done_p <= '0';

    -- CPU mailbox should be written
    wait for 5*CLK_PERIOD;
    assert cu_cmd_wr = '1'
      report "FAIL: CU did not issue command in ENROLL mode" severity error;

    ----------------------------------------------------------------
    -- Match mode
    ----------------------------------------------------------------
    report "TEST: MATCH mode" severity note;
    sw_mode <= '1'; -- match
    --btn_start <= '1'; wait for CLK_PERIOD; btn_start <= '0';

    wait for 2*CLK_PERIOD;
    assert bin_start_p = '1'
      report "FAIL: bin_start_p not asserted at start of MATCH" severity error;
    bin_busy <= '1'; wait for 5*CLK_PERIOD;
    bin_busy <= '0'; bin_done_p <= '1'; wait for CLK_PERIOD; bin_done_p <= '0';

    wait for 2*CLK_PERIOD;
    assert thin_start_p = '1'
      report "FAIL: thin_start_p not asserted after bin_done_p (MATCH)" severity error;
    thin_busy <= '1'; wait for 5*CLK_PERIOD;
    thin_busy <= '0'; thin_done_p <= '1'; wait for CLK_PERIOD; thin_done_p <= '0';

    wait for 2*CLK_PERIOD;
    assert min_start_p = '1'
      report "FAIL: min_start_p not asserted after thin_done_p (MATCH)" severity error;
    min_busy <= '1'; min_feat_count <= to_unsigned(77,10); wait for 5*CLK_PERIOD;
    min_busy <= '0'; min_done_p <= '1'; wait for CLK_PERIOD; min_done_p <= '0';

    wait for 5*CLK_PERIOD;
    assert cu_cmd_wr = '1'
      report "FAIL: CU did not issue command in MATCH mode" severity error;

    ----------------------------------------------------------------
    -- Wrap up
    ----------------------------------------------------------------
    report "ALL TESTS COMPLETED" severity note;
    wait;
  end process;
end architecture;
