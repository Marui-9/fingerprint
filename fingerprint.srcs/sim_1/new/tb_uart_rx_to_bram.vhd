----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 18.09.2025 21:38:57
-- Design Name: 
-- Module Name: tb_uart_rx_to_bram - Behavioral
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_uart_rx_to_bram is
end entity;

architecture tb of tb_uart_rx_to_bram is

  --------------------------------------------------------------------------
  -- TB configuration
  --------------------------------------------------------------------------
  -- Keep image small for sim speed
  constant TB_IMG_W      : integer := 8;   -- 8 x 8 = 64 bytes
  constant TB_IMG_H      : integer := 8;
  constant TB_FRAME_SIZE : integer := TB_IMG_W * TB_IMG_H; -- 64
  constant TB_ADDR_WIDTH : integer := 8;   -- depth 256 >= 64
  constant DATA_WIDTH    : integer := 8;

  -- UART/clock parameters
  constant CLK_FREQ   : integer := 50_000_000; -- 50 MHz
  constant BAUD_RATE  : integer := 9600;
  constant BIT_PERIOD : time    := 104166 ns;  -- ~1/9600 s (104.1666 us)

  --------------------------------------------------------------------------
  -- Signals
  --------------------------------------------------------------------------
  -- System clocks
  signal clk     : std_logic := '0';  -- 50 MHz (20 ns)
  signal rd_clk  : std_logic := '0';  -- 25 MHz (40 ns)

  -- Resets
  signal rst     : std_logic := '0';  -- for uart_to_bram
  signal reset   : std_logic := '0';  -- for uart_rx

  -- UART serial line
  signal rx      : std_logic := '1';  -- idle high

  -- uart_rx outputs
  signal rx_data_out   : std_logic_vector(7 downto 0);
  signal rx_done       : std_logic;
  signal rx_parity_err : std_logic;
  signal rx_frame_err  : std_logic;

  -- uart_to_bram controls
  signal frame_start   : std_logic := '0';
  signal frame_done    : std_logic;

  -- BRAM read port (Port B)
  signal rd_en   : std_logic := '0';
  signal rd_addr : unsigned(TB_ADDR_WIDTH-1 downto 0) := (others => '0');
  signal rd_dout : std_logic_vector(7 downto 0);

  -- Testbench status
  signal tb_finished : std_logic := '0';
  signal tb_fail     : std_logic := '0';
  signal tb_pass     : std_logic := '0';

  --------------------------------------------------------------------------
  -- Helpers: types/arrays
  --------------------------------------------------------------------------
  type slv8_array is array (natural range <>) of std_logic_vector(7 downto 0);

  -- Expected buffers
  signal exp_full_frame : slv8_array(0 to TB_FRAME_SIZE-1);
  signal exp_small      : slv8_array(0 to TB_FRAME_SIZE-1);

  --------------------------------------------------------------------------
  -- DUT declarations
  --------------------------------------------------------------------------
  component uart_rx
    generic (
      CLK_FREQ   : integer := 50_000_000;
      BAUD_RATE  : integer := 9600;
      OVERSAMPLE : integer := 16
    );
    port (
      clk        : in  std_logic;
      reset      : in  std_logic;
      rx         : in  std_logic;
      data_out   : out std_logic_vector(7 downto 0);
      done       : out std_logic;
      parity_err : out std_logic;
      frame_err  : out std_logic
    );
  end component;

  component uart_to_bram
    generic (
      DATA_WIDTH : integer := 8;
      ADDR_WIDTH : integer := 13;
      IMG_WIDTH  : integer := 68;
      IMG_HEIGHT : integer := 118
    );
    port (
      clk    : in  std_logic;
      rst    : in  std_logic;

      data_reg       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
      done_reg       : in  std_logic;
      parity_err_reg : in  std_logic;
      frame_err_reg  : in  std_logic;

      frame_start : in  std_logic;
      frame_done  : out std_logic;

      rd_clk   : in  std_logic;
      rd_en    : in  std_logic;
      rd_addr  : in  unsigned(ADDR_WIDTH-1 downto 0);
      rd_dout  : out std_logic_vector(DATA_WIDTH-1 downto 0)
    );
  end component;

  --------------------------------------------------------------------------
  -- Functions / Procedures (VHDL-93 safe: drive signals via signal parameters)
  --------------------------------------------------------------------------
  -- Even parity over 8 bits: parity bit = XOR of all data bits (even total)
  function even_parity(d : std_logic_vector(7 downto 0)) return std_logic is
    variable p : std_logic := '0';
    variable i : integer;
  begin
    for i in 0 to 7 loop
      p := p xor d(i);
    end loop;
    return p;
  end function;

  -- Transmit one UART frame on 'rx' (LSB first), with optional errors.
  -- Format: 1 start (0), 8 data, 1 parity, 1 stop (1).
  procedure uart_send_byte(
    signal rx_sig          : out std_logic;
    constant b             : in  std_logic_vector(7 downto 0);
    constant inject_par_er : in  boolean;
    constant inject_fr_er  : in  boolean
  ) is
    variable i  : integer;
    variable pb : std_logic;
  begin
    -- idle before start
    rx_sig <= '1';
    wait for BIT_PERIOD;

    -- start bit
    rx_sig <= '0';
    wait for BIT_PERIOD;

    -- data bits LSB first
    for i in 0 to 7 loop
      rx_sig <= b(i);
      wait for BIT_PERIOD;
    end loop;

    -- parity (even), optionally flipped
    pb := even_parity(b);
    if inject_par_er = true then
      pb := not pb;
    end if;
    rx_sig <= pb;
    wait for BIT_PERIOD;

    -- stop bit (1), optionally wrong (0)
    if inject_fr_er = true then
      rx_sig <= '0';
    else
      rx_sig <= '1';
    end if;
    wait for BIT_PERIOD;

    -- back to idle gap
    rx_sig <= '1';
    wait for BIT_PERIOD;
  end procedure;

  -- Wait for rx_done pulse with timeout in clock cycles
  procedure wait_for_done_with_timeout(
    signal clk_s   : in  std_logic;
    signal done_s  : in  std_logic;
    constant max_c : in  integer;
    constant tag   : in  string;
    signal fail_s  : inout std_logic
  ) is
    variable cnt : integer := 0;
  begin
    while done_s = '0' loop
      wait until rising_edge(clk_s);
      cnt := cnt + 1;
      if cnt > max_c then
        fail_s <= '1';
        assert false report "FAIL: timeout waiting for DONE - " & tag severity ERROR;
        exit;
      end if;
    end loop;
    -- ensure we pass the done pulse edge
    wait until rising_edge(clk_s);
  end procedure;

  -- Read Port-B address and check value after one rd_clk edge.
  procedure read_and_check(
    signal rd_addr_s : out unsigned;
    signal rd_en_s   : out std_logic;
    signal rd_clk_s  : in  std_logic;
    signal rd_dout_s : in  std_logic_vector;
    constant addr    : in  integer;
    constant expected: in  std_logic_vector(7 downto 0);
    constant label_s : in  string;
    signal fail_s    : inout std_logic
  ) is
  begin
    rd_addr_s <= to_unsigned(addr, rd_addr_s'length);
    rd_en_s   <= '1';
    wait until rising_edge(rd_clk_s);
    wait for 1 ns;
    rd_en_s   <= '0';
    if rd_dout_s /= expected then
      fail_s <= '1';
      assert false report
        "FAIL: " & label_s & " addr=" & integer'image(addr) &
        " expected=" & integer'image(to_integer(unsigned(expected))) &
        " got=" & integer'image(to_integer(unsigned(rd_dout_s)))
        severity ERROR;
    else
      report "PASS: " & label_s & " addr=" & integer'image(addr) &
             " val=" & integer'image(to_integer(unsigned(rd_dout_s)))
             severity NOTE;
    end if;
    wait until rising_edge(rd_clk_s);
  end procedure;

begin
  ----------------------------------------------------------------------------
  -- Instantiate DUTs
  ----------------------------------------------------------------------------
  U_RX : uart_rx
    generic map (
      CLK_FREQ   => CLK_FREQ,
      BAUD_RATE  => BAUD_RATE,
      OVERSAMPLE => 16
    )
    port map (
      clk        => clk,
      reset      => reset,
      rx         => rx,
      data_out   => rx_data_out,
      done       => rx_done,
      parity_err => rx_parity_err,
      frame_err  => rx_frame_err
    );

  U_WR : uart_to_bram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => TB_ADDR_WIDTH,
      IMG_WIDTH  => TB_IMG_W,
      IMG_HEIGHT => TB_IMG_H
    )
    port map (
      clk            => clk,
      rst            => rst,
      data_reg       => rx_data_out,
      done_reg       => rx_done,
      parity_err_reg => rx_parity_err,
      frame_err_reg  => rx_frame_err,
      frame_start    => frame_start,
      frame_done     => frame_done,
      rd_clk         => rd_clk,
      rd_en          => rd_en,
      rd_addr        => rd_addr,
      rd_dout        => rd_dout
    );

  ----------------------------------------------------------------------------
  -- Clocks
  ----------------------------------------------------------------------------
  clk_proc : process
  begin
    while now < 400 ms loop
      clk <= '0'; wait for 10 ns;   -- 50 MHz
      clk <= '1'; wait for 10 ns;
    end loop;
    wait;
  end process;

  rdclk_proc : process
  begin
    while now < 400 ms loop
      rd_clk <= '0'; wait for 20 ns; -- 25 MHz
      rd_clk <= '1'; wait for 20 ns;
    end loop;
    wait;
  end process;

  ----------------------------------------------------------------------------
  -- Stimulus
  ----------------------------------------------------------------------------
  stim : process
    variable i          : integer;
    variable valid_cnt  : integer;
    variable timeout    : integer;
  begin
    --------------------------------------------------------------------------
    -- Global reset
    --------------------------------------------------------------------------
    rst   <= '1';
    reset <= '1';
    rx    <= '1';
    wait for 500 ns;
    rst   <= '0';
    reset <= '0';
    wait for 5 us;

    --------------------------------------------------------------------------
    -- TEST 1: Normal full-frame (64 bytes)
    --------------------------------------------------------------------------
    report "TEST1: Normal frame capture start" severity NOTE;

    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    for i in 0 to TB_FRAME_SIZE-1 loop
      exp_full_frame(i) <= std_logic_vector(to_unsigned(i mod 256, 8));
      uart_send_byte(rx, exp_full_frame(i), false, false);
      -- Wait for DONE and ensure no errors flagged
      wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST1 DONE", tb_fail);
      if rx_parity_err = '1' then
        tb_fail <= '1';
        assert false report "FAIL: Unexpected parity_err during TEST1 at byte " &
                            integer'image(i) severity ERROR;
      end if;
      if rx_frame_err = '1' then
        tb_fail <= '1';
        assert false report "FAIL: Unexpected frame_err during TEST1 at byte " &
                            integer'image(i) severity ERROR;
      end if;
      -- Occasional concurrent read of previously written address
      if i = 16 then
        read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 8,  exp_full_frame(8),  "TEST1 concurrent read addr 8",  tb_fail);
      elsif i = 40 then
        read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 20, exp_full_frame(20), "TEST1 concurrent read addr 20", tb_fail);
      end if;
    end loop;

    -- Wait for frame_done pulse
    timeout := 0;
    while frame_done = '0' loop
      wait until rising_edge(clk);
      timeout := timeout + 1;
      if timeout > 10_000 then
        tb_fail <= '1';
        assert false report "FAIL: TEST1 frame_done timeout" severity ERROR;
        exit;
      end if;
    end loop;
    report "PASS: TEST1 frame_done observed" severity NOTE;

    -- Verify a handful + all bytes quickly
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 0,  exp_full_frame(0),  "TEST1 first",   tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 1,  exp_full_frame(1),  "TEST1 second",  tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 31, exp_full_frame(31), "TEST1 mid",     tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 63, exp_full_frame(63), "TEST1 last",    tb_fail);

    for i in 0 to TB_FRAME_SIZE-1 loop
      read_and_check(rd_addr, rd_en, rd_clk, rd_dout, i, exp_full_frame(i), "TEST1 full verify", tb_fail);
    end loop;

    --------------------------------------------------------------------------
    -- TEST 2: Parity error injection; ensure bad bytes skipped
    --------------------------------------------------------------------------
    report "TEST2: Parity error injection" severity NOTE;

    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    valid_cnt := 0;
    i := 0;
    while valid_cnt < TB_FRAME_SIZE loop
      if (i mod 10) = 5 then
        uart_send_byte(rx, std_logic_vector(to_unsigned(i mod 256, 8)), true, false);
        wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST2 DONE", tb_fail);
        if rx_parity_err /= '1' then
          tb_fail <= '1';
          assert false report "FAIL: Expected parity_err not seen at tx index " &
                              integer'image(i) severity ERROR;
        end if;
      else
        exp_small(valid_cnt) <= std_logic_vector(to_unsigned(i mod 256, 8));
        uart_send_byte(rx, exp_small(valid_cnt), false, false);
        wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST2 DONE", tb_fail);
        if rx_parity_err = '1' then
          tb_fail <= '1';
          assert false report "FAIL: Unexpected parity_err on valid byte (TEST2)" severity ERROR;
        end if;
        if rx_frame_err = '1' then
          tb_fail <= '1';
          assert false report "FAIL: Unexpected frame_err on valid byte (TEST2)" severity ERROR;
        end if;
        valid_cnt := valid_cnt + 1;
      end if;
      i := i + 1;
    end loop;

    -- Wait for frame_done
    timeout := 0;
    while frame_done = '0' loop
      wait until rising_edge(clk);
      timeout := timeout + 1;
      if timeout > 10_000 then
        tb_fail <= '1';
        assert false report "FAIL: TEST2 frame_done timeout" severity ERROR;
        exit;
      end if;
    end loop;
    report "PASS: TEST2 frame_done observed" severity NOTE;

    for i in 0 to TB_FRAME_SIZE-1 loop
      read_and_check(rd_addr, rd_en, rd_clk, rd_dout, i, exp_small(i), "TEST2 verify", tb_fail);
    end loop;

    --------------------------------------------------------------------------
    -- TEST 3: Frame error injection (bad stop bit); ensure skip
    --------------------------------------------------------------------------
    report "TEST3: Frame error injection" severity NOTE;

    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    valid_cnt := 0;
    i := 0;
    while valid_cnt < TB_FRAME_SIZE loop
      if (i mod 13) = 7 then
        uart_send_byte(rx, std_logic_vector(to_unsigned(i mod 256, 8)), false, true);
        wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST3 DONE", tb_fail);
        if rx_frame_err /= '1' then
          tb_fail <= '1';
          assert false report "FAIL: Expected frame_err not seen at tx index " &
                              integer'image(i) severity ERROR;
        end if;
      else
        exp_small(valid_cnt) <= std_logic_vector(to_unsigned(i mod 256, 8));
        uart_send_byte(rx, exp_small(valid_cnt), false, false);
        wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST3 DONE", tb_fail);
        if rx_parity_err = '1' then
          tb_fail <= '1';
          assert false report "FAIL: Unexpected parity_err on valid byte (TEST3)" severity ERROR;
        end if;
        if rx_frame_err = '1' then
          tb_fail <= '1';
          assert false report "FAIL: Unexpected frame_err on valid byte (TEST3)" severity ERROR;
        end if;
        valid_cnt := valid_cnt + 1;
      end if;
      i := i + 1;
    end loop;

    -- Wait for frame_done
    timeout := 0;
    while frame_done = '0' loop
      wait until rising_edge(clk);
      timeout := timeout + 1;
      if timeout > 10_000 then
        tb_fail <= '1';
        assert false report "FAIL: TEST3 frame_done timeout" severity ERROR;
        exit;
      end if;
    end loop;
    report "PASS: TEST3 frame_done observed" severity NOTE;

    for i in 0 to TB_FRAME_SIZE-1 loop
      read_and_check(rd_addr, rd_en, rd_clk, rd_dout, i, exp_small(i), "TEST3 verify", tb_fail);
    end loop;

    --------------------------------------------------------------------------
    -- TEST 4: Extra bytes after completion overwrite from address 0
    --------------------------------------------------------------------------
    report "TEST4: Post-completion extra bytes (overwrite from addr 0)" severity NOTE;

    uart_send_byte(rx, x"AA", false, false);
    wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST4 DONE", tb_fail);
    uart_send_byte(rx, x"BB", false, false);
    wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST4 DONE", tb_fail);
    uart_send_byte(rx, x"CC", false, false);
    wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST4 DONE", tb_fail);
    uart_send_byte(rx, x"01", false, false);
    wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST4 DONE", tb_fail);
    uart_send_byte(rx, x"02", false, false);
    wait_for_done_with_timeout(clk, rx_done, 50_000, "TEST4 DONE", tb_fail);

    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 0, x"AA", "TEST4 addr 0", tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 1, x"BB", "TEST4 addr 1", tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 2, x"CC", "TEST4 addr 2", tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 3, x"01", "TEST4 addr 3", tb_fail);
    read_and_check(rd_addr, rd_en, rd_clk, rd_dout, 4, x"02", "TEST4 addr 4", tb_fail);

    --------------------------------------------------------------------------
    -- TEST 5: Ignore short glitch (false start) on rx
    --------------------------------------------------------------------------
    report "TEST5: Short glitch on rx (no byte expected)" severity NOTE;

    rx <= '1';
    wait for BIT_PERIOD;
    rx <= '0';
    wait for BIT_PERIOD / 4;  -- short low glitch
    rx <= '1';
    wait for 6*BIT_PERIOD;    -- give UART time; should not assert done

    if rx_done = '1' then
      tb_fail <= '1';
      assert false report "FAIL: Spurious DONE after short glitch" severity ERROR;
    else
      report "PASS: No spurious DONE after short glitch" severity NOTE;
    end if;

    --------------------------------------------------------------------------
    -- Done
    --------------------------------------------------------------------------
    if tb_fail = '0' then
      tb_pass <= '1';
      report "All tests completed. PASS." severity NOTE;
    else
      tb_pass <= '0';
      report "All tests completed. FAIL (see errors above)." severity WARNING;
    end if;

    tb_finished <= '1';
    wait;
  end process;

  ----------------------------------------------------------------------------
  -- Timeout/finish guard
  ----------------------------------------------------------------------------
  finish_guard : process
  begin
    wait until tb_finished = '1' or now > 350 ms;
    if tb_finished = '1' then
      report "TESTBENCH: Finished normally." severity NOTE;
    else
      tb_fail <= '1';
      tb_pass <= '0';
      report "TESTBENCH: Global timeout." severity ERROR;
    end if;
    wait;
  end process;

end architecture;

