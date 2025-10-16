library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_uart_rx is
end tb_uart_rx;

architecture sim of tb_uart_rx is

  -- DUT parameters (must match DUT generics for correct timing)
  constant CLK_FREQ   : integer := 50_000_000; -- 50 MHz
  constant BAUD_RATE  : integer := 9600;      -- 9.6 kbaud for reliable sim
  constant OVERSAMPLE : integer := 16;

  -- Derived timing (exactly in clock cycles, matching DUT calculations)
  -- BAUD_TICKS = number of clk cycles per oversample tick
  constant BAUD_TICKS : integer := CLK_FREQ / (BAUD_RATE * OVERSAMPLE);
  -- BIT_CYCLES = number of clk cycles per UART data bit (BAUD_TICKS * OVERSAMPLE)
  constant BIT_CYCLES : integer := BAUD_TICKS * OVERSAMPLE;
  -- clock period for a 50 MHz clock
  constant CLK_PERIOD : time := 20 ns;
  -- BIT_TIME (for reference / optional use)
  constant BIT_TIME   : time := BIT_CYCLES * CLK_PERIOD;

  -- Signals (DUT interface)
  signal clk        : std_logic := '0';
  signal reset      : std_logic := '1';
  signal rx         : std_logic := '1';  -- idle high
  signal data_out   : std_logic_vector(7 downto 0);
  signal done       : std_logic;
  signal parity_err : std_logic;
  signal frame_err  : std_logic;

  -- Test control
  signal tb_failed  : boolean := false;

  -- Timeout (in clock cycles) for waiting for 'done' (set reasonably large)
  constant WAIT_TIMEOUT_CYCLES : integer := BIT_CYCLES * 40; -- ~40 bit-times

begin

  ----------------------------------------------------------------------------
  -- Instantiate the DUT (must match your uart_rx entity/generics/ports)
  ----------------------------------------------------------------------------
  uut : entity work.uart_rx
    generic map (
      CLK_FREQ   => CLK_FREQ,
      BAUD_RATE  => BAUD_RATE,
      OVERSAMPLE => OVERSAMPLE
    )
    port map (
      clk        => clk,
      reset      => reset,
      rx         => rx,
      data_out   => data_out,
      done       => done,
      parity_err => parity_err,
      frame_err  => frame_err
    );

  ----------------------------------------------------------------------------
  -- Clock generator (50 MHz): use an explicit process so all waits are
  -- aligned to rising_edge(clk) in the stimulus process.
  ----------------------------------------------------------------------------
  clk_proc : process
  begin
    clk <= '0';
    wait for CLK_PERIOD/2;
    clk <= '1';
    wait for CLK_PERIOD/2;
  end process clk_proc;

  ----------------------------------------------------------------------------
  -- Stimulus process: contains local procedures (allowed to use 'wait')
  ----------------------------------------------------------------------------
  stim_proc : process
    -- Test vectors
    constant TEST1 : std_logic_vector(7 downto 0) := x"5A"; -- 0x5A
    constant TEST2 : std_logic_vector(7 downto 0) := x"A5"; -- 0xA5
    constant TEST3 : std_logic_vector(7 downto 0) := x"FF"; -- 0xFF

    -- Local helper: send a UART frame aligned to clk (waits on rising_edge)
    procedure send_frame(
      constant byte_in      : in std_logic_vector(7 downto 0);
      constant send_good_par: in boolean;
      constant good_stop    : in boolean
    ) is
      variable parity_calc : std_logic := '0';
      variable i           : integer;
      variable j           : integer;
      -- local loop counter
    begin
      -- compute even parity across data bits (LSB..MSB)
      parity_calc := '0';
      for j in 0 to 7 loop
        if byte_in(j) = '1' then
          parity_calc := not parity_calc;
        end if;
      end loop;

      -- Align to a clock edge before starting the start bit
      wait until rising_edge(clk);

      -- Start bit (drive low for exactly BIT_CYCLES clock cycles)
      rx <= '0';
      for i in 1 to BIT_CYCLES loop
        wait until rising_edge(clk);
      end loop;

      -- Data bits (LSB first), each for BIT_CYCLES cycles
      for j in 0 to 7 loop
        rx <= byte_in(j);
        for i in 1 to BIT_CYCLES loop
          wait until rising_edge(clk);
        end loop;
      end loop;

      -- Parity bit (correct or intentionally inverted)
      if send_good_par then
        rx <= parity_calc;
      else
        rx <= not parity_calc;
      end if;
      for i in 1 to BIT_CYCLES loop
        wait until rising_edge(clk);
      end loop;

      -- Stop bit (valid '1' or forced '0' -> framing error). Hold stop for 2 bit times
      if good_stop then
        rx <= '1';
      else
        rx <= '0';
      end if;
      -- hold stop for *two* bit-times to give DUT margin
      for i in 1 to (2 * BIT_CYCLES) loop
        wait until rising_edge(clk);
      end loop;

      -- Return to idle (keep line high for at least one bit-time)
      rx <= '1';
      for i in 1 to BIT_CYCLES loop
        wait until rising_edge(clk);
      end loop;
    end procedure send_frame;

    -- Wait for a done pulse synchronized to clk, with timeout.
    -- Returns TRUE in 'success' if done was observed, FALSE otherwise.
    -- Also checks that done does not remain asserted for >1 clk (warns if it does).
    procedure wait_for_done_and_check(
      constant single_cycle_ok : in boolean;
      success                 : out boolean
    ) is
      variable seen   : boolean := false;
      variable cycles : integer := 0;
    begin
      seen := false;
      cycles := 0;

      -- Poll 'done' at rising edges with timeout
      while cycles < WAIT_TIMEOUT_CYCLES loop
        wait until rising_edge(clk) and done = '1';
        cycles := cycles + 1;
        if done = '1' then
          seen := true;
          exit;
        end if;
      end loop;

      if not seen then
        success := false;
        return;
      end if;

      -- Check single-cycle pulse behavior if requested: sample on next rising edge
      if single_cycle_ok then
        wait until rising_edge(clk);
        if done = '1' then
          report "WARNING: 'done' stayed asserted for >1 clock cycle" severity warning;
        end if;
      end if;

      success := true;
    end procedure wait_for_done_and_check;

    -- helper to print vectors as decimal (for readable reports)
    function vec_to_dec_str(v : std_logic_vector) return string is
      variable i : integer := to_integer(unsigned(v));
    begin
      return integer'image(i);
    end function;

    variable ok : boolean;
  begin
    -- Initialize: ensure signals are stable on clock domain edges
    rx <= '1';
    reset <= '1';

    -- Hold reset for a few clock cycles to ensure DUT comes up cleanly
    for ok in 1 to 10 loop
      wait until rising_edge(clk);
    end loop;
    reset <= '0';

    -- Give DUT a little settling time (several bit times)
    for ok in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;

    ----------------------------------------------------------------------------
    -- Test 1: Correct frame (even parity, good stop)
    ----------------------------------------------------------------------------
    report "TB: Test 1 - Correct frame (expect done, data matched, no errors)" severity note;
    send_frame(TEST1, send_good_par => true, good_stop => true);

    wait_for_done_and_check(single_cycle_ok => true, success => ok);
    if not ok then
      report "FAIL: Test 1 - did not observe 'done' within timeout" severity error;
      tb_failed <= true;
    else
      -- We are currently at the rising edge where done was observed.
      -- Check outputs now (they are synchronous/register outputs).
      if data_out /= TEST1 then
        report "FAIL: Test 1 - data mismatch. Expected " & vec_to_dec_str(TEST1)
               & " got " & vec_to_dec_str(data_out) severity error;
        tb_failed <= true;
      else
        report "PASS: Test 1 - data matched." severity note;
      end if;
      if parity_err = '1' then
        report "FAIL: Test 1 - unexpected parity_err" severity warning;
        tb_failed <= true;
      end if;
      if frame_err = '1' then
        report "FAIL: Test 1 - unexpected frame_err" severity warning;
        tb_failed <= true;
      end if;
    end if;

    -- small gap
    for ok in 1 to 3 loop
      wait until rising_edge(clk);
    end loop;

    ----------------------------------------------------------------------------
    -- Test 2: Wrong parity bit
    ----------------------------------------------------------------------------
    report "TB: Test 2 - Wrong parity (expect done, parity_err asserted)" severity note;
    send_frame(TEST2, send_good_par => false, good_stop => true);

    wait_for_done_and_check(single_cycle_ok => true, success => ok);
    if not ok then
      report "FAIL: Test 2 - did not observe 'done' within timeout" severity error;
      tb_failed <= true;
    else
      if data_out /= TEST2 then
        report "FAIL: Test 2 - data mismatch. Expected " & vec_to_dec_str(TEST2)
               & " got " & vec_to_dec_str(data_out) severity error;
        tb_failed <= true;
      else
        report "PASS: Test 2 - data matched (parity bad)" severity note;
      end if;
      if parity_err /= '1' then
        report "FAIL: Test 2 - parity_err was not asserted as expected" severity error;
        tb_failed <= true;
      else
        report "PASS: Test 2 - parity_err detected" severity note;
      end if;
    end if;

    -- small gap
    for ok in 1 to 3 loop
      wait until rising_edge(clk);
    end loop;

    ----------------------------------------------------------------------------
    -- Test 3: Framing error (stop low)
    ----------------------------------------------------------------------------
    report "TB: Test 3 - Framing error (stop bit low) (expect done, frame_err asserted)" severity note;
    send_frame(TEST3, send_good_par => true, good_stop => false);

    wait_for_done_and_check(single_cycle_ok => true, success => ok);
    if not ok then
      report "FAIL: Test 3 - did not observe 'done' within timeout" severity error;
      tb_failed <= true;
    else
      if data_out /= TEST3 then
        report "FAIL: Test 3 - data mismatch. Expected " & vec_to_dec_str(TEST3)
               & " got " & vec_to_dec_str(data_out) severity error;
        tb_failed <= true;
      else
        report "PASS: Test 3 - data matched (framing error case)" severity note;
      end if;
      if frame_err /= '1' then
        report "FAIL: Test 3 - frame_err not asserted as expected" severity error;
        tb_failed <= true;
      else
        report "PASS: Test 3 - frame_err detected" severity note;
      end if;
    end if;

    -- small gap
    for ok in 1 to 4 loop
      wait until rising_edge(clk);
    end loop;

    ----------------------------------------------------------------------------
    -- Test 4: Back-to-back unsolicited frames (always-listening receiver)
    ----------------------------------------------------------------------------
    report "TB: Test 4 - Back-to-back unsolicited frames (expect both accepted)" severity note;

    -- Send frame A then, after one BIT, send frame B
    send_frame(TEST1, send_good_par => true, good_stop => true);
    -- short gap: one bit-time
    for ok in 1 to BIT_CYCLES loop
      wait until rising_edge(clk);
    end loop;
    send_frame(TEST2, send_good_par => true, good_stop => true);

    -- Expect first done
    wait_for_done_and_check(single_cycle_ok => true, success => ok);
    if not ok then
      report "FAIL: Test 4a - first done not observed" severity error;
      tb_failed <= true;
    else
      if data_out /= TEST1 then
        report "FAIL: Test 4a - first data mismatch. Expected " & vec_to_dec_str(TEST1)
               & " got " & vec_to_dec_str(data_out) severity error;
        tb_failed <= true;
      else
        report "PASS: Test 4a - first unsolicited frame received" severity note;
      end if;
    end if;

    -- Expect second done
    wait_for_done_and_check(single_cycle_ok => true, success => ok);
    if not ok then
      report "FAIL: Test 4b - second done not observed" severity error;
      tb_failed <= true;
    else
      if data_out /= TEST2 then
        report "FAIL: Test 4b - second data mismatch. Expected " & vec_to_dec_str(TEST2)
               & " got " & vec_to_dec_str(data_out) severity error;
        tb_failed <= true;
      else
        report "PASS: Test 4b - second unsolicited frame received" severity note;
      end if;
    end if;

    -- Final status
    if not tb_failed then
      report "TB: All tests PASSED for always-listening uart_rx" severity note;
    else
      report "TB: Some tests FAILED - inspect messages above" severity error;
    end if;

    wait; -- end simulation
  end process stim_proc;

end architecture sim;
