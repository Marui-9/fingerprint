----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 14.09.2025 17:05:59
-- Design Name: 
-- Module Name: input_data_bram_tb - Behavioral
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

entity tb_uart_to_bram is
end entity;

architecture tb of tb_uart_to_bram is

  constant DATA_WIDTH : integer := 8;
  constant ADDR_WIDTH : integer := 13;
  constant IMG_W      : integer := 68;
  constant IMG_H      : integer := 118;
  constant FRAME_SIZE : integer := IMG_W * IMG_H;

  signal clk        : std_logic := '0';
  signal rd_clk     : std_logic := '0';

  signal rst            : std_logic := '0';
  signal data_reg       : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal done_reg       : std_logic := '0';
  signal parity_err_reg : std_logic := '0';
  signal frame_err_reg  : std_logic := '0';
  signal frame_start    : std_logic := '0';
  signal frame_done     : std_logic;

  signal rd_en    : std_logic := '0';
  signal rd_addr  : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal rd_dout  : std_logic_vector(DATA_WIDTH-1 downto 0);

  signal test_done : std_logic := '0';

begin

  DUT : entity work.data_bram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_W,
      IMG_HEIGHT => IMG_H
    )
    port map (
      clk            => clk,
      rst            => rst,
      data_reg       => data_reg,
      done_reg       => done_reg,
      parity_err_reg => parity_err_reg,
      frame_err_reg  => frame_err_reg,
      frame_start    => frame_start,
      frame_done     => frame_done,
      rd_clk         => rd_clk,
      rd_en          => rd_en,
      rd_addr        => rd_addr,
      rd_dout        => rd_dout
    );

  clk_gen : process
  begin
    while now < 200 ms loop
      clk <= '0'; wait for 10 ns;
      clk <= '1'; wait for 10 ns;
    end loop;
    wait;
  end process;

  rd_clk_gen : process
  begin
    while now < 200 ms loop
      rd_clk <= '0'; wait for 20 ns;
      rd_clk <= '1'; wait for 20 ns;
    end loop;
    wait;
  end process;

  stim : process
    variable i : integer;
    variable timeout : integer;
    type int_array is array (natural range <>) of integer;
    variable seq_vals : int_array(0 to 9);
    variable expected_seq : int_array(0 to 8);
    variable k : integer;

    procedure send_valid_byte(b : in std_logic_vector(7 downto 0)) is
    begin
      data_reg <= b;
      parity_err_reg <= '0';
      frame_err_reg <= '0';
      wait until rising_edge(clk);
      done_reg <= '1';
      wait until rising_edge(clk);
      done_reg <= '0';
      wait for 1 ns;
    end procedure;

    procedure send_error_byte(b : in std_logic_vector(7 downto 0); which_err : in character) is
    begin
      data_reg <= b;
      if which_err = 'P' then
        parity_err_reg <= '1';
        frame_err_reg <= '0';
      else
        parity_err_reg <= '0';
        frame_err_reg <= '1';
      end if;
      wait until rising_edge(clk);
      done_reg <= '1';
      wait until rising_edge(clk);
      done_reg <= '0';
      parity_err_reg <= '0';
      frame_err_reg <= '0';
      wait for 1 ns;
    end procedure;

    procedure read_and_check(addr : in integer; expected : in std_logic_vector(7 downto 0); test_label : in string) is
    begin
      rd_addr <= to_unsigned(addr, rd_addr'length);
      rd_en <= '1';
      wait until rising_edge(rd_clk);
      wait for 1 ns;
      rd_en <= '0';
      if rd_dout /= expected then
        assert false
          report "FAIL: " & test_label & " - read addr " & integer'image(addr)
                 & " expected " & integer'image(to_integer(unsigned(expected)))
                 & " got " & integer'image(to_integer(unsigned(rd_dout)))
          severity error;
      else
        report "PASS: " & test_label & " - read addr " & integer'image(addr)
               & " = " & integer'image(to_integer(unsigned(rd_dout))) severity note;
      end if;
      wait until rising_edge(rd_clk);
    end procedure;

  begin
    seq_vals(0) := 10; seq_vals(1) := 20; seq_vals(2) := 30; seq_vals(3) := 40;
    seq_vals(4) := 50; seq_vals(5) := 60; seq_vals(6) := 70; seq_vals(7) := 80;
    seq_vals(8) := 90; seq_vals(9) := 100;

    rst <= '1';
    done_reg <= '0';
    parity_err_reg <= '0';
    frame_err_reg <= '0';
    frame_start <= '0';
    rd_en <= '0';
    wait for 200 ns;
    rst <= '0';
    wait for 50 ns;

    report "TEST1: Full frame capture starting..." severity note;
    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    for i in 0 to FRAME_SIZE-1 loop
      send_valid_byte(std_logic_vector(to_unsigned(i mod 256, 8)));
      if i = 100 then
        read_and_check(10, std_logic_vector(to_unsigned(10 mod 256, 8)), "Concurrent read during big write");
      end if;
    end loop;

    timeout := 0;
    while frame_done = '0' and timeout < 5000 loop
      wait until rising_edge(clk);
      timeout := timeout + 1;
    end loop;
    if frame_done = '0' then
      assert false report "FAIL: frame_done did not assert after full-frame write" severity error;
    else
      report "PASS: frame_done pulsed after full-frame write" severity note;
    end if;

    wait for 200 ns;
    read_and_check(0, std_logic_vector(to_unsigned(0 mod 256, 8)), "Full frame - first");
    read_and_check(FRAME_SIZE/2, std_logic_vector(to_unsigned((FRAME_SIZE/2) mod 256, 8)), "Full frame - middle");
    read_and_check(FRAME_SIZE-1, std_logic_vector(to_unsigned((FRAME_SIZE-1) mod 256, 8)), "Full frame - last");

    report "TEST2: Parity-error handling..." severity note;
    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    k := 0;
    for i in 0 to 9 loop
      if i = 5 then
        send_error_byte(std_logic_vector(to_unsigned(seq_vals(i),8)), 'P');
      else
        send_valid_byte(std_logic_vector(to_unsigned(seq_vals(i),8)));
        expected_seq(k) := seq_vals(i);
        k := k + 1;
      end if;
    end loop;

    wait for 200 ns;
    for i in 0 to 8 loop
      read_and_check(i, std_logic_vector(to_unsigned(expected_seq(i),8)), "Parity test - index " & integer'image(i));
    end loop;

    report "TEST3: Frame-error handling..." severity note;
    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    k := 0;
    for i in 0 to 9 loop
      if i = 3 then
        send_error_byte(std_logic_vector(to_unsigned(seq_vals(i),8)), 'F');
      else
        send_valid_byte(std_logic_vector(to_unsigned(seq_vals(i),8)));
        expected_seq(k) := seq_vals(i);
        k := k + 1;
      end if;
    end loop;

    wait for 200 ns;
    for i in 0 to 8 loop
      read_and_check(i, std_logic_vector(to_unsigned(expected_seq(i),8)), "Frame-err test - index " & integer'image(i));
    end loop;

    report "TEST4: frame_start reset behavior..." severity note;
    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';
    send_valid_byte(x"AA");
    send_valid_byte(x"BB");
    send_valid_byte(x"CC");
    send_valid_byte(x"01");
    send_valid_byte(x"02");

    wait for 100 ns;
    read_and_check(0, x"AA", "Frame-reset - addr 0");
    read_and_check(1, x"BB", "Frame-reset - addr 1");
    read_and_check(2, x"CC", "Frame-reset - addr 2");
    read_and_check(3, x"01", "Frame-reset - addr 3");
    read_and_check(4, x"02", "Frame-reset - addr 4");

    report "TEST5: Concurrent writes and reads stress test..." severity note;
    frame_start <= '1';
    wait until rising_edge(clk);
    frame_start <= '0';

    for i in 0 to 199 loop
      send_valid_byte(std_logic_vector(to_unsigned((i+7) mod 256, 8)));
      if i = 50 then
        read_and_check(10, std_logic_vector(to_unsigned((10+7) mod 256,8)), "Concurrent stress - addr 10");
      elsif i = 100 then
        read_and_check(90, std_logic_vector(to_unsigned((90+7) mod 256,8)), "Concurrent stress - addr 90");
      end if;
    end loop;

    wait for 100 ns;
    report "All tests completed - if no FAIL messages above, tests passed." severity note;
    test_done <= '1';
    wait;
  end process stim;

  stop_proc : process
  begin
    wait until test_done = '1' or now > 180 ms;
    if test_done = '1' then
      report "TESTBENCH: Finished normally." severity note;
    else
      report "TESTBENCH: Timeout reached." severity error;
    end if;
    wait;
  end process;

end architecture;

