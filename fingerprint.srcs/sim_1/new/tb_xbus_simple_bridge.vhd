----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 23.09.2025 09:55:02
-- Design Name: 
-- Module Name: tb_xbus_simple_bridge - Behavioral
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

entity tb_xbus_simple_bridge is
end tb_xbus_simple_bridge;

architecture sim of tb_xbus_simple_bridge is
  constant CLK_PERIOD : time := 10 ns;

  -- DUT signals
  signal clk       : std_logic := '0';
  signal rstn      : std_logic := '0';

  -- XBUS/Wishbone side
  signal xbus_adr_o : std_logic_vector(31 downto 0);
  signal xbus_dat_o : std_logic_vector(31 downto 0);
  signal xbus_dat_i : std_logic_vector(31 downto 0);
  signal xbus_we_o  : std_logic;
  signal xbus_sel_o : std_logic_vector(3 downto 0);
  signal xbus_stb_o : std_logic;
  signal xbus_cyc_o : std_logic;
  signal xbus_ack_i : std_logic;

  -- Simple bus side
  signal mem_addr  : std_logic_vector(31 downto 0);
  signal mem_wdata : std_logic_vector(31 downto 0);
  signal mem_rdata : std_logic_vector(31 downto 0);
  signal mem_we    : std_logic;
  signal mem_re    : std_logic;
  signal mem_wstrb : std_logic_vector(3 downto 0);
  signal mem_ready : std_logic;

  -- simple 1-word dummy store
  signal mem_storage : std_logic_vector(31 downto 0) := (others => '0');

  signal tb_failed : boolean := false;

begin
  --------------------------------------------------------------------
  -- Clock
  --------------------------------------------------------------------
  clk <= not clk after CLK_PERIOD/2;

  --------------------------------------------------------------------
  -- DUT
  --------------------------------------------------------------------
  dut : entity work.xbus_simple_bridge
    port map (
      clk        => clk,
      rst_n      => rstn,

      -- XBUS master side
      xbus_adr_o => xbus_adr_o,
      xbus_dat_o => xbus_dat_o,
      xbus_dat_i => xbus_dat_i,
      xbus_we_o  => xbus_we_o,
      xbus_sel_o => xbus_sel_o,
      xbus_stb_o => xbus_stb_o,
      xbus_cyc_o => xbus_cyc_o,
      xbus_ack_i => xbus_ack_i,
      xbus_err_i => open,               -- not used
      xbus_cti_o => (others => '0'),    -- classic single access
      xbus_tag_o => (others => '0'),

      -- Simple bus side
      mem_addr   => mem_addr,
      mem_wdata  => mem_wdata,
      mem_rdata  => mem_rdata,
      mem_we     => mem_we,
      mem_re     => mem_re,
      mem_wstrb  => mem_wstrb,
      mem_ready  => mem_ready
    );

  --------------------------------------------------------------------
  -- Dummy simple bus memory
  --  - Stores last write
  --  - Returns stored value on read
  --  - Acks in one cycle
  --------------------------------------------------------------------
  process(clk)
  begin
    if rising_edge(clk) then
      mem_ready <= '0';  -- default each cycle

      if mem_we = '1' then
        mem_storage <= mem_wdata;
        mem_ready   <= '1';
      elsif mem_re = '1' then
        mem_rdata   <= mem_storage;
        mem_ready   <= '1';
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Stimulus
  --------------------------------------------------------------------
  stim : process
  begin
    -- Reset
    rstn <= '0';
    wait for 50 ns;
    rstn <= '1';
    wait for 20 ns;

    ----------------------------------------------------------------
    -- WRITE test
    ----------------------------------------------------------------
    report "Starting WRITE test";
    xbus_adr_o <= x"00000004";
    xbus_dat_o <= x"DEADBEEF";
    xbus_we_o  <= '1';
    xbus_sel_o <= "1111";
    xbus_stb_o <= '1';
    xbus_cyc_o <= '1';

    wait until rising_edge(clk) and xbus_ack_i = '1';
    xbus_stb_o <= '0';
    xbus_cyc_o <= '0';
    xbus_we_o  <= '0';

    report "WRITE test passed";

    ----------------------------------------------------------------
    -- READ test (expects the stored DEADBEEF)
    ----------------------------------------------------------------
    report "Starting READ test";
    xbus_adr_o <= x"00000008";
    xbus_we_o  <= '0';
    xbus_sel_o <= "1111";
    xbus_stb_o <= '1';
    xbus_cyc_o <= '1';

    wait until rising_edge(clk) and xbus_ack_i = '1';
    if xbus_dat_i /= x"DEADBEEF" then
      report "XBUS bridge READ test failed: unexpected data" severity error;
      tb_failed <= true;
    else
      report "READ test passed";
    end if;
    xbus_stb_o <= '0';
    xbus_cyc_o <= '0';

    ----------------------------------------------------------------
    -- Finish
    ----------------------------------------------------------------
    wait for 50 ns;
    if tb_failed then
      report "TB FAILED" severity failure;
    else
      report "TB PASSED" severity note;
    end if;

    wait;
  end process;

end sim;


