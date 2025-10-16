-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.09.2025 11:42:13
-- Design Name: 
-- Module Name: cpu_subsystem - Behavioral
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


----------------------------------------------------------------------------------
-- cpu_subsystem.vhd  (fixed)
-- - NEORV32 in dedicated library (neorv32lib)
-- - Internal IMEM/DMEM enabled
-- - External bus via Wishbone (MEM_EXT_EN => true)
-- - Local shim converts Wishbone master to simple mem_* handshake
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
-- cpu_subsystem.vhd
-- - NEORV32 (XBUS master) -> xbus_simple_bridge -> simple mem_* bus
-- - cpu_bus_decode maps mem_* to BRAMs & mailbox
-- - UART0 exposed
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;

entity cpu_subsystem is
  generic (
    -- CPU-visible address map windows
    PROBE_BASE_G        : std_logic_vector(31 downto 0) := x"20000000";
    PROBE_BYTES_G       : integer := 512*4;          -- 2 KB
    TMPL_BASE_G         : std_logic_vector(31 downto 0) := x"30000000";
    TMPL_BYTES_G        : integer := 32768;         -- 32 KB (8192 words × 4B)
    MBX_BASE_G          : std_logic_vector(31 downto 0) := x"40000000";
    MBX_BYTES_G         : integer := 256;

    -- BRAM Port-B address widths (words)
    PROBE_ADDR_WIDTH_G  : integer := 9;            -- up to 64k words
    TMPL_ADDR_WIDTH_G   : integer := 13             --  8k words (32 kb window)
  );
  port (
    clk    : in  std_logic;
    rst_n  : in  std_logic;

    ----------------------------------------------------------------------------
    -- Control-Unit (CU) side: command/notify
    ----------------------------------------------------------------------------
    cu_cmd_wr       : in  std_logic;
    cu_cmd          : in  std_logic_vector(7 downto 0);
    cu_cmd_arg0_wr  : in  std_logic;
    cu_cmd_arg0     : in  std_logic_vector(31 downto 0);
    cu_probe_len_wr : in  std_logic;
    cu_probe_len    : in  std_logic_vector(31 downto 0);
    cu_irq_set      : in  std_logic;

    -- Readback to CU
    cu_status       : out std_logic_vector(31 downto 0);
    cu_result_slot  : out std_logic_vector(31 downto 0);
    cu_result_score : out std_logic_vector(31 downto 0);
    -- debug taps (optional)
    dbg_mem_addr  : out std_logic_vector(31 downto 0);
    dbg_mem_we    : out std_logic;
    dbg_mem_re    : out std_logic;
    dbg_mem_ready : out std_logic;
    dbg_mbx_score : out std_logic_vector(31 downto 0);
    ----------------------------------------------------------------------------
    -- BRAM Port-B (CPU side) for PROBE and TEMPLATE memories
    ----------------------------------------------------------------------------
    probe_addr_o  : out std_logic_vector(PROBE_ADDR_WIDTH_G-1 downto 0);
    probe_wdata_o : out std_logic_vector(31 downto 0);
    probe_we_o    : out std_logic_vector(3 downto 0);
    probe_rdata_i : in  std_logic_vector(31 downto 0);

    tmpl_addr_o   : out std_logic_vector(TMPL_ADDR_WIDTH_G-1 downto 0);
    tmpl_wdata_o  : out std_logic_vector(31 downto 0);
    tmpl_we_o     : out std_logic_vector(3 downto 0);
    tmpl_rdata_i  : in  std_logic_vector(31 downto 0);

    ----------------------------------------------------------------------------
    -- Optional UART (NEORV32 bootloader/console)
    ----------------------------------------------------------------------------
    uart_txd_o : out std_logic := '1';
    uart_rxd_i : in  std_logic := '1'
  );
end entity;  

architecture rtl of cpu_subsystem is

  --------------------------------------------------------------------
  -- Simple internal memory bus (to cpu_bus_decode)
  --------------------------------------------------------------------
  signal mem_addr   : std_logic_vector(31 downto 0);
  signal mem_wdata  : std_logic_vector(31 downto 0);
  signal mem_rdata  : std_logic_vector(31 downto 0);
  signal mem_we     : std_logic;
  signal mem_re     : std_logic;
  signal mem_wstrb  : std_logic_vector(3 downto 0);
  signal mem_ready  : std_logic;

  --------------------------------------------------------------------
  -- NEORV32 XBUS master signals (std_ulogic[_vector])
  --------------------------------------------------------------------
  signal xbus_adr_o : std_ulogic_vector(31 downto 0);
  signal xbus_dat_o : std_ulogic_vector(31 downto 0);
  signal xbus_dat_i : std_ulogic_vector(31 downto 0);
  signal xbus_we_o  : std_ulogic;
  signal xbus_sel_o : std_ulogic_vector(3 downto 0);
  signal xbus_stb_o : std_ulogic;
  signal xbus_cyc_o : std_ulogic;
  signal xbus_ack_i : std_ulogic;
  signal xbus_err_i : std_ulogic;  -- driven low by bridge
  signal xbus_cti_o : std_ulogic_vector(2 downto 0);
  signal xbus_tag_o : std_ulogic_vector(2 downto 0);

  --------------------------------------------------------------------
  -- Mailbox registers & IRQ
  --------------------------------------------------------------------
  signal mbx_cmd          : std_logic_vector(7 downto 0)  := (others => '0');
  signal mbx_cmd_arg0     : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_status       : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_result_slot  : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_result_score : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_valid_bmp0   : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_valid_bmp1   : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_irq_enable   : std_logic_vector(31 downto 0) := (others => '0');
  signal mbx_irq_status   : std_logic_vector(31 downto 0) := (others => '0'); -- W1C by CPU
  signal mbx_probe_len    : std_logic_vector(31 downto 0) := (others => '0');

  signal cpu_irq          : std_logic;

begin

  --------------------------------------------------------------------
  -- NEORV32 core (XBUS master enabled)
  --------------------------------------------------------------------
  u_cpu : entity neorv32.neorv32_top
    generic map (
      CLOCK_FREQUENCY  => 50_000_000,
      -- Use the generic names that match your NEORV32 version
      IMEM_EN          => true,
      IMEM_SIZE        => 16*1024,
      DMEM_EN          => true,
      DMEM_SIZE        => 8*1024,
      ICACHE_EN        => false,
      DCACHE_EN        => false,
      XBUS_EN          => true,
      BOOT_MODE_SELECT => 0            -- 0: bootloader via UART0
    )
    port map (
      clk_i         => clk,
      rstn_i        => rst_n,
      -- XBUS master
      xbus_adr_o    => xbus_adr_o,
      xbus_dat_o    => xbus_dat_o,
      xbus_cti_o    => xbus_cti_o,
      xbus_tag_o    => xbus_tag_o,
      xbus_we_o     => xbus_we_o,
      xbus_sel_o    => xbus_sel_o,
      xbus_stb_o    => xbus_stb_o,
      xbus_cyc_o    => xbus_cyc_o,
      xbus_dat_i    => xbus_dat_i,
      xbus_ack_i    => xbus_ack_i,
      xbus_err_i    => xbus_err_i,     -- bridge ties low
      -- UART0
      uart0_txd_o   => uart_txd_o,
      uart0_rxd_i   => uart_rxd_i
      -- irq_i unconnected here (level=0)
    );

  --------------------------------------------------------------------
  -- XBUS -> simple mem_* bridge
  --------------------------------------------------------------------
  u_xbus_bridge : entity work.xbus_simple_bridge
    port map (
      clk         => clk,
      rst_n       => rst_n,

      -- XBUS side
      xbus_adr_o  => xbus_adr_o,
      xbus_dat_o  => xbus_dat_o,
      xbus_dat_i  => xbus_dat_i,
      xbus_we_o   => xbus_we_o,
      xbus_sel_o  => xbus_sel_o,
      xbus_stb_o  => xbus_stb_o,
      xbus_cyc_o  => xbus_cyc_o,
      xbus_ack_i  => xbus_ack_i,
      xbus_err_i  => xbus_err_i,     -- bridge drives '0'
      xbus_cti_o  => xbus_cti_o,
      xbus_tag_o  => xbus_tag_o,

      -- Simple bus side (std_logic / std_logic_vector)
      mem_addr    => mem_addr,
      mem_wdata   => mem_wdata,
      mem_rdata   => mem_rdata,
      mem_we      => mem_we,
      mem_re      => mem_re,
      mem_wstrb   => mem_wstrb,
      mem_ready   => mem_ready
    );

  --------------------------------------------------------------------
  -- Bus decoder: maps simple mem_* to BRAMs & mailbox
  --------------------------------------------------------------------
  u_bus : entity work.cpu_bus_decode
    generic map (
      PROBE_BASE_G        => PROBE_BASE_G,
      PROBE_BYTES_G       => PROBE_BYTES_G,
      TMPL_BASE_G         => TMPL_BASE_G,
      TMPL_BYTES_G        => TMPL_BYTES_G,
      MBX_BASE_G          => MBX_BASE_G,
      MBX_BYTES_G         => MBX_BYTES_G,
      PROBE_ADDR_WIDTH_G  => PROBE_ADDR_WIDTH_G,
      TMPL_ADDR_WIDTH_G   => TMPL_ADDR_WIDTH_G
    )
    port map (
      clk_i      => clk,
      rstn_i     => rst_n,

      -- CPU simple bus
      addr_i     => mem_addr,
      wdata_i    => mem_wdata,
      wstrb_i    => mem_wstrb,
      we_i       => mem_we,
      re_i       => mem_re,
      rdata_o    => mem_rdata,
      ready_o    => mem_ready,

      -- BRAM-B (CPU ports)
      probe_addr_o  => probe_addr_o,
      probe_wdata_o => probe_wdata_o,
      probe_we_o    => probe_we_o,
      probe_rdata_i => probe_rdata_i,

      tmpl_addr_o   => tmpl_addr_o,
      tmpl_wdata_o  => tmpl_wdata_o,
      tmpl_we_o     => tmpl_we_o,
      tmpl_rdata_i  => tmpl_rdata_i,

      -- Mailbox registers
      mbx_cmd          => mbx_cmd,
      mbx_cmd_arg0     => mbx_cmd_arg0,
      mbx_status       => mbx_status,
      mbx_result_slot  => mbx_result_slot,
      mbx_result_score => mbx_result_score,
      mbx_valid_bmp0   => mbx_valid_bmp0,
      mbx_valid_bmp1   => mbx_valid_bmp1,
      mbx_irq_enable   => mbx_irq_enable,
      mbx_irq_status   => mbx_irq_status,
      mbx_probe_len    => mbx_probe_len,

      -- CU-side strobes passthrough
      cu_cmd_wr        => cu_cmd_wr,
      cu_cmd           => cu_cmd,
      cu_cmd_arg0_wr   => cu_cmd_arg0_wr,
      cu_cmd_arg0      => cu_cmd_arg0,
      cu_probe_len_wr  => cu_probe_len_wr,
      cu_probe_len     => cu_probe_len,
      cu_irq_set       => cu_irq_set
    );

  --------------------------------------------------------------------
  -- External interrupt (IRQ0) from mailbox, if you later wire irq_i
  --------------------------------------------------------------------
  cpu_irq <= '1' when (mbx_irq_enable(0) = '1' and mbx_irq_status(0) = '1') else '0';

  -- CU readback mirrors
  cu_status       <= mbx_status;
  cu_result_slot  <= mbx_result_slot;
  cu_result_score <= mbx_result_score;
  dbg_mem_addr  <= mem_addr;
  dbg_mem_we    <= mem_we;
  dbg_mem_re    <= mem_re;
  dbg_mem_ready <= mem_ready;
  dbg_mbx_score <= mbx_result_score;
process(clk, rst_n)
  variable cnt: unsigned(31 downto 0);
begin
  if rst_n='0' then cnt := (others=>'0');
  elsif rising_edge(clk) then cnt := cnt + 1;
  end if;
  -- comment out normal mirror for one build:
  --cu_result_score <= std_logic_vector(cnt); -- should animate on 7-seg
end process;

end architecture;
 

