----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 23.09.2025 09:47:37
-- Design Name: 
-- Module Name: xbus_simple_bridge - Behavioral
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
use IEEE.NUMERIC_STD.ALL;
-- Simple, single-beat XBUS -> "mem_*" shim
entity xbus_simple_bridge is
  port (
    -- clock & reset are optional here; we keep them in case you later want to register paths
    clk   : in  std_logic;
    rst_n : in  std_logic;

    -- NEORV32 XBUS master (from neorv32_top)
    xbus_adr_o : in  std_ulogic_vector(31 downto 0);
    xbus_dat_o : in  std_ulogic_vector(31 downto 0);
    xbus_dat_i : out std_ulogic_vector(31 downto 0);
    xbus_we_o  : in  std_ulogic;
    xbus_sel_o : in  std_ulogic_vector(3 downto 0);
    xbus_stb_o : in  std_ulogic;
    xbus_cyc_o : in  std_ulogic;
    xbus_ack_i : out std_ulogic;
    xbus_err_i : out std_ulogic;  -- we don't signal errors

    -- Optional: present but ignored (tie-thru if you wish)
    xbus_cti_o : in  std_ulogic_vector(2 downto 0);
    xbus_tag_o : in  std_ulogic_vector(2 downto 0);

    -- Simple bus toward your cpu_bus_decode
    mem_addr  : out std_logic_vector(31 downto 0);
    mem_wdata : out std_logic_vector(31 downto 0);
    mem_rdata : in  std_logic_vector(31 downto 0);
    mem_we    : out std_logic;
    mem_re    : out std_logic;
    mem_wstrb : out std_logic_vector(3 downto 0);
    mem_ready : in  std_logic
  );
end entity;

architecture rtl of xbus_simple_bridge is
  signal req  : std_logic;
  signal we   : std_logic;
  signal re   : std_logic;
begin
  -- Combinational request decode
  req <= std_logic(xbus_cyc_o and xbus_stb_o);
  we  <= std_logic(xbus_we_o) and req;
  re  <= (not std_logic(xbus_we_o)) and req;

  -- Drive simple bus
  mem_addr  <= std_logic_vector(xbus_adr_o);          -- byte address passthrough
  mem_wdata <= std_logic_vector(xbus_dat_o);
  mem_wstrb <= std_logic_vector(xbus_sel_o);
  mem_we    <= we;                                    -- gate with req
  mem_re    <= re;

  -- Return data/handshake to XBUS
  xbus_dat_i <= std_ulogic_vector(mem_rdata);
xbus_ack_i <= mem_ready;
xbus_err_i <= '0'; -- no error signaling for now

  -- cti/tag are ignored in this non-burst shim
  -- (left here to keep your instance "warnings-free")
  -- pragma translate_off
  assert (xbus_cti_o = "000") report "XBUS bursts not supported in simple bridge" severity note;
  -- pragma translate_on
end architecture;

