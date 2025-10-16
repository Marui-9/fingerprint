----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.09.2025 12:12:16
-- Design Name: 
-- Module Name: cpu_bus_decode - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: CPU bus decoder with mailbox registers
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.03 - Merge mailbox writers into single process (fix multi-drivers)
-- 
----------------------------------------------------------------------------------
 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity cpu_bus_decode is
  generic (
    PROBE_BASE_G        : std_logic_vector(31 downto 0) := x"20000000";
    PROBE_BYTES_G       : integer := 512*4;
    TMPL_BASE_G         : std_logic_vector(31 downto 0) := x"30000000";
    TMPL_BYTES_G        : integer := 32768;
    MBX_BASE_G          : std_logic_vector(31 downto 0) := x"40000000";
    MBX_BYTES_G         : integer := 256;
    PROBE_ADDR_WIDTH_G  : integer := 9;
    TMPL_ADDR_WIDTH_G   : integer := 13
  );
  port (
    clk_i, rstn_i : in std_logic;

    -- CPU MEM bus
    addr_i   : in  std_logic_vector(31 downto 0);
    wdata_i  : in  std_logic_vector(31 downto 0);
    wstrb_i  : in  std_logic_vector(3 downto 0);
    we_i     : in  std_logic;
    re_i     : in  std_logic;
    rdata_o  : out std_logic_vector(31 downto 0);
    ready_o  : out std_logic;

    -- PROBE BRAM (Port-B)
    probe_addr_o  : out std_logic_vector(PROBE_ADDR_WIDTH_G-1 downto 0);
    probe_wdata_o : out std_logic_vector(31 downto 0);
    probe_we_o    : out std_logic_vector(3 downto 0);
    probe_rdata_i : in  std_logic_vector(31 downto 0);

    -- TEMPLATE BRAM (Port-B)
    tmpl_addr_o   : out std_logic_vector(TMPL_ADDR_WIDTH_G-1 downto 0);
    tmpl_wdata_o  : out std_logic_vector(31 downto 0);
    tmpl_we_o     : out std_logic_vector(3 downto 0);
    tmpl_rdata_i  : in  std_logic_vector(31 downto 0);

    -- Mailbox regs (outputs only, updated internally)
    mbx_cmd          : out std_logic_vector(7 downto 0);
    mbx_cmd_arg0     : out std_logic_vector(31 downto 0);
    mbx_status       : out std_logic_vector(31 downto 0);
    mbx_result_slot  : out std_logic_vector(31 downto 0);
    mbx_result_score : out std_logic_vector(31 downto 0);
    mbx_valid_bmp0   : out std_logic_vector(31 downto 0);
    mbx_valid_bmp1   : out std_logic_vector(31 downto 0);
    mbx_irq_enable   : out std_logic_vector(31 downto 0);
    mbx_irq_status   : out std_logic_vector(31 downto 0);
    mbx_probe_len    : out std_logic_vector(31 downto 0);

    -- CU update strobes (merged with CPU MMIO)
    cu_cmd_wr        : in  std_logic;
    cu_cmd           : in  std_logic_vector(7 downto 0);
    cu_cmd_arg0_wr   : in  std_logic;
    cu_cmd_arg0      : in  std_logic_vector(31 downto 0);
    cu_probe_len_wr  : in  std_logic;
    cu_probe_len     : in  std_logic_vector(31 downto 0);
    cu_irq_set       : in  std_logic
  );
end entity;

architecture rtl of cpu_bus_decode is

  type sel_t is (SEL_NONE, SEL_PROBE, SEL_TMPL, SEL_MBX);
  signal sel        : sel_t := SEL_NONE;

  signal rdata_q    : std_logic_vector(31 downto 0) := (others=>'0');
  signal ready_q    : std_logic := '0';

  -- For 1-cycle read latency
  signal rd_pending : std_logic := '0';
  signal rd_sel_q   : sel_t := SEL_NONE;
  signal rd_addr_q  : std_logic_vector(31 downto 0) := (others => '0');

  -- Mailbox registers (internal storage)
  signal mbx_cmd_r          : std_logic_vector(7 downto 0)  := (others=>'0');
  signal mbx_cmd_arg0_r     : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_status_r       : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_result_slot_r  : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_result_score_r : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_valid_bmp0_r   : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_valid_bmp1_r   : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_irq_enable_r   : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_irq_status_r   : std_logic_vector(31 downto 0) := (others=>'0');
  signal mbx_probe_len_r    : std_logic_vector(31 downto 0) := (others=>'0');

  -- Helpers
  function in_range(a, base: std_logic_vector(31 downto 0); bytes: integer) return boolean is
    variable ua : unsigned(31 downto 0) := unsigned(a);
    variable ub : unsigned(31 downto 0) := unsigned(base);
  begin
    return (ua >= ub) and (ua < ub + to_unsigned(bytes, 32));
  end;

    function byte_offset(a, base: std_logic_vector(31 downto 0)) return unsigned is
  variable ua: unsigned(31 downto 0) := unsigned(a);
  variable ub: unsigned(31 downto 0) := unsigned(base);
begin
  return ua - ub; -- bytes
end;
  -- Address to word index (drop bits [1:0])
  function word_index(a, base: std_logic_vector(31 downto 0)) return unsigned is
    variable ua  : unsigned(31 downto 0) := unsigned(a);
    variable ub  : unsigned(31 downto 0) := unsigned(base);
    variable off : unsigned(31 downto 0);
  begin
    off := ua - ub;
    return off(31 downto 2);
  end;

begin
  ------------------------------------------------------------------------------
  -- Output assigns
  ------------------------------------------------------------------------------
  rdata_o <= rdata_q;
  ready_o <= ready_q;

  mbx_cmd          <= mbx_cmd_r;
  mbx_cmd_arg0     <= mbx_cmd_arg0_r;
  mbx_status       <= mbx_status_r;
  mbx_result_slot  <= mbx_result_slot_r;
  mbx_result_score <= mbx_result_score_r;
  mbx_valid_bmp0   <= mbx_valid_bmp0_r;
  mbx_valid_bmp1   <= mbx_valid_bmp1_r;
  mbx_irq_enable   <= mbx_irq_enable_r;
  mbx_irq_status   <= mbx_irq_status_r;
  mbx_probe_len    <= mbx_probe_len_r;

  ------------------------------------------------------------------------------
  -- Chip-select combinational
  ------------------------------------------------------------------------------
  process(addr_i)
  begin
    if    in_range(addr_i, PROBE_BASE_G, PROBE_BYTES_G) then sel <= SEL_PROBE;
    elsif in_range(addr_i, TMPL_BASE_G,  TMPL_BYTES_G)  then sel <= SEL_TMPL;
    elsif in_range(addr_i, MBX_BASE_G,   MBX_BYTES_G)   then sel <= SEL_MBX;
    else sel <= SEL_NONE;
    end if;
  end process;

  ------------------------------------------------------------------------------
  -- Static assignments to BRAM write-data and addresses
  ------------------------------------------------------------------------------
  probe_wdata_o <= wdata_i;
  tmpl_wdata_o  <= wdata_i;

  probe_addr_o <= std_logic_vector( resize(word_index(addr_i, PROBE_BASE_G), PROBE_ADDR_WIDTH_G) );
  tmpl_addr_o  <= std_logic_vector( resize(word_index(addr_i, TMPL_BASE_G),  TMPL_ADDR_WIDTH_G)  );

  ------------------------------------------------------------------------------
  -- SINGLE writer for mailbox regs + CPU WE/RE handshake + BRAM WEs
  ------------------------------------------------------------------------------
  process(clk_i, rstn_i)
    variable mbx_off_b : unsigned(7 downto 0);
    --variable off32   : unsigned(29 downto 0);
  begin
    if rstn_i='0' then
      -- Mailbox regs
      mbx_cmd_r          <= (others=>'0');
      mbx_cmd_arg0_r     <= (others=>'0');
      mbx_status_r       <= (others=>'0');
      mbx_result_slot_r  <= (others=>'0');
      mbx_result_score_r <= (others=>'0');
      mbx_valid_bmp0_r   <= (others=>'0');
      mbx_valid_bmp1_r   <= (others=>'0');
      mbx_irq_enable_r   <= (others=>'0');
      mbx_irq_status_r   <= (others=>'0');
      mbx_probe_len_r    <= (others=>'0');

      -- Bus pipeline
      rdata_q    <= (others=>'0');
      ready_q    <= '0';
      rd_pending <= '0';
      rd_sel_q   <= SEL_NONE;
      rd_addr_q  <= (others => '0');

      -- BRAM WEs
      probe_we_o <= (others => '0');
      tmpl_we_o  <= (others => '0');

    elsif rising_edge(clk_i) then
    --to take off, just as test
   -- mbx_result_score_r <= x"00001234";
      -- defaults each cycle
      ready_q    <= '0';
      probe_we_o <= (others => '0');
      tmpl_we_o  <= (others => '0');

      ----------------------------------------------------------------------------
      -- CU strobes take effect immediately (single writer of *_r)
      ----------------------------------------------------------------------------
      if cu_cmd_wr = '1'      then mbx_cmd_r      <= cu_cmd;      end if;
      if cu_cmd_arg0_wr = '1' then mbx_cmd_arg0_r <= cu_cmd_arg0; end if;
      if cu_probe_len_wr='1'  then mbx_probe_len_r<= cu_probe_len;end if;
      if cu_irq_set = '1'     then mbx_irq_status_r(0) <= '1';    end if;

      ----------------------------------------------------------------------------
      -- CPU writes (ack same cycle)
      ----------------------------------------------------------------------------
      if we_i = '1' then
        case sel is
          when SEL_PROBE =>
            probe_we_o <= wstrb_i; -- write-through to BRAM
            ready_q    <= '1';

          when SEL_TMPL  =>
            tmpl_we_o  <= wstrb_i;
            ready_q    <= '1';

          when SEL_MBX =>
              if addr_i(1 downto 0) = "00" and wstrb_i = "1111" ------------------------------------------
              then--------------------------------------------------------------------------------------
              mbx_off_b := byte_offset(addr_i, MBX_BASE_G)(7 downto 0);
              case std_logic_vector(mbx_off_b) is
                when x"00" => mbx_cmd_r          <= wdata_i(7 downto 0);
                when x"04" => mbx_cmd_arg0_r     <= wdata_i;
                when x"08" => mbx_status_r       <= wdata_i;
                when x"0C" => mbx_result_slot_r  <= wdata_i;
                when x"10" => mbx_result_score_r <= wdata_i;
                when x"14" => mbx_valid_bmp0_r   <= wdata_i;
                when x"18" => mbx_valid_bmp1_r   <= wdata_i;
                when x"1C" => mbx_irq_enable_r   <= wdata_i;
                when x"20" => mbx_irq_status_r   <= (mbx_irq_status_r and (not wdata_i)); -- W1C
                when x"24" => mbx_probe_len_r    <= wdata_i;
                when others => null;
              end case;
            ready_q <= '1';
            end if; -----------------------------------------------------------------------

          when others =>
            ready_q <= '1';
        end case;
      end if;

      ----------------------------------------------------------------------------
      -- Start reads (complete next cycle)
      ----------------------------------------------------------------------------
      if re_i = '1' then
        rd_pending <= '1';
        rd_sel_q   <= sel;
        rd_addr_q  <= addr_i;
      end if;

      ----------------------------------------------------------------------------
      -- Complete reads
      ----------------------------------------------------------------------------
      if rd_pending = '1' then
        case rd_sel_q is
          when SEL_PROBE =>
            rdata_q <= probe_rdata_i;

          when SEL_TMPL  =>
            rdata_q <= tmpl_rdata_i;

          when SEL_MBX   =>
            --off32   := word_index(rd_addr_q, MBX_BASE_G);
            mbx_off_b := byte_offset(rd_addr_q, MBX_BASE_G)(7 downto 0);
            case std_logic_vector(mbx_off_b) is
              when x"00" => rdata_q <= (31 downto 8 => '0') & mbx_cmd_r;
              when x"04" => rdata_q <= mbx_cmd_arg0_r;
              when x"08" => rdata_q <= mbx_status_r;
              when x"0C" => rdata_q <= mbx_result_slot_r;
              when x"10" => rdata_q <= mbx_result_score_r;
              when x"14" => rdata_q <= mbx_valid_bmp0_r;
              when x"18" => rdata_q <= mbx_valid_bmp1_r;
              when x"1C" => rdata_q <= mbx_irq_enable_r;
              when x"20" => rdata_q <= mbx_irq_status_r;
              when x"24" => rdata_q <= mbx_probe_len_r;
              when others => rdata_q <= (others=>'0');
            end case;

          when others =>
            rdata_q <= (others=>'0');
        end case;

        ready_q    <= '1'; 
        rd_pending <= '0';
      end if;
    end if;
  end process;

end architecture;
