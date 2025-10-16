----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 16:50:08
-- Design Name: 
-- Module Name: control_unit - Behavioral
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity control_unit is
  generic (
    IMG_WIDTH : integer := 68;
    CPU_STATUS_DONE_BIT : integer := 0
  );
  port (
    clk, rst_n         : in  std_logic;

    -- UI raw
    sw_mode            : in  std_logic;           
    sw_single          : in  std_logic;      
    sw_vga_sel         : in  std_logic_vector(1 downto 0); 
    sw_tmpl_id         : in  std_logic_vector(5 downto 0);  
    btn_start          : in  std_logic;
    btn_pause          : in  std_logic;
    btn_step           : in  std_logic;

    -- Data BRAM handshake
    frame_ready : in  std_logic;
    frame_start : out std_logic;

    -- pipeline handshakes
    bin_busy   : in  std_logic;
    bin_done_p : in  std_logic;
    thin_busy  : in  std_logic;
    thin_done_p: in  std_logic;
    min_busy   : in  std_logic;
    min_done_p : in  std_logic;

    -- From minutiae_extractor
    min_feat_count     : in  unsigned(9 downto 0);
    min_overflow       : in  std_logic;

    -- proc clock control
    proc_ce            : out std_logic;

    -- start pulses
    bin_start_p        : out std_logic;
    thin_start_p       : out std_logic;
    min_start_p        : out std_logic;

    -- CPU mailbox
    cu_cmd_wr          : out std_logic;
    cu_cmd             : out std_logic_vector(7 downto 0);
    cu_cmd_arg0_wr     : out std_logic;
    cu_cmd_arg0        : out std_logic_vector(31 downto 0);
    cu_probe_len_wr    : out std_logic;
    cu_probe_len       : out std_logic_vector(31 downto 0);
    cu_irq_set         : out std_logic;
    --debug
 --   cu_in_cpu_wait : out std_logic;
    -- CPU readback
    cu_status          : in  std_logic_vector(31 downto 0)
  --  cu_result_slot     : in  std_logic_vector(31 downto 0);
    --cu_result_score    : in  std_logic_vector(31 downto 0)
  );
end control_unit;

architecture rtl of control_unit is

  -- Button synchronizers
  signal start_meta, start_sync0, start_sync1 : std_logic := '0';
  signal pause_meta, pause_sync0, pause_sync1 : std_logic := '0';
  signal step_meta,  step_sync0,  step_sync1  : std_logic := '0';

  signal start_p, pause_tgl_p, step_p : std_logic := '0';

  -- CE generator
  type step_state_t is (STEP_IDLE, STEP_BURST);
  signal step_state : step_state_t := STEP_IDLE;
  signal ce_run     : std_logic := '1';
  signal ce_step    : std_logic := '0';
  signal step_cnt   : integer range 0 to IMG_WIDTH := 0;

  -- FSM
  type cu_state_t is (
    S_IDLE, S_WAIT_FRAME,
    -- binarizer stage
     S_WAIT_STEP_BIN, S_RUN_BIN, S_WAIT_BIN,
    -- thinning stage
     S_WAIT_STEP_THIN, S_RUN_THIN, S_WAIT_THIN,
    -- minutiae stage
    S_WAIT_STEP_MIN, S_RUN_MIN, S_WAIT_MIN,
    -- CPU handoff
    S_CPU_PREP, S_CPU_KICK, S_CPU_WAIT,
    S_DONE
  );
  signal st, st_n : cu_state_t := S_IDLE;

  -- start strobes
  signal bin_start_p_r, thin_start_p_r, min_start_p_r : std_logic := '0';

  -- mailbox
  signal cu_cmd_wr_r, cu_cmd_arg0_wr_r, cu_probe_len_wr_r, cu_irq_set_r : std_logic := '0';
  signal cu_cmd_r        : std_logic_vector(7 downto 0)  := (others=>'0');
  signal cu_cmd_arg0_r   : std_logic_vector(31 downto 0) := (others=>'0');
  signal cu_probe_len_r  : std_logic_vector(31 downto 0) := (others=>'0');

  signal mode_enroll : std_logic := '1';
  signal latched_slot: std_logic_vector(5 downto 0) := (others=>'0');

  signal cpu_done_s  : std_logic;
  signal frame_start_r : std_logic := '0';
  --re-arm for uart
  signal arm_rx        : std_logic := '0';

begin
  --------------------------------------------------------------------
  -- Button sync + edge detect
  --------------------------------------------------------------------
  process(clk, rst_n)
  begin
    if rst_n = '0' then
      start_meta <= '0'; start_sync0 <= '0'; start_sync1 <= '0'; start_p <= '0';
      pause_meta <= '0'; pause_sync0 <= '0'; pause_sync1 <= '0'; pause_tgl_p <= '0';
      step_meta  <= '0'; step_sync0  <= '0'; step_sync1  <= '0'; step_p <= '0';
    elsif rising_edge(clk) then
      start_meta <= btn_start; start_sync0 <= start_meta; start_sync1 <= start_sync0;
      pause_meta <= btn_pause; pause_sync0 <= pause_meta; pause_sync1 <= pause_sync0;
      step_meta  <= btn_step;  step_sync0  <= step_meta;  step_sync1  <= step_sync0;

      start_p     <= (not start_sync1) and start_sync0;
      pause_tgl_p <= (not pause_sync1) and pause_sync0;
      step_p      <= (not step_sync1)  and step_sync0;
    end if;
  end process;

  mode_enroll <= sw_mode;

  --------------------------------------------------------------------
  -- CE
  --------------------------------------------------------------------
  process(clk, rst_n)
  begin
    if rst_n='0' then
      ce_run <= '1';
    elsif rising_edge(clk) then
      if pause_tgl_p='1' then
        ce_run <= not ce_run;
      end if;
    end if;
  end process;

  process(clk, rst_n)
  begin
    if rst_n='0' then
      step_state <= STEP_IDLE; ce_step <= '0'; step_cnt <= 0;
    elsif rising_edge(clk) then
      case step_state is
        when STEP_IDLE =>
          ce_step <= '0';
          if step_p='1' then
            step_cnt   <= IMG_WIDTH;
            ce_step    <= '1';
            step_state <= STEP_BURST;
          end if;
        when STEP_BURST =>
          if step_cnt=0 then
            ce_step    <= '0';
            step_state <= STEP_IDLE;
          else
            step_cnt <= step_cnt - 1;
          end if;
      end case;
    end if;
  end process;

  --proc_ce <= ce_run or ce_step;

  --------------------------------------------------------------------
  -- CPU done
  --------------------------------------------------------------------
  cpu_done_s <= cu_status(CPU_STATUS_DONE_BIT);
-- State reg + frame_start pulse on enter S_WAIT_FRAME
process(clk, rst_n)
begin
  if rst_n = '0' then
    st            <= S_IDLE;
    frame_start_r <= '0';
  elsif rising_edge(clk) then
    frame_start_r <= '0';      -- default
    st <= st_n;

    -- 1-cycle arm of data_bram whenever we enter S_WAIT_FRAME
    if (st /= S_WAIT_FRAME) and (st_n = S_WAIT_FRAME) then
      frame_start_r <= '1';
    end if;
  end if;
end process;

 

  --------------------------------------------------------------------
  -- Outputs
  --------------------------------------------------------------------
  bin_start_p  <= bin_start_p_r;  
  thin_start_p <= thin_start_p_r;  
  min_start_p  <= min_start_p_r;
  cu_cmd_wr    <= cu_cmd_wr_r;    
  cu_cmd       <= cu_cmd_r;
  cu_cmd_arg0_wr <= cu_cmd_arg0_wr_r; 
  cu_cmd_arg0    <= cu_cmd_arg0_r;
  cu_probe_len_wr<= cu_probe_len_wr_r; 
  cu_probe_len   <= cu_probe_len_r;
  cu_irq_set     <= cu_irq_set_r;
  frame_start <= frame_start_r;
  
  
  --------------------------------------------------------------------
  -- FSM with STEP gating (treat *_done_p as LEVEL, not pulse)
  --------------------------------------------------------------------
  process(st, start_p, step_p, frame_ready,
          bin_done_p, thin_done_p, min_done_p,
          mode_enroll, sw_tmpl_id, min_feat_count, 
          cu_status)
    variable probe_len_v : unsigned(31 downto 0);
  begin
    -- defaults each cycle
    st_n              <= st;
    bin_start_p_r     <= '0';
    thin_start_p_r    <= '0';
    min_start_p_r     <= '0';
   -- frame_start_r     <= '0';
    
    cu_cmd_wr_r       <= '0';
    cu_cmd_arg0_wr_r  <= '0';
    cu_probe_len_wr_r <= '0';
    cu_irq_set_r      <= '0';
    
    cu_cmd_r          <= (others=>'0');
    cu_cmd_arg0_r     <= (others=>'0');
    cu_probe_len_r    <= (others=>'0');

    case st is
      ----------------------------------------------------------------
      -- Idle → wait for a frame in BRAM
      ----------------------------------------------------------------
      when S_IDLE =>
        if start_p = '1' then
          st_n <= S_WAIT_FRAME;
         -- frame_start_r   <= '1';    -- pulse start eachcapture
          cu_cmd_arg0_r <= (others => '0'); --0=enroll 1=test 
          cu_cmd_arg0_r(0) <= not mode_enroll;
          cu_cmd_arg0_wr_r <= '1';
        end if;

      when S_WAIT_FRAME =>
        if frame_ready = '1' then
          -- ensure old BIN done is low before we arm a new run
          st_n <= S_WAIT_STEP_BIN;
        end if;

      ----------------------------------------------------------------
      -- BIN stage: wait done low → wait STEP → start → wait done high
      ----------------------------------------------------------------
   
      when S_WAIT_STEP_BIN =>
        if step_p = '1' then
          bin_start_p_r <= '1';     -- one-cycle strobe; binarizer latches it
          st_n          <= S_RUN_BIN;
        end if;

      when S_RUN_BIN =>
        st_n <= S_WAIT_BIN;

      when S_WAIT_BIN =>
        -- bin_done_p is a LEVEL that stays '1' until next start is accepted
        if bin_done_p = '1' then
          st_n <= S_WAIT_STEP_THIN;
        end if;

      ----------------------------------------------------------------
      -- THIN stage
      ----------------------------------------------------------------
      when S_WAIT_STEP_THIN =>
        if step_p = '1' then
          thin_start_p_r <= '1';
          st_n           <= S_RUN_THIN;
        end if;

      when S_RUN_THIN =>
        st_n <= S_WAIT_STEP_MIN;

      ----------------------------------------------------------------
      -- MIN stage
      ----------------------------------------------------------------
      when S_WAIT_STEP_MIN =>
        if step_p = '1' then
          min_start_p_r <= '1';
          st_n          <= S_RUN_MIN;
        end if;

      when S_RUN_MIN =>
        st_n <= S_WAIT_MIN;

      when S_WAIT_MIN =>
        if min_done_p = '1' then
          st_n <= S_CPU_PREP;
          --st_n <= S_DONE;
        end if;

      ----------------------------------------------------------------
      -- CPU side (unchanged, still pulse the mailbox writes/IRQ)
      ----------------------------------------------------------------
      when S_CPU_PREP =>
        probe_len_v := (others=>'0');
        probe_len_v(9 downto 0) := min_feat_count; -- 0..512
        cu_probe_len_r    <= std_logic_vector(probe_len_v);
        cu_probe_len_wr_r <= '1';
        st_n <= S_CPU_KICK;

      when S_CPU_KICK =>
        if mode_enroll = '1' then
          cu_cmd_arg0_r(5 downto 0) <= sw_tmpl_id;
          cu_cmd_arg0_r(7)              <= sw_single;
          cu_cmd_arg0_wr_r <= '1';
          cu_cmd_r         <= x"10"; -- ENROLL 
          cu_cmd_wr_r      <= '1';
          cu_irq_set_r     <= '1';
        else
          cu_cmd_arg0_r(5 downto 0) <= (others=>'1'); -- match all
          cu_cmd_arg0_r(7)              <= sw_single;
          cu_cmd_arg0_wr_r <= '1';
          cu_cmd_r         <= x"20"; -- TEST 
          cu_cmd_wr_r      <= '1';
          cu_irq_set_r     <= '1';
        end if;
        st_n <= S_CPU_WAIT;

      when S_CPU_WAIT =>
        if cu_status(CPU_STATUS_DONE_BIT) = '1' then
          st_n <= S_DONE;
        end if;

      when S_DONE =>
          st_n <= S_WAIT_FRAME;

      when others =>
        st_n <= S_IDLE;
    end case;
  end process;

end rtl;
