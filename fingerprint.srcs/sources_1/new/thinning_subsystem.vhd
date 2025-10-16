-- Engineer: 
-- 
-- Create Date: 20.09.2025 10:07:58
-- Design Name: 
-- Module Name: thinning_subsystem - Behavioral
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

-- thinning_subsystem.vhd
--- thinning_subsystem.vhd
-- VHDL-93, Vivado-friendly
-- Guo-Hall thinning with ping-pong frame buffers and fixed iteration cap.
-- Uses an image_window_reader that drives bram_rd_{en,addr,dout}.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity thinning_subsystem is
  generic (
    DATA_WIDTH : integer := 8;
    ADDR_WIDTH : integer := 13;  -- must cover IMG_WIDTH*IMG_HEIGHT
    IMG_WIDTH  : integer := 68;
    IMG_HEIGHT : integer := 118;
    MAX_ITERS  : integer := 2    -- full iterations; sub-iterations = 2*MAX_ITERS
  );
  port (
    clk          : in  std_logic;
    rst          : in  std_logic;

    -- Control-unit handshakes
    s_thin_start : in  std_logic;
    s_thin_busy  : out std_logic;
    s_thin_done  : out std_logic;

    -- Source: BINARY_BRAM Port-B (read)
    bin_rd_en    : out std_logic;
    bin_rd_addr  : out unsigned(ADDR_WIDTH-1 downto 0);
    bin_rd_dout  : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Source: THIN_A Port-B (read)
    thinA_rd_en   : out std_logic;
    thinA_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    thinA_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Source: THIN_B Port-B (read)
    thinB_rd_en   : out std_logic;
    thinB_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    thinB_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Destination: THIN_A Port-A (write)
    thinA_we     : out std_logic;
    thinA_addr   : out unsigned(ADDR_WIDTH-1 downto 0);
    thinA_din    : out std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Destination: THIN_B Port-A (write)
    thinB_we     : out std_logic;
    thinB_addr   : out unsigned(ADDR_WIDTH-1 downto 0);
    thinB_din    : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of thinning_subsystem is

  ---------------------------------------------------------------------------
  -- Constants / types
  ---------------------------------------------------------------------------
  constant TOTAL_PIX : integer := IMG_WIDTH * IMG_HEIGHT;

  type state_t is (
    IDLE,
    PREP_SUBITER,
    CLEAR_DST,
    START_READER,
    RUN_SUBITER,
    NEXT_SUBITER,
    DONE
  );

  type src_sel_t is (SRC_BIN, SRC_A, SRC_B);
  type dst_sel_t is (DST_A, DST_B);

  ---------------------------------------------------------------------------
  -- Start latch
  ---------------------------------------------------------------------------
  signal thin_start_req : std_logic := '0';
  

  ---------------------------------------------------------------------------
  -- Window reader interface
  ---------------------------------------------------------------------------
  signal wr_start       : std_logic := '0';
  signal wr_pixel_valid : std_logic;
  signal wr_done_pulse  : std_logic;
  signal wr_busy_unused : std_logic;
  signal wr_rd_en       : std_logic;
  signal wr_rd_addr     : unsigned(ADDR_WIDTH-1 downto 0);
  signal wr_rd_dout     : std_logic_vector(DATA_WIDTH-1 downto 0);

  signal w00, w01, w02 : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal w10, w11, w12 : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal w20, w21, w22 : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal cx8, cy8      : std_logic_vector(7 downto 0);

  ---------------------------------------------------------------------------
  -- FSM / control
  ---------------------------------------------------------------------------
  signal cur_state      : state_t := IDLE;
  signal next_state     : state_t := IDLE;

  signal src_sel        : src_sel_t := SRC_BIN;
  signal dst_sel        : dst_sel_t := DST_A;

  signal subiter        : std_logic := '0';  
  signal total_subiters : integer range 0 to (2*MAX_ITERS) := 0;

  signal clear_addr     : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');

  -- Destination write pipe
  signal dst_we    : std_logic := '0';
  signal dst_addr  : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal dst_din   : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

  -- Status
  signal busy_r    : std_logic := '0';
  signal done_r    : std_logic := '0';

  ---------------------------------------------------------------------------
  -- Output drivers (internal)
  ---------------------------------------------------------------------------
  signal bin_rd_en_i     : std_logic := '0';
  signal bin_rd_addr_i   : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thinA_rd_en_i   : std_logic := '0';
  signal thinA_rd_addr_i : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thinB_rd_en_i   : std_logic := '0';
  signal thinB_rd_addr_i : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thinA_we_i      : std_logic := '0';
  signal thinA_addr_i    : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thinA_din_i     : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal thinB_we_i      : std_logic := '0';
  signal thinB_addr_i    : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thinB_din_i     : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

  ---------------------------------------------------------------------------
  -- Helpers
  ---------------------------------------------------------------------------
  function pix_to_bit(p : std_logic_vector) return std_logic is
    variable any1 : boolean := false;
  begin
    for i in p'range loop
      if p(i)='1' then any1 := true; end if;
    end loop;
    if any1 then return '1'; else return '0'; end if;
  end function;

  function b2i(b : boolean) return integer is
  begin
    if b then return 1; else return 0; end if;
  end function;

  function addr_of(xu8, yu8 : std_logic_vector(7 downto 0)) return unsigned is
    variable x, y : integer;
  begin
    x := to_integer(unsigned(xu8));
    y := to_integer(unsigned(yu8));
    return to_unsigned(y*IMG_WIDTH + x, ADDR_WIDTH);
  end function;
    
  function z2o(a,b: std_logic) return integer is
    begin
      if (a='0') and (b='1') then return 1; else return 0; end if;
    end;
    
  function cn_transitions(p2,p3,p4,p5,p6,p7,p8,p9: std_logic) return integer is
    begin
      return z2o(p2,p3)+z2o(p3,p4)+z2o(p4,p5)+z2o(p5,p6)+
             z2o(p6,p7)+z2o(p7,p8)+z2o(p8,p9)+z2o(p9,p2);
    end;
begin
  ---------------------------------------------------------------------------
  -- Start latch
  ---------------------------------------------------------------------------

  process(clk, rst)
  begin
    if rst='1' then
      thin_start_req <= '0';
    elsif rising_edge(clk) then
      if s_thin_start='1' then
        thin_start_req <= '1';
      elsif (cur_state=IDLE and next_state=PREP_SUBITER) then
        thin_start_req <= '0';  -- clear when accepted
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- Output mappings
  ---------------------------------------------------------------------------
  s_thin_busy <= busy_r;
  s_thin_done <= done_r;

  bin_rd_en   <= bin_rd_en_i;
  bin_rd_addr <= bin_rd_addr_i;
  thinA_rd_en   <= thinA_rd_en_i;
  thinA_rd_addr <= thinA_rd_addr_i;
  thinB_rd_en   <= thinB_rd_en_i;
  thinB_rd_addr <= thinB_rd_addr_i;
  thinA_we   <= thinA_we_i;
  thinA_addr <= thinA_addr_i;
  thinA_din  <= thinA_din_i;
  thinB_we   <= thinB_we_i;
  thinB_addr <= thinB_addr_i;
  thinB_din  <= thinB_din_i;

  ---------------------------------------------------------------------------
  -- Source selection mux
  ---------------------------------------------------------------------------
  process(src_sel, wr_rd_en, wr_rd_addr, bin_rd_dout, thinA_rd_dout, thinB_rd_dout)
  begin
    bin_rd_en_i <= '0'; thinA_rd_en_i <= '0'; thinB_rd_en_i <= '0';
    bin_rd_addr_i <= (others=>'0');
    thinA_rd_addr_i <= (others=>'0');
    thinB_rd_addr_i <= (others=>'0');
    wr_rd_dout <= (others=>'0');
    case src_sel is
      when SRC_BIN => bin_rd_en_i <= wr_rd_en; bin_rd_addr_i <= wr_rd_addr; wr_rd_dout <= bin_rd_dout;
      when SRC_A   => thinA_rd_en_i <= wr_rd_en; thinA_rd_addr_i <= wr_rd_addr; wr_rd_dout <= thinA_rd_dout;
      when SRC_B   => thinB_rd_en_i <= wr_rd_en; thinB_rd_addr_i <= wr_rd_addr; wr_rd_dout <= thinB_rd_dout;
    end case;
  end process;

  ---------------------------------------------------------------------------
  -- Destination selection mux
  ---------------------------------------------------------------------------
  process(dst_sel, dst_we, dst_addr, dst_din)
  begin
    thinA_we_i <= '0'; thinB_we_i <= '0';
    thinA_addr_i <= (others=>'0'); thinB_addr_i <= (others=>'0');
    thinA_din_i  <= (others=>'0'); thinB_din_i  <= (others=>'0');
    case dst_sel is
      when DST_A => thinA_we_i <= dst_we; thinA_addr_i <= dst_addr; thinA_din_i <= dst_din;
      when DST_B => thinB_we_i <= dst_we; thinB_addr_i <= dst_addr; thinB_din_i <= dst_din;
    end case;
  end process;

  ---------------------------------------------------------------------------
  -- Window reader instance
  ---------------------------------------------------------------------------
  win_reader_inst: entity work.image_window_reader
    generic map (
      DATA_WIDTH=>DATA_WIDTH, 
      ADDR_WIDTH=>ADDR_WIDTH,
      IMG_WIDTH=>IMG_WIDTH, 
      IMG_HEIGHT=>IMG_HEIGHT 
      )
    port map (
      bram_rd_clk=>clk, 
      rst=>rst, 
      start=>wr_start,
      bram_rd_en=>wr_rd_en, 
      bram_rd_addr=>wr_rd_addr, 
      bram_rd_dout=>wr_rd_dout,
      pixel_valid=>wr_pixel_valid, 
      center_x=>cx8, 
      center_y=>cy8,
      w00=>w00, w01=>w01,w02=>w02, 
      w10=>w10,w11=>w11,w12=>w12,
      w20=>w20,w21=>w21,w22=>w22,
      busy=>wr_busy_unused, 
      done_pulse=>wr_done_pulse);

  ---------------------------------------------------------------------------
  -- FSM registers
  ---------------------------------------------------------------------------
  process(clk)
  begin
    if rising_edge(clk) then
      if rst='1' then
        cur_state<=IDLE;
        busy_r<='0';
        done_r<='0';
        wr_start<='0';
        src_sel<=SRC_BIN;
        dst_sel<=DST_A;
        subiter<='0';
        total_subiters<=0;
        clear_addr<=(others=>'0');
      else
        cur_state<=next_state;
        wr_start<='0';

        case cur_state is
          when IDLE =>
            busy_r<='0';
            -- clear done when starting a new run
            if (thin_start_req='1' and next_state=PREP_SUBITER) then
              done_r <= '0';
            end if;

          when PREP_SUBITER =>
            busy_r<='1';
            if total_subiters=0 then
              src_sel<=SRC_BIN; dst_sel<=DST_A;
            else
              if dst_sel=DST_A then
                src_sel<=SRC_A; dst_sel<=DST_B;
              else
                src_sel<=SRC_B; dst_sel<=DST_A;
              end if;
            end if;
            clear_addr<=(others=>'0');

          when CLEAR_DST =>
            busy_r<='1';
            if clear_addr<TOTAL_PIX-1 then
              clear_addr<=clear_addr+1;
            end if;

          when START_READER =>
            busy_r<='1';
            wr_start<='1';

          when RUN_SUBITER =>
            busy_r<='1';

          when NEXT_SUBITER =>
            busy_r<='1';
            if subiter='0' then subiter<='1'; else subiter<='0'; end if;
            if total_subiters<2*MAX_ITERS then
              total_subiters<=total_subiters+1;
            end if;

          when DONE =>
            busy_r<='0';
            done_r<='1';  -- latch done until next start

        end case;
      end if;
    end if;
  end process;
  ------------------------------------------------------
  -- WRITE PROCESS drive destination writes (ThinA/ThinB via dst_sel mux)
  -----------------------------------------------------
process(cur_state, clear_addr, wr_pixel_valid, subiter, cx8, cy8,
        w00,w01,w02, w10,w11,w12, w20,w21,w22)
  -- declare variables ONCE here (process declarative region)
  variable c,p2,p3,p4,p5,p6,p7,p8,p9 : std_logic;
  variable N,A : integer;
  variable del : boolean;
begin
  -- safe defaults
  dst_we   <= '0';
  dst_addr <= (others => '0');
  dst_din  <= (others => '0');

  case cur_state is
    when CLEAR_DST =>
      dst_we   <= '1';
      dst_addr <= clear_addr;
      dst_din  <= (others => '0');

    when RUN_SUBITER =>
      if wr_pixel_valid = '1' then
        -- use the variables here...
        c  := pix_to_bit(w11);
        p2 := pix_to_bit(w01); p3 := pix_to_bit(w02); p4 := pix_to_bit(w12);
        p5 := pix_to_bit(w22); p6 := pix_to_bit(w21); p7 := pix_to_bit(w20);
        p8 := pix_to_bit(w10); p9 := pix_to_bit(w00);

        N := b2i(p2='1') + b2i(p3='1') + b2i(p4='1') + b2i(p5='1') +
             b2i(p6='1') + b2i(p7='1') + b2i(p8='1') + b2i(p9='1');
        A := cn_transitions(p2,p3,p4,p5,p6,p7,p8,p9);

        if subiter='0' then
          del := (c='1') and (N>=2) and (N<=6) and (A=1) and
          ------------------------------------       ((p2='0') or (p4='0') or (p6='0')) and
                 ((p4='0') or (p6='0') or (p8='0'));
        else
          del := (c='1') and (N>=2) and (N<=6) and (A=1) and
              ------------------------------------   ((p2='0') or (p4='0') or (p8='0')) and
                 ((p2='0') or (p6='0') or (p8='0'));
        end if;

        dst_we   <= '1';
        dst_addr <= addr_of(cx8, cy8);
        if del then
          dst_din <= (others => '0');
        else
          dst_din <= (others => '1');  -- = x"FF" for 8-bit
        end if;
      end if;

    when others => null;
  end case;
end process;

  ---------------------------------------------------------------------------
  -- Next state logic
  ---------------------------------------------------------------------------
  process(cur_state, thin_start_req, clear_addr, wr_done_pulse, total_subiters)
  begin
    next_state<=cur_state;
    case cur_state is
      when IDLE => if thin_start_req='1' then next_state<=PREP_SUBITER; end if;
      when PREP_SUBITER => next_state<=CLEAR_DST;
      when CLEAR_DST => if clear_addr=TOTAL_PIX-1 then next_state<=START_READER; end if;
      when START_READER => next_state<=RUN_SUBITER;
      when RUN_SUBITER =>
        if wr_done_pulse='1' then
          if (total_subiters+1)>=2*MAX_ITERS then next_state<=DONE;
          else next_state<=NEXT_SUBITER; end if;
        end if;
      when NEXT_SUBITER => next_state<=PREP_SUBITER;
      when DONE => next_state<=IDLE;
    end case;
  end process;

end architecture;
