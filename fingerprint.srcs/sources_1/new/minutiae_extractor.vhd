----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 21.09.2025 17:14:57
-- Design Name: 
-- Module Name: minutiae_extractor - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: Minutiae extractor with single-driver registers (Vivado-friendly)
-- 
-- Dependencies: image_window_reader
-- 
-- Revision:
-- Revision 0.02 - Fix multi-driven registers, keep entity intact
-- 
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minutiae_extractor is
  generic (
    DATA_WIDTH : integer := 8;
    ADDR_WIDTH : integer := 13;
    IMG_WIDTH  : integer := 68;
    IMG_HEIGHT : integer := 118;
    FEAT_DEPTH : integer := 512
  );
  port (
    clk          : in  std_logic;
    rst          : in  std_logic;

    -- Control
    s_min_start  : in  std_logic;
    s_min_busy   : out std_logic;
    s_min_done   : out std_logic;

    -- Select source buffer (final skeleton): '1' => THIN_A, '0' => THIN_B
    src_is_a     : in  std_logic;

    -- THIN_A Port-B (read)
    thinA_rd_en   : out std_logic;
    thinA_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    thinA_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- THIN_B Port-B (read)
    thinB_rd_en   : out std_logic;
    thinB_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    thinB_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Feature output BRAM (Port-A write)
    feat_we      : out std_logic;
    feat_addr    : out unsigned(8 downto 0);
    feat_din     : out std_logic_vector(31 downto 0);

    -- Status / counters
    feat_count   : out unsigned(9 downto 0);
    feat_overflow: out std_logic
  );
end entity;

architecture rtl of minutiae_extractor is

  type state_t is (IDLE, PREP, START_READER, RUN, DONE);

  constant TYPE_END   : std_logic_vector(1 downto 0) := "00";
  constant TYPE_BIF   : std_logic_vector(1 downto 0) := "01";
  constant TYPE_OTHER : std_logic_vector(1 downto 0) := "10";

  signal wr_start       : std_logic := '0';
  signal wr_pixel_valid : std_logic;
  signal wr_done_pulse  : std_logic;
  signal wr_busy_unused : std_logic;

  signal wr_rd_en       : std_logic;
  signal wr_rd_addr     : unsigned(ADDR_WIDTH-1 downto 0);
  signal wr_rd_dout     : std_logic_vector(DATA_WIDTH-1 downto 0);

  signal w00,w01,w02,w10,w11,w12,w20,w21,w22 : std_logic_vector(DATA_WIDTH-1 downto 0);
  signal cx8, cy8      : std_logic_vector(7 downto 0);

  signal cur_state, next_state : state_t := IDLE;

  signal busy_r, done_r : std_logic := '0';
  signal use_src_a      : std_logic := '0';
  signal clear_feats    : std_logic := '0';

  signal min_start_req  : std_logic := '0';

  -- BRAM routing
  signal thinA_rd_en_i   : std_logic := '0';
  signal thinA_rd_addr_i : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
  signal thinB_rd_en_i   : std_logic := '0';
  signal thinB_rd_addr_i : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');

  signal feat_we_i        : std_logic := '0';
  signal feat_addr_i      : unsigned(8 downto 0) := (others=>'0');
  signal feat_din_i       : std_logic_vector(31 downto 0) := (others=>'0');
  signal feat_count_i     : unsigned(9 downto 0) := (others=>'0');
  signal feat_overflow_i  : std_logic := '0';

  signal push_feat        : std_logic := '0';
  signal feat_din_next    : std_logic_vector(31 downto 0) := (others=>'0');

  ---------------------------------------------------------------------------
  -- Helper functions (same as before)
  ---------------------------------------------------------------------------
  function pix_to_bit(p : std_logic_vector) return std_logic is
    variable any1 : boolean := false;
  begin
    for i in p'range loop
      if p(i)='1' then any1 := true; end if;
    end loop;
    if any1 then return '1'; else return '0'; end if;
  end function;

  function sum8(b0,b1,b2,b3,b4,b5,b6,b7 : std_logic) return integer is
    variable s : integer := 0;
  begin
    if b0='1' then s:=s+1; end if; if b1='1' then s:=s+1; end if;
    if b2='1' then s:=s+1; end if; if b3='1' then s:=s+1; end if;
    if b4='1' then s:=s+1; end if; if b5='1' then s:=s+1; end if;
    if b6='1' then s:=s+1; end if; if b7='1' then s:=s+1; end if;
    return s;
  end function;

  function cn_transitions(p2,p3,p4,p5,p6,p7,p8,p9 : std_logic) return integer is
    variable t : integer := 0;
    function z2o(a,b: std_logic) return integer is
    begin
      if (a='0') and (b='1') then return 1; else return 0; end if;
    end function;
  begin
    t := t + z2o(p2,p3)+z2o(p3,p4)+z2o(p4,p5)+z2o(p5,p6)
         +z2o(p6,p7)+z2o(p7,p8)+z2o(p8,p9)+z2o(p9,p2);
    return t;
  end function;

  function pack_record(valid : std_logic;
                       typ   : std_logic_vector(1 downto 0);
                       x10   : unsigned(9 downto 0);
                       y9    : unsigned(8 downto 0);
                       cn_val,n_val : integer)
                       return std_logic_vector is
    variable rec : std_logic_vector(31 downto 0) := (others=>'0');
    variable cn4,n4 : std_logic_vector(3 downto 0);
    variable cval,nv : integer;
  begin
    if cn_val < 0 then
  cval := 0;
elsif cn_val > 15 then
  cval := 15;
else
  cval := cn_val;
end if;

if n_val < 0 then
  nv := 0;
elsif n_val > 15 then
  nv := 15;
else
  nv := n_val;
end if;
    cn4 := std_logic_vector(to_unsigned(cval,4));
    n4  := std_logic_vector(to_unsigned(nv,4));
    rec(31) := valid; rec(30 downto 29):=typ;
    rec(28 downto 19):=std_logic_vector(x10);
    rec(18 downto 10):=std_logic_vector(y9);
    rec(9 downto 6):=cn4; rec(5 downto 2):=n4;
    rec(1 downto 0):="00";
    return rec;
  end function;

begin
  ---------------------------------------------------------------------------
  -- Start latch
  ---------------------------------------------------------------------------
  process(clk, rst)
  begin
    if rst='1' then
      min_start_req <= '0';
    elsif rising_edge(clk) then
      if s_min_start='1' then
        min_start_req <= '1';
      elsif (cur_state=IDLE and next_state=PREP) then
        min_start_req <= '0';
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- Port mapping
  ---------------------------------------------------------------------------
  s_min_busy <= busy_r;
  s_min_done <= done_r;

  thinA_rd_en   <= thinA_rd_en_i;
  thinA_rd_addr <= thinA_rd_addr_i;
  thinB_rd_en   <= thinB_rd_en_i;
  thinB_rd_addr <= thinB_rd_addr_i;

  feat_we       <= feat_we_i;
  feat_addr     <= feat_addr_i;
  feat_din      <= feat_din_i;
  feat_count    <= feat_count_i;
  feat_overflow <= feat_overflow_i;

  ---------------------------------------------------------------------------
  -- Reader source select
  ---------------------------------------------------------------------------
  process(use_src_a, wr_rd_en, wr_rd_addr, thinA_rd_dout, thinB_rd_dout)
  begin
    thinA_rd_en_i   <= '0'; thinB_rd_en_i   <= '0';
    thinA_rd_addr_i <= (others=>'0'); thinB_rd_addr_i <= (others=>'0');
    wr_rd_dout <= (others=>'0');
    if use_src_a='1' then
      thinA_rd_en_i <= wr_rd_en; thinA_rd_addr_i <= wr_rd_addr;
      wr_rd_dout <= thinA_rd_dout;
    else
      thinB_rd_en_i <= wr_rd_en; thinB_rd_addr_i <= wr_rd_addr;
      wr_rd_dout <= thinB_rd_dout;
    end if;
  end process;

  rdr : entity work.image_window_reader
    generic map (
      DATA_WIDTH => DATA_WIDTH, 
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH => IMG_WIDTH, 
      IMG_HEIGHT => IMG_HEIGHT)
    port map (
      bram_rd_clk => clk, 
      rst => rst, 
      start => wr_start,
      bram_rd_en => wr_rd_en, 
      bram_rd_addr => wr_rd_addr,
      bram_rd_dout => wr_rd_dout,
      pixel_valid => wr_pixel_valid,
      center_x => cx8, 
      center_y => cy8,
      w00=>w00,w01=>w01,w02=>w02,
      w10=>w10,w11=>w11,w12=>w12,
      w20=>w20,w21=>w21,w22=>w22,
      busy=>wr_busy_unused, 
      done_pulse=>wr_done_pulse);

  ---------------------------------------------------------------------------
  -- FSM control with safe reset
  ---------------------------------------------------------------------------
  process(clk)
  begin
    if rising_edge(clk) then
      if rst='1' then
        cur_state <= IDLE;
        busy_r <= '0'; 
        done_r <= '0';
        wr_start <= '0'; 
        use_src_a <= '0';
      else
        cur_state <= next_state;
        wr_start <= '0';
        done_r <= '0';  -- auto clear unless re-asserted

        case cur_state is
          when IDLE =>
            busy_r <= '0';
            if min_start_req='1' then done_r <= '0'; end if;

          when PREP =>
            busy_r <= '1';
            use_src_a <= src_is_a;

          when START_READER =>
            busy_r <= '1'; wr_start <= '1';

          when RUN =>
            busy_r <= '1';

          when DONE =>
            busy_r <= '0'; done_r <= '1';
        end case;
      end if;
    end if;
  end process;

  process(cur_state, min_start_req, wr_done_pulse)
  begin
    next_state <= cur_state;
    case cur_state is
      when IDLE => if min_start_req='1' then next_state<=PREP; end if;
      when PREP => next_state<=START_READER;
      when START_READER => next_state<=RUN;
      when RUN =>
        if wr_done_pulse='1' then next_state<=DONE; end if;
      when DONE => next_state<=IDLE;
    end case;
  end process;

  clear_feats <= '1' when cur_state=PREP else '0';

  ---------------------------------------------------------------------------
  -- Datapath and feature register logic (unchanged, but consistent reset)
  ---------------------------------------------------------------------------
  process(clk)
    variable c,p2,p3,p4,p5,p6,p7,p8,p9 : std_logic;
    variable N,CN : integer;
    variable typ : std_logic_vector(1 downto 0);
    variable xv : unsigned(9 downto 0);
    variable yv : unsigned(8 downto 0);
  begin
    if rising_edge(clk) then
      if rst='1' then
        push_feat <= '0'; feat_din_next <= (others=>'0');
      else
        push_feat <= '0';
        if (cur_state=RUN and wr_pixel_valid='1') then
          c  := pix_to_bit(w11);
          p2 := pix_to_bit(w01); p3 := pix_to_bit(w02); p4 := pix_to_bit(w12);
          p5 := pix_to_bit(w22); p6 := pix_to_bit(w21); p7 := pix_to_bit(w20);
          p8 := pix_to_bit(w10); p9 := pix_to_bit(w00);
          if c='1' then
            N := sum8(p2,p3,p4,p5,p6,p7,p8,p9);
            CN:= cn_transitions(p2,p3,p4,p5,p6,p7,p8,p9);
            if CN=1 then typ:=TYPE_END;
            elsif CN=3 then typ:=TYPE_BIF;
            else typ:=TYPE_OTHER; end if;
            xv:=resize(unsigned(cx8),10); yv:=resize(unsigned(cy8),9);
            feat_din_next<=pack_record('1',typ,xv,yv,CN,N);
            push_feat<='1';
          end if;
        end if;
      end if;
    end if;
  end process;

  process(clk)
    constant FEAT_DEPTH_U : unsigned(feat_count_i'range):=to_unsigned(FEAT_DEPTH,feat_count_i'length);
  begin
    if rising_edge(clk) then
      if rst='1' then
        feat_we_i<='0'; feat_addr_i<=(others=>'0');
        feat_din_i<=(others=>'0'); feat_count_i<=(others=>'0');
        feat_overflow_i<='0';
      else
        feat_we_i<='0';
        if clear_feats='1' then
          feat_addr_i<=(others=>'0'); feat_count_i<=(others=>'0');
          feat_overflow_i<='0';
        end if;
        if push_feat='1' then
          if feat_count_i<FEAT_DEPTH_U then
            feat_din_i<=feat_din_next; feat_we_i<='1';
            feat_addr_i<=resize(feat_count_i, feat_addr_i'length); 
            feat_count_i<=feat_count_i+1;
          else
            feat_overflow_i<='1';
          end if;
        end if;
      end if;
    end if;
  end process;

end architecture;
