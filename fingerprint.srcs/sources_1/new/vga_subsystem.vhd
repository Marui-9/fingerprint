library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity vga_subsystem is
  generic (
    H_RES   : integer := 640;
    V_RES   : integer := 480;
    IMG_W   : integer := 68;
    IMG_H   : integer := 118;
    SCALE   : integer := 4;
    WIN_X0  : integer := 184;  -- centered for 272x472
    WIN_Y0  : integer := 4
  );
  --port map
  ------------------------------------------------------------------
  port (
    -- clocks & reset
    clk_pix       : in  std_logic;  -- 25 MHz
    rst_n         : in  std_logic;
    
    
    -- control
    sw_vga_sel    : in  std_logic_vector(1 downto 0); -- 00=binary, 01=thinA, 10=thinB, 11=thinB+overlay
    overlay_en    : in  std_logic;                    -- can tie '1' when sw_vga_sel="11"
    min_done_p    : in  std_logic;                    -- trigger to (re)build overlay after MIN stage

    -- BRAM Port-B (read-only from VGA)
    -- binary
    bin_dout      : in  std_logic_vector(7 downto 0);
    bin_addr      : out std_logic_vector(12 downto 0);
    bin_en        : out std_logic;
    -- thinA
    thinA_dout    : in  std_logic_vector(7 downto 0);
    thinA_addr    : out std_logic_vector(12 downto 0);
    thinA_en      : out std_logic;
    -- thinB
    thinB_dout    : in  std_logic_vector(7 downto 0);
    thinB_addr    : out std_logic_vector(12 downto 0);
    thinB_en      : out std_logic;

    -- minutiae list BRAM (Port-B read for rasterizer)
    min_list_dout : in  std_logic_vector(31 downto 0);
    min_list_addr : out std_logic_vector(8 downto 0);   -- 0..511
    min_list_en   : out std_logic;

    -- VGA out
    vga_hsync     : out std_logic;
    vga_vsync     : out std_logic;
    vga_r         : out std_logic_vector(7 downto 0);
    vga_g         : out std_logic_vector(7 downto 0);
    vga_b         : out std_logic_vector(7 downto 0)
    
  );
end vga_subsystem; 
---------------------------------------------------------
architecture rtl of vga_subsystem is
  -- Timing generator
  component vga_timing_640x480
    port (
      clk_pix      : in  std_logic;
      rst_n        : in  std_logic;
      hsync        : out std_logic;
      vsync        : out std_logic;
      active_video : out std_logic;
      x            : out unsigned(9 downto 0);
      y            : out unsigned(9 downto 0)
    );
  end component;

  -- Overlay bitmap RAM
  component dp_ram_simple
    generic (
      ADDR_W : integer := 13;
      DATA_W : integer := 8
    );
    port (
      clk    : in  std_logic;
      a_en   : in  std_logic;
      a_we   : in  std_logic;
      a_addr : in  std_logic_vector(ADDR_W-1 downto 0);
      a_din  : in  std_logic_vector(DATA_W-1 downto 0);
      a_dout : out std_logic_vector(DATA_W-1 downto 0);
      b_en   : in  std_logic;
      b_addr : in  std_logic_vector(ADDR_W-1 downto 0);
      b_dout : out std_logic_vector(DATA_W-1 downto 0)   -- <-- fixed "downto"
    );
  end component;

  -- Timing signals
  signal hsync_i, vsync_i, active_i : std_logic;
  signal vga_x, vga_y : unsigned(9 downto 0);

  -- Window detection
  signal in_window         : std_logic;
  signal img_x_int         : integer range 0 to IMG_W-1 := 0; -- 0..67
  signal img_y_int         : integer range 0 to IMG_H-1 := 0; -- 0..117

  -- Address into image BRAMs
  signal addr_int          : integer range 0 to IMG_W*IMG_H-1 := 0; -- up to 8023
  signal addr_vec          : std_logic_vector(12 downto 0);

  -- Pipeline align (account for 1-cycle BRAM latency)
  signal in_window_d, active_d : std_logic := '0';
  signal base_px_d             : std_logic_vector(7 downto 0) := (others=>'0');

  -- Overlay RAM Port-A (rasterizer) / Port-B (VGA)
  signal ovl_a_en, ovl_a_we : std_logic := '0';
  signal ovl_a_addr         : std_logic_vector(12 downto 0) := (others=>'0');
  signal ovl_a_din          : std_logic_vector(7 downto 0)  := (others=>'0');
  signal ovl_a_dout         : std_logic_vector(7 downto 0);
  signal ovl_b_en           : std_logic := '0';
  signal ovl_b_addr         : std_logic_vector(12 downto 0) := (others=>'0');
  signal ovl_b_dout         : std_logic_vector(7 downto 0);

  -- Selected base pixel (combinational pre-latency)
  signal base_px            : std_logic_vector(7 downto 0);

  -- Edge detect for min_done_p
  signal min_done_q  : std_logic := '0';
  signal rast_start  : std_logic := '0';
  
  signal md_sync1, md_sync2 : std_logic := '0';
signal md_pe              : std_logic := '0';

  -- Rasterizer FSM
  type rstate_t is (R_IDLE, R_CLEAR, R_SCAN_ADDR, R_SCAN_READ, R_DRAW_SETUP, R_DRAW_PX);
  signal rstate       : rstate_t := R_IDLE;
  signal clear_addr   : integer range 0 to IMG_W*IMG_H := 0;

  signal min_idx      : integer range 0 to 511 := 0;
  signal min_valid    : std_logic := '0';
  signal min_type     : std_logic_vector(1 downto 0) := (others=>'0');
  signal min_x        : integer range 0 to IMG_W-1 := 0; -- 0..67
  signal min_y        : integer range 0 to IMG_H-1 := 0; -- 0..117

  -- Draw cross/X pattern
  type xy_pair is record dx : integer; dy : integer; end record;
  type pattern_t is array (natural range <>) of xy_pair;

  constant P_LEN : integer := 13;
  constant PAT : pattern_t := (
    (0,0),
    (-1,0), (1,0), (-2,0), (2,0),
    (0,-1), (0,1), (0,-2), (0,2),
    (-1,-1), (1,1), (-1,1), (1,-1)
  );
  signal p_idx        : integer range 0 to P_LEN-1 := 0;

  -- RGB regs
  signal r_i, g_i, b_i : std_logic_vector(7 downto 0) := (others=>'0');
begin
  --------------------------------------------------------------------------
  -- Timing generator
  --------------------------------------------------------------------------
  u_tmg: vga_timing_640x480
    port map (
      clk_pix      => clk_pix,
      rst_n        => rst_n,
      hsync        => hsync_i,
      vsync        => vsync_i,
      active_video => active_i,
      x            => vga_x,
      y            => vga_y
    );

  vga_hsync <= hsync_i;
  vga_vsync <= vsync_i;

  -- Window detection (centered 272x472 window)
  in_window <= '1' when (to_integer(vga_x) >= WIN_X0 and to_integer(vga_x) < (WIN_X0 + IMG_W*SCALE) and
                         to_integer(vga_y) >= WIN_Y0 and to_integer(vga_y) < (WIN_Y0 + IMG_H*SCALE))
               else '0';

  -- Source coordinates by /SCALE (SCALE=4)
  process(vga_x, vga_y, in_window)
    variable xw, yw : integer;
  begin
    if in_window = '1' then
      xw := to_integer(vga_x) - WIN_X0;
      yw := to_integer(vga_y) - WIN_Y0;
      img_x_int <= xw / SCALE;  -- 0..67
      img_y_int <= yw / SCALE;  -- 0..117
    else
      img_x_int <= 0;
      img_y_int <= 0;
    end if;
  end process;

  -- Address = y * IMG_W + x
  process(img_x_int, img_y_int)
    variable a : integer;
  begin
    a := img_y_int * IMG_W + img_x_int; -- 0..8023
    addr_int <= a;
    addr_vec <= std_logic_vector(to_unsigned(a, 13));
  end process;

  -- Drive BRAM Port-B address/enables (only one source active)
  bin_addr   <= addr_vec;
  thinA_addr <= addr_vec;
  thinB_addr <= addr_vec;

  bin_en   <= in_window when (sw_vga_sel = "00") else '0';
  thinA_en <= in_window when (sw_vga_sel = "01") else '0';
  thinB_en <= in_window when (sw_vga_sel = "10" or sw_vga_sel = "11") else '0'; -- thinB is base for overlay

  -- Select base pixel (combinational)
  with sw_vga_sel select base_px <=
    bin_dout    when "00",
    thinA_dout  when "01",
    thinB_dout  when others;  -- "10" or "11"

  -- Overlay RAM Port-B read at same address (only when overlay enabled & within window)
  ovl_b_addr <= addr_vec;
  ovl_b_en   <= in_window and overlay_en;

  -- Pipeline align for 1-cycle RAM latency
  process(clk_pix, rst_n)
  begin
    if rst_n='0' then
    md_sync1 <= '0'; md_sync2 <= '0'; md_pe <= '0';
      in_window_d <= '0';
      active_d    <= '0';
      base_px_d   <= (others=>'0');
    elsif rising_edge(clk_pix) then
      in_window_d <= in_window;
      active_d    <= active_i;
      base_px_d   <= base_px; -- aligns with RAM read latency
      md_sync1 <= min_done_p;
    md_sync2 <= md_sync1;
    md_pe    <= md_sync1 and not md_sync2;
    end if;
  end process;

  -- Color mapping + overlay combine
  process(clk_pix, rst_n)
    variable y8 : unsigned(7 downto 0);
  begin
    if rst_n='0' then
      r_i <= (others=>'0'); g_i <= (others=>'0'); b_i <= (others=>'0');
    elsif rising_edge(clk_pix) then
      if active_d = '1' and in_window_d = '1' then
        y8 := unsigned(base_px_d);
        r_i<=x"0A"; g_i<=x"00"; b_i<=x"FF";
        if (overlay_en = '1') and (ovl_b_dout /= x"00") then
            case ovl_b_dout is
              when x"01" => r_i<=x"FF"; g_i<=(others=>'0'); b_i<=(others=>'0'); -- red
              when x"02" => r_i<=(others=>'0'); g_i<=x"FF"; b_i<=(others=>'0'); -- green
              when others=> r_i<=(others=>'0'); g_i<=(others=>'0'); b_i<=x"FF"; -- blue
            end case;
        else
        
         -- r_i <= std_logic_vector(y8); g_i <= std_logic_vector(y8); b_i <= std_logic_vector(y8);
          r_i <= base_px_d; g_i <= base_px_d; b_i <= base_px_d;
        end if;
      else
        r_i <= (others=>'0'); g_i <= (others=>'0'); b_i <= (others=>'1');
      end if;
    end if;
  end process;

  vga_r <= r_i;
  vga_g <= g_i;
  vga_b <= b_i;

  --------------------------------------------------------------------------
  -- Overlay bitmap RAM (Port-A: rasterizer write, Port-B: VGA read)
  --------------------------------------------------------------------------
  u_ovl: dp_ram_simple
    generic map (
      ADDR_W => 13,
      DATA_W => 8
    )
    port map (
      clk    => clk_pix,
      a_en   => ovl_a_en,
      a_we   => ovl_a_we,
      a_addr => ovl_a_addr,
      a_din  => ovl_a_din,
      a_dout => ovl_a_dout, -- unused
      b_en   => ovl_b_en,
      b_addr => ovl_b_addr,
      b_dout => ovl_b_dout
    );

  --------------------------------------------------------------------------
  -- Minutiae overlay rasterizer (internal FSM)
  -- Minutiae list entry (32-bit): [31]=valid, [30:29]=type, [28:20]=x(9b), [19:10]=y(10b), [9:0]=meta
  --------------------------------------------------------------------------

  -- Edge detect for min_done_p
  process(clk_pix, rst_n)
  begin
    if rst_n='0' then
      min_done_q <= '0';
      rast_start <= '0';
    elsif rising_edge(clk_pix) then
      rast_start <= '0';
      if (min_done_q = '0') and (min_done_p = '1') then
        rast_start <= '1';
      end if;
      min_done_q <= min_done_p;
    end if;
  end process;
 
process(rstate)
begin
  if (rstate = R_CLEAR) or (rstate = R_DRAW_PX) then
    ovl_a_en <= '1';
    ovl_a_we <= '1';
  else
    ovl_a_en <= '0';
    ovl_a_we <= '0';
  end if;
end process;

-- VHDL-93 safe drive for Port-A data
process(rstate, min_type)
begin
  if rstate = R_CLEAR then
    ovl_a_din <= x"00";
  else
    case min_type is
      when "00"   => ovl_a_din <= x"01"; -- ending: RED
      when "01"   => ovl_a_din <= x"02"; -- bifurcation: GREEN
      when others => ovl_a_din <= x"03"; -- other types: BLUE
    end case;
  end if;
end process;
    

  -- Minutiae list BRAM read control (Port-B style interface)
  min_list_en   <= '1' when (rstate = R_SCAN_ADDR or rstate = R_SCAN_READ) else '0';
  min_list_addr <= std_logic_vector(to_unsigned(min_idx, 9));

  -- Rasterizer FSM
  process(clk_pix, rst_n)
    variable word_v : std_logic_vector(31 downto 0);
    variable xv, yv : integer;
    variable dx, dy : integer;
    variable tx, ty : integer;
    variable addr_v : integer;
  begin
    if rst_n='0' then
      rstate     <= R_IDLE;
      clear_addr <= 0;
      min_idx    <= 0;
      min_valid  <= '0';
      min_type   <= (others=>'0');
      min_x      <= 0;
      min_y      <= 0;
      p_idx      <= 0;
      ovl_a_addr <= (others=>'0');
    elsif rising_edge(clk_pix) then
      case rstate is
        when R_IDLE =>
        
          if rast_start = '1' then
            clear_addr <= 0;
            rstate     <= R_CLEAR;
            else
             -- idle screen color
      --            r_i <= x"00";
     --             g_i <= x"00";
     --             b_i <= x"40";  -- blue background
          end if;

        when R_CLEAR =>
          ovl_a_addr <= std_logic_vector(to_unsigned(clear_addr, 13));
          if clear_addr = IMG_W*IMG_H-1 then
            min_idx <= 0;
            rstate  <= R_SCAN_ADDR;
          else
            clear_addr <= clear_addr + 1;
          end if;

        when R_SCAN_ADDR =>
          -- present address; read next cycle
          rstate <= R_SCAN_READ;

        when R_SCAN_READ =>
          word_v    := min_list_dout;
          min_valid <= word_v(31);
          min_type  <= word_v(30 downto 29);
          xv := to_integer(unsigned(word_v(28 downto 20))); -- x (0..67)
          yv := to_integer(unsigned(word_v(19 downto 10))); -- y (0..117)
          -- clamp
          if xv < 0 then xv := 0; end if;
          if xv > (IMG_W-1) then xv := IMG_W-1; end if;
          if yv < 0 then yv := 0; end if;
          if yv > (IMG_H-1) then yv := IMG_H-1; end if;
          min_x <= xv;
          min_y <= yv;

          if min_valid = '1' then
            p_idx  <= 0;
            rstate <= R_DRAW_SETUP;
          else
            if min_idx = 511 then
              rstate <= R_IDLE;
            else
              min_idx <= min_idx + 1;
              rstate  <= R_SCAN_ADDR;
            end if;
          end if;

        when R_DRAW_SETUP =>
          dx := PAT(p_idx).dx;
          dy := PAT(p_idx).dy;
          tx := min_x + dx;
          ty := min_y + dy;
          if (tx >= 0) and (tx < IMG_W) and (ty >= 0) and (ty < IMG_H) then
            addr_v := ty * IMG_W + tx;
            ovl_a_addr <= std_logic_vector(to_unsigned(addr_v, 13));
            rstate <= R_DRAW_PX;
          else
            if p_idx = P_LEN-1 then
              if min_idx = 511 then
                rstate <= R_IDLE;
              else
                min_idx <= min_idx + 1;
                rstate  <= R_SCAN_ADDR;
              end if; 
            else
              p_idx  <= p_idx + 1;
              rstate <= R_DRAW_SETUP;
            end if;
          end if;

        when R_DRAW_PX =>
          if p_idx = P_LEN-1 then
            if min_idx = 511 then
              rstate <= R_IDLE;
            else
              min_idx <= min_idx + 1;
              rstate  <= R_SCAN_ADDR;
            end if;
          else
            p_idx  <= p_idx + 1;
            rstate <= R_DRAW_SETUP;
          end if;

        when others =>
          rstate <= R_IDLE;
      end case;
    end if;
  end process;
end rtl;
