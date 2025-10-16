library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity vga_timing_640x480 is
  port (
    clk_pix      : in  std_logic;  -- 25 MHz pixel clock
    rst_n        : in  std_logic;

    hsync        : out std_logic;
    vsync        : out std_logic;
    active_video : out std_logic;  -- '1' during visible 640x480
    x            : out unsigned(9 downto 0); -- 0..639 valid when active_video='1'
    y            : out unsigned(9 downto 0)  -- 0..479 valid when active_video='1'
  );
end vga_timing_640x480;

architecture rtl of vga_timing_640x480 is
  -- 640x480@60Hz timing (25.175 MHz nominal; 25.000 MHz commonly accepted)
  constant H_VISIBLE : integer := 640;
  constant H_FP      : integer := 16;
  constant H_SYNC    : integer := 96;   -- HSYNC low
  constant H_BP      : integer := 48;
  constant H_TOTAL   : integer := H_VISIBLE + H_FP + H_SYNC + H_BP; -- 800

  constant V_VISIBLE : integer := 480;
  constant V_FP      : integer := 10;
  constant V_SYNC    : integer := 2;    -- VSYNC low
  constant V_BP      : integer := 33;
  constant V_TOTAL   : integer := V_VISIBLE + V_FP + V_SYNC + V_BP; -- 525

  signal h_cnt11 : unsigned(10 downto 0) := (others=>'0'); -- 0..799
  signal v_cnt10 : unsigned(9 downto 0)  := (others=>'0'); -- 0..524

  signal hsync_i, vsync_i, active_i : std_logic := '0';
  signal x_i : unsigned(9 downto 0) := (others=>'0');
  signal y_i : unsigned(9 downto 0) := (others=>'0');
begin
  process(clk_pix, rst_n)
  begin
    if rst_n = '0' then
      h_cnt11 <= (others=>'0');
      v_cnt10 <= (others=>'0');
    elsif rising_edge(clk_pix) then
      if h_cnt11 = to_unsigned(H_TOTAL-1, h_cnt11'length) then
        h_cnt11 <= (others=>'0');
        if v_cnt10 = to_unsigned(V_TOTAL-1, v_cnt10'length) then
          v_cnt10 <= (others=>'0');
        else
          v_cnt10 <= v_cnt10 + 1;
        end if;
      else
        h_cnt11 <= h_cnt11 + 1;
      end if;
    end if;
  end process;

  -- Syncs (active low)
  hsync_i <= '0' when (to_integer(h_cnt11) >= (H_VISIBLE + H_FP) and
                       to_integer(h_cnt11) <  (H_VISIBLE + H_FP + H_SYNC)) else '1';

  vsync_i <= '0' when (to_integer(v_cnt10) >= (V_VISIBLE + V_FP) and
                       to_integer(v_cnt10) <  (V_VISIBLE + V_FP + V_SYNC)) else '1';

  active_i <= '1' when (to_integer(h_cnt11) < H_VISIBLE and to_integer(v_cnt10) < V_VISIBLE) else '0';

  -- Visible coordinates
  x_i <= resize(h_cnt11(9 downto 0), 10);
  y_i <= v_cnt10;

  -- Outputs
  hsync        <= hsync_i;
  vsync        <= vsync_i;
  active_video <= active_i;
  x            <= x_i;
  y            <= y_i;
end rtl;
