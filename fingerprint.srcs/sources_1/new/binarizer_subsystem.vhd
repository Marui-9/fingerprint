----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 16.09.2025 10:32:07
-- Design Name: 
-- Module Name: utils_pkg - Behavioral
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
use work.utils_pkg.all;  -- ceil_div, clog2

entity binarizer_subsystem is
  generic (
    DATA_WIDTH    : integer := 8;
    ADDR_WIDTH    : integer := 13;
    IMG_WIDTH     : integer := 68;
    IMG_HEIGHT    : integer := 118;
    BLOCK_SIZE    : integer := 8;
    THRESH_OFFSET : integer := 12
  );
  port (
    -- clock & reset (reader / processing domain)
    rd_clk     : in  std_logic;
    rst        : in  std_logic;

    -- control
    start      : in  std_logic; -- CU one-cycle pulse, latched internally

    -- data_bram Port-B (reader side)
    db_rd_en   : out std_logic;
    db_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    db_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- VGA/reader port for the resulting binary image
    bin_vga_clk : in  std_logic;
    bin_vga_en  : in  std_logic;
    bin_vga_addr: in  unsigned(ADDR_WIDTH-1 downto 0);
    bin_vga_dout: out std_logic_vector(7 downto 0);

    -- Bridge write to thinning source BRAM (Port-A style)
    ts_a_we     : out std_logic;
    ts_a_addr   : out unsigned(ADDR_WIDTH-1 downto 0);
    ts_a_din    : out std_logic_vector(7 downto 0);
    ts_a_en     : out std_logic := '1';

    -- status
    busy       : out std_logic;
    done_pulse : out std_logic   -- level: stays '1' after finish until next accepted start
  );
end entity;

architecture rtl of binarizer_subsystem is
  ---------------------------------------------------------------------------
  -- Constants
  ---------------------------------------------------------------------------
  constant NBX          : integer := ceil_div(IMG_WIDTH,  BLOCK_SIZE);
  constant NBY          : integer := ceil_div(IMG_HEIGHT, BLOCK_SIZE);
  constant NBLOCKS      : integer := NBX * NBY;
  constant THR_AW       : integer := clog2(NBLOCKS);
  constant TOTAL_PIXELS : integer := IMG_WIDTH * IMG_HEIGHT;

  ---------------------------------------------------------------------------
  -- Threshold builder I/F
  ---------------------------------------------------------------------------
  signal tb_start    : std_logic := '0';
  signal tb_busy     : std_logic := '0';
  signal tb_done     : std_logic := '0';
  signal tb_rd_en    : std_logic := '0';
  signal tb_rd_addr  : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal thr_wr_en   : std_logic := '0';
  signal thr_wr_addr : unsigned(THR_AW-1 downto 0) := (others => '0');
  signal thr_wr_data : std_logic_vector(7 downto 0) := (others => '0');

  -- THR_BRAM read side (for binarizer)
  signal thr_rd_en   : std_logic := '0';
  signal thr_rd_addr : unsigned(THR_AW-1 downto 0) := (others => '0');
  signal thr_rd_data : std_logic_vector(7 downto 0) := (others => '0');

  ---------------------------------------------------------------------------
  -- Image window reader (pass 2) + binarizer stream
  ---------------------------------------------------------------------------
  component image_window_reader
    generic (
      DATA_WIDTH : integer := 8;
      ADDR_WIDTH : integer := 13;
      IMG_WIDTH  : integer := 68;
      IMG_HEIGHT : integer := 118
    );
    port (
      bram_rd_clk  : in  std_logic;
      rst          : in  std_logic;
      start        : in  std_logic;

      bram_rd_en   : out std_logic;
      bram_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
      bram_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

      pixel_valid  : out std_logic;
      center_x     : out std_logic_vector(7 downto 0);
      center_y     : out std_logic_vector(7 downto 0);

      w00 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w01 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w02 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w10 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w11 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w12 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w20 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w21 : out std_logic_vector(DATA_WIDTH-1 downto 0);
      w22 : out std_logic_vector(DATA_WIDTH-1 downto 0);

      busy         : out std_logic;
      done_pulse   : out std_logic
    );
  end component;

  signal ir_start     : std_logic := '0';
  signal ir_rd_en     : std_logic := '0';
  signal ir_rd_addr   : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal ir_pixel_v   : std_logic := '0';
  signal ir_cx, ir_cy : std_logic_vector(7 downto 0) := (others => '0');
  signal ir_w11       : std_logic_vector(7 downto 0) := (others => '0');
  signal ir_done      : std_logic := '0';

  signal bin_wr_en    : std_logic := '0';
  signal bin_wr_addr  : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bin_wr_data  : std_logic_vector(7 downto 0) := (others => '0');

  ---------------------------------------------------------------------------
  -- BINARY BRAM A-port (writer)
  ---------------------------------------------------------------------------
  signal bin_a_en    : std_logic := '0';
  signal bin_a_we    : std_logic := '0';
  signal bin_a_addr  : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bin_a_din   : std_logic_vector(7 downto 0) := (others => '0');

  -- Clear loop
  signal clr_addr    : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');

  ---------------------------------------------------------------------------
  -- data_bram Port-B mux (registered)
  ---------------------------------------------------------------------------
  signal db_en_mux   : std_logic := '0';
  signal db_addr_mux : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal db_en_q     : std_logic := '0';
  signal db_addr_q   : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');

  ---------------------------------------------------------------------------
  -- FSM & status
  ---------------------------------------------------------------------------
  type state_t is (IDLE, RUN_TB, CLEAR_BIN, START_IR, RUN_BIN, DONE);
  signal state  : state_t := IDLE;

  signal busy_r : std_logic := '0';
  signal done_l : std_logic := '0';  -- latched done (level)

  -- start latch
  signal start_req : std_logic := '0';

begin
  -----------------------------------------------------------------------------
  -- Start latch & Done latch (level)
  -----------------------------------------------------------------------------
  process(rd_clk)
  begin
    if rising_edge(rd_clk) then
      if rst = '1' then
        start_req <= '0';
        done_l    <= '0';
      else
        if start = '1' then
          start_req <= '1';
        end if;

        -- accept a pending start while idle
        if (state = IDLE) and (start_req = '1') then
          start_req <= '0';
          done_l    <= '0';
        end if;

        -- latch done when finishing RUN_BIN
        if (state = RUN_BIN) and (ir_done = '1') then
          done_l <= '1';
        end if;
      end if;
    end if;
  end process;

  busy       <= busy_r;
  done_pulse <= done_l;  -- exported as level

  -----------------------------------------------------------------------------
  -- External connections to DATA BRAM (registered to be glitch-safe)
  -----------------------------------------------------------------------------
  db_rd_en   <= db_en_q;
  db_rd_addr <= db_addr_q;

  process(rd_clk)
  begin
    if rising_edge(rd_clk) then
      if rst = '1' then
        db_en_q   <= '0';
        db_addr_q <= (others => '0');
      else
        db_en_q   <= db_en_mux;
        db_addr_q <= db_addr_mux;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- Mirror binary writes to thinning source BRAM Port-A
  -----------------------------------------------------------------------------
  ts_a_en   <= bin_a_en;
  ts_a_we   <= bin_a_we;
  ts_a_addr <= bin_a_addr;
  ts_a_din  <= bin_a_din;

  -----------------------------------------------------------------------------
  -- Binary image BRAM (A: writer here, B: VGA)
  -----------------------------------------------------------------------------
  bin_bram_inst : entity work.binary_bram
    generic map (
      DATA_WIDTH => 8,
      ADDR_WIDTH => ADDR_WIDTH
    )
    port map (
      -- Port A (writer in rd_clk domain)
      a_clk  => rd_clk,
      a_en   => bin_a_en,
      a_we   => bin_a_we,
      a_addr => bin_a_addr,
      a_din  => bin_a_din,
      -- Port B (VGA)
      b_clk  => bin_vga_clk,
      b_en   => bin_vga_en,
      b_addr => bin_vga_addr,
      b_dout => bin_vga_dout
    );

  -----------------------------------------------------------------------------
  -- Threshold BRAM + Builder
  -----------------------------------------------------------------------------
  thr_bram_inst : entity work.thr_bram
    generic map (
      DATA_WIDTH => 8,
      ADDR_WIDTH => THR_AW
    )
    port map (
      clk     => rd_clk,
      wr_en   => thr_wr_en,
      wr_addr => thr_wr_addr,
      wr_data => thr_wr_data,
      rd_en   => thr_rd_en,
      rd_addr => thr_rd_addr,
      rd_data => thr_rd_data
    );

  tb_inst : entity work.threshold_builder
    generic map (
      DATA_WIDTH    => DATA_WIDTH,
      ADDR_WIDTH    => ADDR_WIDTH,
      IMG_WIDTH     => IMG_WIDTH,
      IMG_HEIGHT    => IMG_HEIGHT,
      BLOCK_SIZE    => BLOCK_SIZE,
      THRESH_OFFSET => THRESH_OFFSET
    )
    port map (
      clk         => rd_clk,
      rst         => rst,
      start       => tb_start,      -- one-cycle pulse
      rd_en       => tb_rd_en,
      rd_addr     => tb_rd_addr,
      rd_dout     => db_rd_dout,
      thr_wr_en   => thr_wr_en,
      thr_wr_addr => thr_wr_addr,
      thr_wr_data => thr_wr_data,
      busy        => tb_busy,
      done_pulse  => tb_done
    );

  -----------------------------------------------------------------------------
  -- Image window reader + binarizer stream
  -----------------------------------------------------------------------------
  ir_inst : image_window_reader
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT
    )
    port map (
      bram_rd_clk  => rd_clk,
      rst          => rst,
      start        => ir_start,     -- one-cycle pulse
      bram_rd_en   => ir_rd_en,
      bram_rd_addr => ir_rd_addr,
      bram_rd_dout => db_rd_dout,
      pixel_valid  => ir_pixel_v,
      center_x     => ir_cx,
      center_y     => ir_cy,
      w00 => open, w01 => open, w02 => open,
      w10 => open, w11 => ir_w11, w12 => open,
      w20 => open, w21 => open, w22 => open,
      busy         => open,
      done_pulse   => ir_done
    );

  bin_stream_inst : entity work.binarizer_stream
    generic map (
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT,
      BLOCK_SIZE => BLOCK_SIZE,
      ADDR_WIDTH => ADDR_WIDTH
    )
    port map (
      clk         => rd_clk,
      rst         => rst,
      pixel_valid => ir_pixel_v,
      center_x    => ir_cx,
      center_y    => ir_cy,
      center_pix  => ir_w11,
      thr_rd_en   => thr_rd_en,
      thr_rd_addr => thr_rd_addr,
      thr_rd_data => thr_rd_data,
      bin_wr_en   => bin_wr_en,
      bin_wr_addr => bin_wr_addr,
      bin_wr_data => bin_wr_data
    );

  -----------------------------------------------------------------------------
  -- DATA BRAM Port-B source mux (TB during RUN_TB; IR during START_IR/RUN_BIN)
  -----------------------------------------------------------------------------
  db_en_mux <=  tb_rd_en when (state = RUN_TB) else
                ir_rd_en when (state = START_IR or state = RUN_BIN) else
                '0';

  db_addr_mux <= tb_rd_addr when (state = RUN_TB) else
                 ir_rd_addr when (state = START_IR or state = RUN_BIN) else
                 (others => '0');

  -----------------------------------------------------------------------------
  -- Datapath: drive binary BRAM A-port (and mirror to ts_a_*)
  -----------------------------------------------------------------------------
  process(rd_clk)
  begin
    if rising_edge(rd_clk) then
      if rst = '1' then
        bin_a_en   <= '0';
        bin_a_we   <= '0';
        bin_a_addr <= (others => '0');
        bin_a_din  <= (others => '0');
      else
        -- defaults each cycle
        bin_a_en <= '0';
        bin_a_we <= '0';

        case state is
          when CLEAR_BIN =>
            if to_integer(clr_addr) < TOTAL_PIXELS then
              bin_a_en   <= '1';
              bin_a_we   <= '1';
              bin_a_addr <= clr_addr;
              bin_a_din  <= (others => '0');
            end if;

          when RUN_BIN =>
            if bin_wr_en = '1' then
              bin_a_en   <= '1';
              bin_a_we   <= '1';
              bin_a_addr <= bin_wr_addr;
              bin_a_din  <= bin_wr_data;
            end if;

          when others =>
            null;
        end case;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- FSM: issues one-shots tb_start / ir_start and owns counters
  -----------------------------------------------------------------------------
  process(rd_clk)
  begin
    if rising_edge(rd_clk) then
      if rst = '1' then
        state    <= IDLE;
        busy_r   <= '0';
        tb_start <= '0';
        ir_start <= '0';
        clr_addr <= (others => '0');
      else
        -- default one-shots low
        tb_start <= '0';
        ir_start <= '0';

        case state is
          when IDLE =>
            busy_r <= '0';
            if start_req = '1' then
              busy_r   <= '1';
              tb_start <= '1';
              state    <= RUN_TB;
            end if;

          when RUN_TB =>
            if (tb_done = '1') and (tb_busy = '0') then
              clr_addr <= (others => '0');
              state    <= CLEAR_BIN;
            end if;

          when CLEAR_BIN =>
            if to_integer(clr_addr) < (TOTAL_PIXELS - 1) then
              clr_addr <= clr_addr + 1;
            else
              ir_start <= '1';          -- kick window reader
              state    <= START_IR;
            end if;

          when START_IR =>
            state <= RUN_BIN;

          when RUN_BIN =>
            if ir_done = '1' then
              busy_r <= '0';
              state  <= DONE;
            end if;

          when DONE =>
            -- hold here (done_l already set) until next accepted start
            state <= IDLE;

          when others =>
            state <= IDLE;
        end case;
      end if;
    end if;
  end process;

end architecture;

