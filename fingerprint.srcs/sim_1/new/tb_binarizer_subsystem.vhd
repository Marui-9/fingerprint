library ieee;
use ieee.std_logic_1164.all; 
use ieee.numeric_std.all;
use std.textio.all;

entity tb_binarizer_subsystem is
  -- testbench exposes tb_done/tb_failed so you can probe them in waveforms
end entity;

architecture sim of tb_binarizer_subsystem is
  ---------------------------------------------------------------------------
  -- Generics / parameters
  ---------------------------------------------------------------------------
  constant DATA_WIDTH : integer := 8;
  constant ADDR_WIDTH : integer := 13;  -- 2^13 = 8192 > 8024
  constant IMG_WIDTH  : integer := 68;
  constant IMG_HEIGHT : integer := 118;
  constant TOTAL_PIX  : integer := IMG_WIDTH * IMG_HEIGHT;

  ---------------------------------------------------------------------------
  -- Writer (UART-like) clock domain
  ---------------------------------------------------------------------------
  signal clk_wr  : std_logic := '0';
  signal rst_wr  : std_logic := '1';

  -- data_bram UART-side inputs
  signal data_reg_s       : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal done_reg_s       : std_logic := '0';
  signal parity_err_s     : std_logic := '0';
  signal frame_err_s      : std_logic := '0';
  signal frame_start_s    : std_logic := '0';
  signal frame_done_s     : std_logic;

  ---------------------------------------------------------------------------
  -- Reader/processing clock domain
  ---------------------------------------------------------------------------
  signal clk_rd  : std_logic := '0';
  signal rst_rd  : std_logic := '1';

  -- data_bram Port-B (to binarizer_subsystem)
  signal db_rd_en_s   : std_logic;
  signal db_rd_addr_s : unsigned(ADDR_WIDTH-1 downto 0);
  signal db_rd_dout_s : std_logic_vector(DATA_WIDTH-1 downto 0);

  ---------------------------------------------------------------------------
  -- Binarizer subsystem <-> VGA port (for reading output image)
  ---------------------------------------------------------------------------
  signal bin_vga_en_s   : std_logic := '0';
  signal bin_vga_addr_s : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal bin_vga_dout_s : std_logic_vector(7 downto 0);

  -- Subsystem status
  signal bin_busy_s : std_logic;
  signal bin_done_s : std_logic;

  ---------------------------------------------------------------------------
  -- Start synchronizer (frame_done -> rd_clk domain)
  ---------------------------------------------------------------------------
  signal fd_sync0   : std_logic := '0';
  signal fd_sync1   : std_logic := '0';
  signal start_pulse: std_logic := '0';

  ---------------------------------------------------------------------------
  -- TB pass/fail indicators (new)
  ---------------------------------------------------------------------------
  signal tb_done   : std_logic := '0';
  signal tb_failed : std_logic := '0';

begin
  ---------------------------------------------------------------------------
  -- Clocks (50 MHz each, independent domains)
  ---------------------------------------------------------------------------
  clk_wr <= not clk_wr after 10 ns;  -- 20 ns period
  clk_rd <= not clk_rd after 10 ns;  -- 20 ns period

  ---------------------------------------------------------------------------
  -- Resets
  ---------------------------------------------------------------------------
  process
  begin
    rst_wr <= '1';
    rst_rd <= '1';
    wait for 200 ns;
    rst_wr <= '0';
    rst_rd <= '0';
    wait;
  end process;

  ---------------------------------------------------------------------------
  -- DUT: data_bram (behavioral architecture provided separately/included)
  ---------------------------------------------------------------------------
  data_bram_inst : entity work.data_bram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT
    )
    port map (
      clk            => clk_wr,
      rst            => rst_wr,
      data_reg       => data_reg_s,
      done_reg       => done_reg_s,
      parity_err_reg => parity_err_s,
      frame_err_reg  => frame_err_s,
      frame_start    => frame_start_s,
      frame_done     => frame_done_s,
      rd_clk         => clk_rd,
      rd_en          => db_rd_en_s,
      rd_addr        => db_rd_addr_s,
      rd_dout        => db_rd_dout_s
    );

  ---------------------------------------------------------------------------
  -- DUT: binarizer_subsystem (thresholds + stream binarizer + binary BRAM)
  -- start now driven by synchronized single-cycle pulse start_pulse
  ---------------------------------------------------------------------------
  binarizer_ss_inst : entity work.binarizer_subsystem
    generic map (
      DATA_WIDTH    => DATA_WIDTH,
      ADDR_WIDTH    => ADDR_WIDTH,
      IMG_WIDTH     => IMG_WIDTH,
      IMG_HEIGHT    => IMG_HEIGHT,
      BLOCK_SIZE    => 8,
      THRESH_OFFSET => 12
    )
    port map (
      rd_clk       => clk_rd,
      rst          => rst_rd,
      start        => start_pulse,     -- synchronized one-cycle pulse
      db_rd_en     => db_rd_en_s,
      db_rd_addr   => db_rd_addr_s,
      db_rd_dout   => db_rd_dout_s,
      bin_vga_clk  => clk_rd,
      bin_vga_en   => bin_vga_en_s,
      bin_vga_addr => bin_vga_addr_s,
      bin_vga_dout => bin_vga_dout_s,
      busy         => bin_busy_s,
      done_pulse   => bin_done_s
    );

  ---------------------------------------------------------------------------
  -- Stimulus: generate a synthetic frame and "send" it via UART-like inputs
  -- Pattern: grayscale gradient with gentle modulation to exercise thresholds.
  ---------------------------------------------------------------------------
  writer_proc : process
    variable x, y : integer;
    impure function pix_val(xx, yy : integer) return integer is
      variable v : integer := (4*xx + 2*yy) mod 256;
    begin
      if ((xx/8) mod 2) = 1 then
        v := (v + 24) mod 256;
      end if;
      return v;
    end function;
  begin
    -- Wait until resets deassert
    wait until rst_wr = '0';
    wait for 50 ns;

    report "Writer: starting frame" severity note;

    -- Start a new frame
    frame_start_s <= '1';
    wait until rising_edge(clk_wr);
    frame_start_s <= '0';

    -- Stream all pixels (row-major) using done_reg pulses
    for yy in 0 to IMG_HEIGHT-1 loop
      for xx in 0 to IMG_WIDTH-1 loop
        data_reg_s <= std_logic_vector(to_unsigned(pix_val(xx, yy), DATA_WIDTH));
        done_reg_s <= '1';
        wait until rising_edge(clk_wr);
        done_reg_s <= '0';
        -- idle a cycle between bytes
        wait until rising_edge(clk_wr);
      end loop;
    end loop;

    report "Writer: frame transmission finished, waiting for data_bram.frame_done" severity note;

    -- data_bram's frame_done is sticky until next frame_start; wait for it
    wait until frame_done_s = '1';
    report "Writer: data_bram.frame_done asserted" severity note;

    wait;
  end process;

  -----------------------------------------------------------------------------
  -- Synchronize frame_done_s into clk_rd domain and generate one-cycle start
  -----------------------------------------------------------------------------
  process(clk_rd)
  begin
    if rising_edge(clk_rd) then
      if rst_rd = '1' then
        fd_sync0 <= '0';
        fd_sync1 <= '0';
        start_pulse <= '0';
      else
        fd_sync0 <= frame_done_s;
        fd_sync1 <= fd_sync0;

        -- one-cycle pulse on rising edge of synchronized frame_done
        if (fd_sync1 = '0') and (fd_sync0 = '1') then
          start_pulse <= '1';
          report "TB: start_pulse asserted (synchronized frame_done)" severity note;
        else
          start_pulse <= '0';
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- Monitor completion and set tb_done. Do not write files; only waveforms.
  ---------------------------------------------------------------------------
  completion_monitor : process
  begin
    wait until bin_done_s = '1';
    wait until rising_edge(clk_rd); -- align with clock
    report "TB: bin_done observed. Marking tb_done and stopping simulation." severity note;
    tb_done <= '1';
    -- keep tb_done high for waveforms; wait a short time then stop
    wait for 200 ns;
    wait;
  end process;

  ---------------------------------------------------------------------------
  -- Watchdog: set tb_failed on timeout and stop
  ---------------------------------------------------------------------------
  watchdog_proc : process
  begin
    wait for 100 ms; -- timeout
    tb_failed <= '1';
    report "TB: simulation timeout. Marking tb_failed and stopping." severity FAILURE;
    wait;
  end process;

end architecture;
