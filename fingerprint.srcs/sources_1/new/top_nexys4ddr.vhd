library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity top_nexys4ddr is
  generic (
    DATA_WIDTH  : integer := 8;
    ADDR_WIDTH  : integer := 13;  -- 68*118 = 8024 < 8192 (13 bits)
    IMG_WIDTH   : integer := 68;
    IMG_HEIGHT  : integer := 118;
    BLOCK_SIZE  : integer := 8
  );
  port (
    -- Board clock & reset
    CLK100MHZ   : in  std_logic;
    CPU_RESETN  : in  std_logic;

    -- Switches and buttons
    SW          : in  std_logic_vector(15 downto 0);
    BTNC        : in  std_logic;  -- start
    BTNU        : in  std_logic;  -- pause
    BTNR        : in  std_logic;  -- step
    BTNL        : in std_logic;
    -- Leds and seven segment displays (common anode)
    LED         : out std_logic_vector(15 downto 0);
    -- rightmost
    CA, CB, CC, CD, CE, CF, CG, DP : out std_logic;
    AN : out std_logic_vector(7 downto 0);

    -- USB-UART (FTDI)
    UART_TXD_IN : in  std_logic;  -- PC->FPGA (RX for CPU)
    UART_RXD_OUT: out std_logic;  -- FPGA->PC (TX from CPU)

    -- VGA 4:4:4
    VGA_R       : out std_logic_vector(3 downto 0);
    VGA_G       : out std_logic_vector(3 downto 0);
    VGA_B       : out std_logic_vector(3 downto 0);
    VGA_HSync   : out std_logic;
    VGA_VSync   : out std_logic
  );
end entity;

architecture rtl of top_nexys4ddr is

  ------------------------------------------------------------------
  -- Clocks & reset
  ------------------------------------------------------------------
  signal clk_50m    : std_logic := '0';
  signal clk_pix    : std_logic := '0';
  signal clk_locked : std_logic;

  signal rstn_sys, rstn_pix : std_logic := '0';
  signal rst_sys, rst_pix   : std_logic := '1'; -- active-high
  signal clk_sys            : std_logic;

  ------------------------------------------------------------------
  -- Control Unit ↔ subsystems
  ------------------------------------------------------------------
  signal sw_mode       : std_logic;
  signal sw_single     : std_logic;
  signal sw_vga_sel    : std_logic_vector(1 downto 0);
  signal sw_tmpl_id    : std_logic_vector(5 downto 0);

  signal start_btn     : std_logic;
  signal pause_btn     : std_logic;
  signal step_btn      : std_logic;
  signal clear_st : std_logic;

  signal proc_ce       : std_logic;

  -- UART → BRAM signals
  signal uart_data  : std_logic_vector(7 downto 0);
  signal uart_done  : std_logic;
  signal uart_perr  : std_logic;
  signal uart_ferr  : std_logic;

  -- Frame control
  signal frame_start_sig : std_logic := '0';
  signal frame_ready     : std_logic;

  -- Binarizer
  signal bin_start_p   : std_logic;
  signal bin_busy      : std_logic;
  signal bin_done_p    : std_logic;

  -- Bridge BRAM write from binarizer (both @ 50 MHz)
  signal ts_a_en, ts_a_we : std_logic;
  signal ts_a_addr        : unsigned(ADDR_WIDTH-1 downto 0);
  signal ts_a_din         : std_logic_vector(7 downto 0);
  

-- registered edges + sticky for _done_p signals
---dones
signal bin_done_pe, thin_done_pe, min_done_pe : std_logic := '0';
signal bin_done_q,  thin_done_q,  min_done_q  : std_logic := '0';
signal bin_done_st, thin_done_st, min_done_st : std_logic := '0';
--cpu debugs for xbus
signal dbg_mem_addr  : std_logic_vector(31 downto 0);
signal dbg_mem_we    : std_logic;
signal dbg_mem_re    : std_logic;
signal dbg_mem_ready : std_logic;
signal dbg_mbx_score : std_logic_vector(31 downto 0);

  -- Thinning
  signal thin_start_p     : std_logic;
  signal thin_busy        : std_logic;
  signal thin_done_p      : std_logic;

  -- Thinning reads binary via bridge BRAM (50 MHz)
  signal thin_bin_rd_en    : std_logic;
  signal thin_bin_rd_addr  : unsigned(ADDR_WIDTH-1 downto 0);
  signal thin_bin_rd_dout  : std_logic_vector(7 downto 0);

    
  -- Minutiae
  signal min_start_p    : std_logic;
  signal min_busy       : std_logic;
  signal min_done_p     : std_logic;
  signal min_feat_count : unsigned(9 downto 0);
  signal min_overflow   : std_logic;
  
  signal overlay_en_i : std_logic;
  -- Overlay bitmap writer (clk_sys)
        signal ovl_we     : std_logic := '0';
        signal ovl_addr   : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
        signal ovl_din    : std_logic_vector(7 downto 0) := (others=>'0');
        
        -- simple clear FSM for overlay bitmap
        signal ovl_clr     : std_logic := '0';
        signal ovl_clr_cnt : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
        
        -- drive overlay enable from switches
        signal overlay_en_s : std_logic;
    ------------------------------------------------------------------
  -- Minutiae BRAM (512 x 32): Port-A=writer (extractor), Port-B=CPU
  ------------------------------------------------------------------
  signal feat_we       : std_logic;
  signal feat_addr_a   : unsigned(8 downto 0);
  signal feat_din_a    : std_logic_vector(31 downto 0);

  -- VGA overlay (not used now)
  signal min_list_en_vga   : std_logic := '0';
  signal min_list_addr_vga : std_logic_vector(8 downto 0) := (others => '0');
  signal min_list_dout_vga : std_logic_vector(31 downto 0) := (others => '0');
  

  -- CPU mailbox
  signal cu_cmd_wr      : std_logic;
  signal cu_cmd         : std_logic_vector(7 downto 0);
  signal cu_cmd_arg0_wr : std_logic;
  signal cu_cmd_arg0    : std_logic_vector(31 downto 0);
  signal cu_probe_len_wr: std_logic;
  signal cu_probe_len   : std_logic_vector(31 downto 0);
  signal cu_irq_set     : std_logic;

  signal cu_status       : std_logic_vector(31 downto 0);
  signal cu_result_slot  : std_logic_vector(31 downto 0);
  signal cu_result_score : std_logic_vector(31 downto 0);
  signal score_digits : std_logic_vector(15 downto 0);
  
  signal probe_addr_o  : std_logic_vector(8 downto 0);   -- 4 KiB / 4 = 1024 words
    signal probe_wdata_o : std_logic_vector(31 downto 0);
    signal probe_we_o    : std_logic_vector(3 downto 0);
    signal probe_rdata_i : std_logic_vector(31 downto 0);
    
    signal tmpl_addr_o   : std_logic_vector(12 downto 0);  -- 32 KiB / 4 = 8192 words
    signal tmpl_wdata_o  : std_logic_vector(31 downto 0);
    signal tmpl_we_o     : std_logic_vector(3 downto 0);
    signal tmpl_rdata_i  : std_logic_vector(31 downto 0);
    

  ------------------------------------------------------------------
  -- data_bram ↔ binarizer_subsystem
  ------------------------------------------------------------------
  signal db_rd_en      : std_logic;
  signal db_rd_addr    : unsigned(ADDR_WIDTH-1 downto 0);
  signal db_rd_dout    : std_logic_vector(DATA_WIDTH-1 downto 0);

  ------------------------------------------------------------------
  -- binarizer_subsystem VGA port (Binary only)
  ------------------------------------------------------------------
  signal bin_vga_en_vga   : std_logic;
  signal bin_vga_addr_vga : std_logic_vector(ADDR_WIDTH-1 downto 0);
  signal bin_vga_dout     : std_logic_vector(7 downto 0);

  ------------------------------------------------------------------
  -- THIN_A / THIN_B BRAMs and readers/writers
  ------------------------------------------------------------------
  signal thinA_we      : std_logic;
  signal thinA_addr_a  : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinA_din     : std_logic_vector(7 downto 0);

  signal thinB_we      : std_logic;
  signal thinB_addr_a  : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinB_din     : std_logic_vector(7 downto 0);

  -- Thinning (Port-B read)
  signal thinA_rd_en_T   : std_logic;
  signal thinA_rd_addr_T : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinB_rd_en_T   : std_logic;
  signal thinB_rd_addr_T : unsigned(ADDR_WIDTH-1 downto 0);

  -- Minutiae (Port-B read)
  signal thinA_rd_en_M   : std_logic;
  signal thinA_rd_addr_M : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinB_rd_en_M   : std_logic;
  signal thinB_rd_addr_M : unsigned(ADDR_WIDTH-1 downto 0);

  -- Shared Port-B muxed interface + data back to both blocks
  signal thinA_b_en    : std_logic;
  signal thinA_b_addr  : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinA_b_dout  : std_logic_vector(7 downto 0);

  signal thinB_b_en    : std_logic;
  signal thinB_b_addr  : unsigned(ADDR_WIDTH-1 downto 0);
  signal thinB_b_dout  : std_logic_vector(7 downto 0);

  ------------------------------------------------------------------
  -- VGA RGB (8-bit internal)
  ------------------------------------------------------------------
  signal vga_r8, vga_g8, vga_b8 : std_logic_vector(7 downto 0);
    -- Dual-clock VGA BRAMs for ThinA/ThinB display
  signal vgaA_we, vgaB_we : std_logic := '0';
  signal vgaA_addr, vgaB_addr : unsigned(ADDR_WIDTH-1 downto 0);
  signal vgaA_din, vgaB_din : std_logic_vector(7 downto 0);
  signal vga_thinA_en, vga_thinB_en : std_logic;
  signal vga_thinA_addr, vga_thinB_addr : std_logic_vector(ADDR_WIDTH-1 downto 0);
  signal vga_thinA_dout, vga_thinB_dout : std_logic_vector(7 downto 0);
  ------------------------------------------------------------------
  -- Copy FSM signals (to snapshot ThinA/B into VGA buffers)
  ------------------------------------------------------------------
  constant TOTAL_PIX : integer := IMG_WIDTH * IMG_HEIGHT;
  type cstate_t is (C_IDLE, C_COPY_A_RD, C_COPY_A_WR, C_COPY_B_RD, C_COPY_B_WR);
  signal cst         : cstate_t := C_IDLE;
  signal caddr       : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
  signal copy_active : std_logic := '0';
  signal wiz_cpu_reset: std_logic;
  -- additional copy fsm latches
signal caddr_rd : unsigned(ADDR_WIDTH-1 downto 0);
signal copy_done : std_logic := '0';
signal seen_A_nz, seen_B_nz : std_logic := '0';
signal wr_pulses : unsigned(7 downto 0) := (others=>'0');

  component clk_wiz_0
    port(
      clk_out1 : out std_logic;  -- 50 MHz
      clk_out2 : out std_logic;  -- 25 MHz
      reset    : in  std_logic;
      locked   : out std_logic;
      clk_in1  : in  std_logic
    );
  end component;

begin
  ------------------------------------------------------------------
  -- Clocks & reset
  ------------------------------------------------------------------
  wiz_cpu_reset <= not CPU_RESETN;

  u_clk_wiz : clk_wiz_0
    port map(
      clk_out1 => clk_50m,
      clk_out2 => clk_pix,
      reset    => wiz_cpu_reset,
      locked   => clk_locked,
      clk_in1  => CLK100MHZ
    );

  process(clk_50m)
  begin
    if rising_edge(clk_50m) then
      rstn_sys <= CPU_RESETN;
      rst_sys  <= not CPU_RESETN;
    end if;
  end process;

  process(clk_pix)
  begin
    if rising_edge(clk_pix) then
      rstn_pix <= CPU_RESETN;
      rst_pix  <= not rstn_pix;
    end if;
  end process;

  clk_sys <= clk_50m;

  ------------------------------------------------------------------
  -- Switches/buttons
  ------------------------------------------------------------------
  sw_mode    <= SW(0);  --0=test, 1(up)=enroll
  sw_single  <= SW(1);  
  sw_vga_sel <= SW(3 downto 2);
  sw_tmpl_id <= SW(9 downto 4);

  start_btn  <= BTNC;
  pause_btn  <= BTNU;
  step_btn   <= BTNR;
  clear_st <= BTNL;

  score_digits <= cu_result_score(15 downto 0); 
  ------------------------------------------------------------------
  -- UART RX
  ------------------------------------------------------------------
  u_uart_rx : entity work.uart_rx
    generic map (
      CLK_FREQ   => 50_000_000,
      BAUD_RATE  => 19200,
      OVERSAMPLE => 16
    )
    port map (
      clk        => clk_sys,
      reset      => rst_sys,
      rx         => UART_TXD_IN,
      data_out   => uart_data,
      done       => uart_done,
      parity_err => uart_perr,
      frame_err  => uart_ferr
    );

  ------------------------------------------------------------------
  -- DATA BRAM (probe image)
  ------------------------------------------------------------------
  data_bram_inst : entity work.data_bram
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT
    )
    port map (
      clk           => clk_sys,
      rst           => rst_sys,
      data_reg      => uart_data,
      done_reg      => uart_done,
      parity_err_reg=> uart_perr,
      frame_err_reg => uart_ferr,
      frame_start   => frame_start_sig,
      frame_done    => frame_ready,
      rd_clk        => clk_sys,
      rd_en         => db_rd_en,
      rd_addr       => db_rd_addr,
      rd_dout       => db_rd_dout
    );

  ------------------------------------------------------------------
  -- BINARIZER
  ------------------------------------------------------------------
  u_bin : entity work.binarizer_subsystem
    generic map (
      DATA_WIDTH    => DATA_WIDTH,
      ADDR_WIDTH    => ADDR_WIDTH,
      IMG_WIDTH     => IMG_WIDTH,
      IMG_HEIGHT    => IMG_HEIGHT,
      BLOCK_SIZE    => BLOCK_SIZE,
      THRESH_OFFSET => 12
    )
    port map (
      rd_clk        => clk_sys,
      rst           => rst_sys,
      start         => bin_start_p,
      -- read from DATA BRAM
      db_rd_en      => db_rd_en,
      db_rd_addr    => db_rd_addr,
      db_rd_dout    => db_rd_dout,
      -- VGA view of Binary (dual-clock inside binarizer)
      bin_vga_clk   => clk_pix,
      bin_vga_en    => bin_vga_en_vga,
      bin_vga_addr  => unsigned(bin_vga_addr_vga),
      bin_vga_dout  => bin_vga_dout,
      -- mirror writes to a bridge RAM for thinning
      ts_a_en       => ts_a_en,
      ts_a_we       => ts_a_we,
      ts_a_addr     => ts_a_addr,
      ts_a_din      => ts_a_din,
      busy          => bin_busy,
      done_pulse    => bin_done_p
    );

  ------------------------------------------------------------------
  -- Binary → Thin bridge RAM (Byte, dual-port single clock)
  --   Port-A: written by binarizer (ts_*)
  --   Port-B: read by thinning as its "binary source"
  ------------------------------------------------------------------
  bin_bridge_bram : entity work.dp_bram_byte
    generic map ( ADDR_WIDTH => ADDR_WIDTH )
    port map (
      -- Port A (writer from binarizer)
      a_clk  => clk_sys,
      a_en   => ts_a_en,
      a_we   => ts_a_we,
      a_addr => ts_a_addr,
      a_din  => ts_a_din,
      a_dout => open,
      -- Port B (reader to thinning)
      b_clk  => clk_sys,
      b_en   => thin_bin_rd_en,
      b_we   => '0',
      b_addr => thin_bin_rd_addr,
      b_din  => (others => '0'),
      b_dout => thin_bin_rd_dout
    );




  thinA_bram : entity work.dp_bram_byte
    generic map ( ADDR_WIDTH => ADDR_WIDTH )
    port map (
      -- A: write by thinning
      a_clk  => clk_sys,
      a_en   => '1',
      a_we   => thinA_we,
      a_addr => thinA_addr_a,
      a_din  => thinA_din,
      a_dout => open,
      -- B: read by thinning or minutiae (muxed)
      b_clk  => clk_sys,
      b_en   => thinA_b_en,
      b_we   => '0',
      b_addr => thinA_b_addr,
      b_din  => (others => '0'),
      b_dout => thinA_b_dout
    );

  thinB_bram : entity work.dp_bram_byte
    generic map ( ADDR_WIDTH => ADDR_WIDTH )
    port map (
      -- A: write by thinning
      a_clk  => clk_sys,
      a_en   => '1',
      a_we   => thinB_we,
      a_addr => thinB_addr_a,
      a_din  => thinB_din,
      a_dout => open,
      -- B: read by thinning or minutiae (muxed)
      b_clk  => clk_sys,
      b_en   => thinB_b_en,
      b_we   => '0',
      b_addr => thinB_b_addr,
      b_din  => (others => '0'),
      b_dout => thinB_b_dout
    );

  ------------------------------------------------------------------
  -- THINNING SUBSYSTEM
  ------------------------------------------------------------------
  u_thin : entity work.thinning_subsystem
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT,
      MAX_ITERS  => 8
    )
    port map (
      clk           => clk_sys,
      rst           => rst_sys,
      -- CU handshake
      s_thin_start  => thin_start_p,
      s_thin_busy   => thin_busy,
      s_thin_done   => thin_done_p,
      -- Binary source (bridge RAM)
      bin_rd_en     => thin_bin_rd_en,
      bin_rd_addr   => thin_bin_rd_addr,
      bin_rd_dout   => thin_bin_rd_dout,
      -- ThinA ports
      thinA_rd_en   => thinA_rd_en_T,
      thinA_rd_addr => thinA_rd_addr_T,
      thinA_rd_dout => thinA_b_dout,
      thinA_we      => thinA_we,
      thinA_addr    => thinA_addr_a,
      thinA_din     => thinA_din,
      -- ThinB ports
      thinB_rd_en   => thinB_rd_en_T,
      thinB_rd_addr => thinB_rd_addr_T,
      thinB_rd_dout => thinB_b_dout,
      thinB_we      => thinB_we,
      thinB_addr    => thinB_addr_a,
      thinB_din     => thinB_din
    );

  ------------------------------------------------------------------
  -- MINUTIAE EXTRACTOR
  --   Reads final skeleton from ThinB (ping-pong ends there)
  --   Writes features to an external BRAM (omitted here); count/overflow exposed.
  ------------------------------------------------------------------
  u_min : entity work.minutiae_extractor
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH,
      IMG_WIDTH  => IMG_WIDTH,
      IMG_HEIGHT => IMG_HEIGHT,
      FEAT_DEPTH => 512
    )
    port map (
      clk           => clk_sys,
      rst           => rst_sys,
      -- CU handshake
      s_min_start   => min_start_p,
      s_min_busy    => min_busy,
      s_min_done    => min_done_p,
      -- choose final buffer: ThinB assumed final
      src_is_a      => '0',
      -- ThinA/B read (Port-B shared)
      thinA_rd_en   => thinA_rd_en_M,
      thinA_rd_addr => thinA_rd_addr_M,
      thinA_rd_dout => thinA_b_dout,
      thinB_rd_en   => thinB_rd_en_M,
      thinB_rd_addr => thinB_rd_addr_M,
      thinB_rd_dout => thinB_b_dout,
      -- Feature BRAM Port-A
    feat_we    => feat_we,
    feat_addr  => feat_addr_a,
    feat_din   => feat_din_a,
      -- Status
      feat_count    => min_feat_count,
      feat_overflow => min_overflow
    );

  ------------------------------------------------------------------
  -- Copy FSM to snapshot ThinA/B → VGA buffers
  ------------------------------------------------------------------
  process(clk_sys)
     variable v_dout : std_logic_vector(7 downto 0);
  begin
    if rising_edge(clk_sys) then
      if rst_sys = '1' then
        cst <= C_IDLE;
        caddr <= (others=>'0');
        caddr_rd <= (others=>'0');
        vgaA_we <= '0';
        vgaB_we <= '0';
        copy_active <= '0';
        copy_done <= '0';
      else
        vgaA_we <= '0';
        vgaB_we <= '0';
        -- one-shot poke on BTN Right to prove A-port writes reach the display BRAMs
--if BTNL = '1' then
  --vgaA_we   <= '1';  vgaA_addr <= (others => '0'); vgaA_din <= x"FF";
  --vgaB_we   <= '1';  vgaB_addr <= (others => '0'); vgaB_din <= x"80";
--end if;
        
        case cst is
          when C_IDLE =>
          if min_done_pe='1' then     -- your registered edge
            copy_active <= '1';
            copy_done   <= '0';   
            caddr    <= (others=>'0');
            cst      <= C_COPY_A_RD;
            seen_A_nz <= '0';
            seen_B_nz <= '0';
          end if;
            
          -- ==== THIN A ====
        when C_COPY_A_RD =>
          caddr_rd <= caddr;           -- present address to BRAM (via mux)
          cst      <= C_COPY_A_WR;

        when C_COPY_A_WR =>
        vgaA_we   <= '1';
          v_dout :=  thinA_b_dout;   -- data from previous cycle
          if thinA_b_dout /= x"00" then seen_A_nz <= '1'; end if;
          vgaA_addr <= caddr_rd;
          vgaA_din  <= v_dout;
          --vgaA_din <= std_logic_vector(resize(caddr_rd(7 downto 0), 8)); -- visible gradient forced
          if caddr = to_unsigned(TOTAL_PIX-1, caddr'length) then
            caddr <= (others=>'0');
            cst   <= C_COPY_B_RD;
          else
            caddr <= caddr + 1;
            cst   <= C_COPY_A_RD;
          end if;
               -- ==== THIN B ====
        when C_COPY_B_RD =>
          caddr_rd <= caddr;
          cst      <= C_COPY_B_WR;

        when C_COPY_B_WR =>
            vgaB_we <= '1';
          v_dout := thinB_b_dout;
          if thinB_b_dout /= x"00" then seen_B_nz <= '1'; end if;
          vgaB_addr <= caddr_rd;
          vgaB_din  <= v_dout ;
          if caddr = to_unsigned(TOTAL_PIX-1, caddr'length) then
            copy_active <= '0';
            copy_done <= '1';
            cst         <= C_IDLE;
          else
            caddr <= caddr + 1;
            cst   <= C_COPY_B_RD;
          end if;
          when others =>
          cst <= C_IDLE;
      end case;
      end if;
      end if;
  end process;

  ------------------------------------------------------------------
  -- Dual-clock VGA BRAMs
  ------------------------------------------------------------------
  thinA_vga_bram : entity work.dp_bram_byte
    generic map (ADDR_WIDTH => ADDR_WIDTH)
    port map (a_clk => clk_sys, a_en => '1', a_we => vgaA_we, a_addr => vgaA_addr, a_din => vgaA_din,
              a_dout => open, b_clk => clk_pix, b_en => vga_thinA_en, b_we => '0',
              b_addr => unsigned(vga_thinA_addr), b_din => (others=>'0'), b_dout => vga_thinA_dout);

  thinB_vga_bram : entity work.dp_bram_byte
    generic map (ADDR_WIDTH => ADDR_WIDTH)
    port map (a_clk => clk_sys, a_en => '1', a_we => vgaB_we, a_addr => vgaB_addr, a_din => vgaB_din,
              a_dout => open, b_clk => clk_pix, b_en => vga_thinB_en, b_we => '0',
              b_addr => unsigned(vga_thinB_addr), b_din => (others=>'0'), b_dout => vga_thinB_dout);

  ------------------------------------------------------------------
  -- Updated Port-B mux with copy_active
  ------------------------------------------------------------------
thinA_b_en   <= '1' when copy_active='1' else
                thinA_rd_en_T when thin_busy='1' else
                thinA_rd_en_M when min_busy='1' else '0';
thinA_b_addr <= caddr_rd when copy_active='1' else
                thinA_rd_addr_T when thin_busy='1' else
                thinA_rd_addr_M when min_busy='1' else (others=>'0');

thinB_b_en   <= '1' when copy_active='1' else
                thinB_rd_en_T when thin_busy='1' else
                thinB_rd_en_M when min_busy='1' else '0';
thinB_b_addr <= caddr_rd when copy_active='1' else
                thinB_rd_addr_T when thin_busy='1' else
                thinB_rd_addr_M when min_busy='1' else (others=>'0');
  ------------------------------------------------------------------
  -- VGA SUBSYSTEM (Binary only; ThinA/ThinB disconnected to avoid CDC)
  ------------------------------------------------------------------
 process(sw_vga_sel) begin
             if sw_vga_sel = "11" 
                then overlay_en_i <= '1';
                        overlay_en_s <= '1';
              else 
                overlay_en_i <= '0'; 
                overlay_en_s <= '1';
              end if;
        end process;
 
        
  u_vga : entity work.vga_subsystem
    generic map (
      H_RES  => 640, V_RES => 480,
      IMG_W  => IMG_WIDTH, IMG_H => IMG_HEIGHT,
      SCALE  => 4, WIN_X0 => 184, WIN_Y0 => 4
    )
    port map (
      clk_pix       => clk_pix,
      rst_n         => CPU_RESETN,

      sw_vga_sel    => sw_vga_sel,
      overlay_en => overlay_en_i,
      min_done_p    => min_done_p,

      -- Binary BRAM (Port-B, read-only for VGA)
      bin_dout      => bin_vga_dout,
      bin_addr      => bin_vga_addr_vga,
      bin_en        => bin_vga_en_vga,

      -- ThinA/ThinB 
      thinA_dout => vga_thinA_dout, 
      thinA_addr => vga_thinA_addr, 
      thinA_en => vga_thinA_en,
      
      thinB_dout => vga_thinB_dout, 
      thinB_addr => vga_thinB_addr, 
      thinB_en => vga_thinB_en,

      -- Minutiae overlay (unused)
      min_list_dout => min_list_dout_vga,
      min_list_addr => min_list_addr_vga,
      min_list_en   => min_list_en_vga,

      vga_hsync     => VGA_HSync,
      vga_vsync     => VGA_VSync,
      vga_r         => vga_r8,
      vga_g         => vga_g8,
      vga_b         => vga_b8
    );

  -- 8-bit VGA down-mapped to 4-bit outputs
  VGA_R <= vga_r8(7 downto 4);
  VGA_G <= vga_g8(7 downto 4);
  VGA_B <= vga_b8(7 downto 4);
  
  ------------------------------------------------------------------
  -- MINUTIAE BRAM (512 x 32): A=writer (minutiae), B=CPU
  ------------------------------------------------------------------
  -- duplicate one for cpu and one for vga (both get the same Port-A)
min_bram_vga : entity work.minutiae_bram
  generic map (
    DATA_WIDTH=>32, 
    ADDR_WIDTH=>9)
  port map (
    clkA => clk_sys, 
    weA  => feat_we, 
    addrA=> feat_addr_a, 
    dinA => feat_din_a, 
    doutA=> open,
    
    clkB => clk_pix, 
    enB  => min_list_en_vga,
    addrB=> unsigned(min_list_addr_vga), 
    doutB=> min_list_dout_vga);

min_bram_cpu : entity work.minutiae_bram
  generic map (
    DATA_WIDTH=>32, 
        ADDR_WIDTH=>9)
  port map (
    clkA  => clk_sys, 
    weA   => feat_we, 
    addrA => feat_addr_a, 
    dinA  => feat_din_a, 
    doutA => open,
    clkB  => clk_sys, 
    enB   => '1',
    addrB => unsigned(probe_addr_o(8 downto 0)), 
    doutB => probe_rdata_i);



  ------------------------------------------------------------------
  -- TEMPLATE BRAM (CPU Port-B)
  ------------------------------------------------------------------
  tmpl_bram : entity work.template_bram
    generic map (
      ADDR_WIDTH => 13
    )
    port map (
      -- Port A unused (reserved for HW copy/commit)
      a_clk   => clk_sys,
      a_en    => '0',
      a_we    => (others => '0'),
      a_addr  => (others => '0'),
      a_din   => (others => '0'),

      -- Port B (CPU R/W with byte enables)
      b_clk   => clk_sys,
      b_en    => '1',
      b_we    => tmpl_we_o,                  -- 4-bit byte enables
      b_addr  => tmpl_addr_o,
      b_din   => tmpl_wdata_o,
      b_dout  => tmpl_rdata_i
    );


  
  ------------------------------------------------------------------
  -- CPU SUBSYSTEM
  ------------------------------------------------------------------
  u_cpu : entity work.cpu_subsystem
    generic map (
      PROBE_BASE_G        => x"20000000",
      PROBE_BYTES_G       => 512*4,       -- 2 KB probe window
      TMPL_BASE_G         => x"30000000",
      TMPL_BYTES_G        => 32768,    
      MBX_BASE_G          => x"40000000",
      MBX_BYTES_G         => 256,
      PROBE_ADDR_WIDTH_G  => 9,
      TMPL_ADDR_WIDTH_G   => 13
    )
    port map (
      clk    => clk_sys,
      rst_n  => CPU_RESETN,

      -- CU→CPU mailbox writes / IRQ
      cu_cmd_wr       => cu_cmd_wr,
      cu_cmd          => cu_cmd,
      cu_cmd_arg0_wr  => cu_cmd_arg0_wr,
      cu_cmd_arg0     => cu_cmd_arg0,
      cu_probe_len_wr => cu_probe_len_wr,
      cu_probe_len    => cu_probe_len,
      cu_irq_set      => cu_irq_set,
      --debugs
      dbg_mem_addr  => dbg_mem_addr,
    dbg_mem_we    => dbg_mem_we,
    dbg_mem_re    => dbg_mem_re,
    dbg_mem_ready => dbg_mem_ready,
    dbg_mbx_score => dbg_mbx_score,

      -- CPU→CU readbacks
      cu_status       => cu_status,
      cu_result_slot  => cu_result_slot,
      cu_result_score => cu_result_score,

      -- BRAM-B (CPU ports)
      probe_addr_o    => probe_addr_o,
      probe_wdata_o   => probe_wdata_o,           -- not used here
      probe_we_o      => probe_we_o,
      probe_rdata_i   => probe_rdata_i,

      tmpl_addr_o     => tmpl_addr_o,
      tmpl_wdata_o    => tmpl_wdata_o,
      tmpl_we_o       => tmpl_we_o,
      tmpl_rdata_i    => tmpl_rdata_i,

      -- UART to FTDI
      uart_txd_o      => UART_RXD_OUT,
      uart_rxd_i      => UART_TXD_IN
      
      
    );


  ------------------------------------------------------------------
  -- CONTROL UNIT (step-gated, stage-by-stage)
  ------------------------------------------------------------------
  u_cu : entity work.control_unit
    generic map ( IMG_WIDTH => IMG_WIDTH )
    port map (
      clk             => clk_sys,
      rst_n           => CPU_RESETN,
      sw_mode         => sw_mode,
      sw_vga_sel      => sw_vga_sel,
      sw_tmpl_id      => sw_tmpl_id,
      sw_single       => sw_single,
      btn_start       => start_btn,
      btn_pause       => pause_btn,
      btn_step        => step_btn,
      -- data BRAM handshake
      frame_ready     => frame_ready,
      frame_start     => frame_start_sig,
      -- pipeline handshakes
      bin_busy        => bin_busy,
      bin_done_p      => bin_done_p,
      thin_busy       => thin_busy,
      thin_done_p     => thin_done_p,
      min_busy        => min_busy,
      min_done_p      => min_done_p,
      -- counts to CPU/LEDs
      min_feat_count  => min_feat_count,
      min_overflow    => min_overflow,
      -- gated CE (not used internally here, kept for future)
      proc_ce         => proc_ce,
      -- start pulses to subsystems
      bin_start_p     => bin_start_p,
      thin_start_p    => thin_start_p,
      min_start_p     => min_start_p,
      -- CPU mailbox (present but not used by a CPU here)
      cu_cmd_wr       => cu_cmd_wr,
      cu_cmd          => cu_cmd,
      cu_cmd_arg0_wr  => cu_cmd_arg0_wr,
      cu_cmd_arg0     => cu_cmd_arg0,
      cu_probe_len_wr => cu_probe_len_wr,
      cu_probe_len    => cu_probe_len,
      cu_irq_set      => cu_irq_set,
      cu_status       => cu_status
      
    );
   
  process(clk_sys)
begin
  if rising_edge(clk_sys) then
    if rst_sys='1' or min_done_pe='1' then
      wr_pulses <= (others=>'0');
    elsif (vgaA_we='1') or (vgaB_we='1') then
      wr_pulses <= wr_pulses + 1;
    end if;
  end if;
end process;

   process(clk_sys)
begin
  if rising_edge(clk_sys) then
    if rst_sys='1' then
      bin_done_q  <= '0'; thin_done_q <= '0'; min_done_q <= '0';
      bin_done_pe <= '0'; thin_done_pe<= '0'; min_done_pe<= '0';
      bin_done_st <= '0'; thin_done_st<= '0'; min_done_st<= '0';

    elsif clear_st='1' then
      bin_done_st <= '0'; thin_done_st <= '0'; min_done_st <= '0';

    else
      -- edge detect
      bin_done_pe <= bin_done_p and (not bin_done_q);
      thin_done_pe<= thin_done_p and (not thin_done_q);
      min_done_pe <= min_done_p and (not min_done_q);
      bin_done_q  <= bin_done_p;
      thin_done_q <= thin_done_p;
      min_done_q  <= min_done_p;
      
    
        --sticky flags: clear on start, set on rising edge
      if bin_start_p='1' 
        then bin_done_st<='0';
      elsif bin_done_pe='1' 
        then bin_done_st<='1'; 
      end if;

      if thin_start_p='1' 
        then thin_done_st<='0';
      elsif thin_done_pe='1' 
        then thin_done_st<='1'; 
      end if;

      if min_start_p='1' 
        then min_done_st<='0';
      elsif min_done_pe='1' 
        then min_done_st<='1'; 
      end if;
    end if; 
  end if;
end process;
   
   u_seg_right : entity work.seven_segment_driver
  generic map (CNT_WIDTH => 18)
  port map (
    clk        => clk_sys,
    rst        => rst_sys,
    score_bin  => unsigned(score_digits(15 downto 0)),
    --score_bin => unsigned(SW(15 downto 0)),
    --score_bin => unsigned(dbg_mbx_score(15 downto 0)),
    CA     => CA,
      CB     => CB,
      CC     => CC,
      CD     => CD,
      CE     => CE,
      CF     => CF,
      CG     => CG,
      DP     => DP,
      AN     => AN(3 downto 0)
      -- for left segment, instantiate another with 7 downto 4
  );
  ------------------------------------------------------------------
  -- LEDs
  ------------------------------------------------------------------
  LED(0) <= cu_status(0);  -- DONE
  LED(1) <= cu_status(1);  -- KICK/RUN
  LED(2) <= cu_status(2);  -- HEARTBEAT (CPU toggles)
  LED(3) <= not (bin_busy or thin_busy or min_busy); -- idle
 -- LED(4) <= start_btn;
  
  LED(5) <= '0';
  LED(6) <= overlay_en_i;
  LED(7) <= sw_mode;
  LED(8)  <= '0';
  LED(9)  <= thinB_we; 
  LED(10) <= thinA_we;
  
  LED(11) <= not (bin_busy or thin_busy or min_busy);
  LED(12) <= min_done_st;
  LED(13) <= thin_done_st;
  LED(14) <= bin_done_st;
  LED(15) <= frame_ready;



  -- Unused UART TX back to PC in this design
  --UART_RXD_OUT <= '1';

end architecture;
