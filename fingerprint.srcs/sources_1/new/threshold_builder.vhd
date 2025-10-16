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
use work.utils_pkg.all;

entity threshold_builder is
  generic (
    DATA_WIDTH    : integer := 8;
    ADDR_WIDTH    : integer := 13;
    IMG_WIDTH     : integer := 68;
    IMG_HEIGHT    : integer := 118;
    BLOCK_SIZE    : integer := 8;
    THRESH_OFFSET : integer := 12
  );
  port (
    clk         : in  std_logic;
    rst         : in  std_logic;
    start       : in  std_logic;

    -- data_bram Port-B (reader)
    rd_en       : out std_logic;
    rd_addr     : out unsigned(ADDR_WIDTH-1 downto 0);
    rd_dout     : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- THR_BRAM write
    thr_wr_en   : out std_logic;
    thr_wr_addr : out unsigned;
    thr_wr_data : out std_logic_vector(7 downto 0);

    busy        : out std_logic;
    done_pulse  : out std_logic
  );
end entity;

architecture rtl of threshold_builder is
-- local helpers
  function i_clog2(n : integer) return integer is
    variable v : integer := n - 1;
    variable r : integer := 0;
  begin
    while v > 0 loop v := v / 2; r := r + 1; end loop;
    return r;
  end;

  function i_ceil_div(a,b: integer) return integer is
  begin
    return (a + b - 1) / b;
  end;

  constant NBX          : integer := ceil_div(IMG_WIDTH,  BLOCK_SIZE); -- 9
  constant NBY          : integer := ceil_div(IMG_HEIGHT, BLOCK_SIZE); -- 15
  constant NBLOCKS      : integer := NBX * NBY;                         -- 135
  constant TOTAL_PIXELS : integer := IMG_WIDTH * IMG_HEIGHT;
  constant THR_AW       : integer := clog2(NBLOCKS);                    -- 8

  --subtype u7  is unsigned(6 downto 0);
  --subtype u16 is unsigned(15 downto 0);
  --type u7_array  is array (0 to NBLOCKS-1) of u7;
  --type u16_array is array (0 to NBLOCKS-1) of u16;
    type state_t is (
    IDLE, CLEAR_ACC, 
    PASS1_READ, PASS1_WRITE,   -- 2-cycle RMW to BRAM
    MAKE_THR_READ, MAKE_THR_MUL, MAKE_THR_WRITE,
    DONE
  );
   signal state : state_t := IDLE;
  --data BRAm stream pipeline
    signal rd_en_s, rd_en_q, data_valid : std_logic := '0';
  signal rd_addr_s : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');

  signal addr_issued : integer range 0 to TOTAL_PIXELS := 0;
  signal row_cnt     : integer range 0 to IMG_HEIGHT-1 := 0;
  signal col_cnt     : integer range 0 to IMG_WIDTH-1  := 0;
  -- block index and addressing
    signal bidx      : unsigned(THR_AW-1 downto 0) := (others => '0');
  signal idx_thr   : unsigned(THR_AW-1 downto 0) := (others => '0');
    -- SUM BRAM (16-bit) via dp_ram_simple
  --   Port A: RMW during PASS1  (addr=bidx)
  --   Port B: READ during MAKE_THR (addr=idx_thr)
   signal sum_a_en   : std_logic := '0';
  signal sum_a_we   : std_logic := '0';
  signal sum_a_addr : std_logic_vector(THR_AW-1 downto 0) := (others => '0');
  signal sum_a_din  : std_logic_vector(15 downto 0) := (others => '0');
  signal sum_a_dout : std_logic_vector(15 downto 0);

  signal sum_b_en   : std_logic := '0';
  signal sum_b_addr : std_logic_vector(THR_AW-1 downto 0) := (others => '0');
  signal sum_b_dout : std_logic_vector(15 downto 0);
  
    -- count bram (8-bit) via dp_ram_simple
  signal cnt_a_en   : std_logic := '0';
  signal cnt_a_we   : std_logic := '0';
  signal cnt_a_addr : std_logic_vector(THR_AW-1 downto 0) := (others => '0');
  signal cnt_a_din  : std_logic_vector(7 downto 0) := (others => '0');
  signal cnt_a_dout : std_logic_vector(7 downto 0);

  signal cnt_b_en   : std_logic := '0';
  signal cnt_b_addr : std_logic_vector(THR_AW-1 downto 0) := (others => '0');
  signal cnt_b_dout : std_logic_vector(7 downto 0);
  
  --make threshold pipeline rags
    signal sum_q      : unsigned(15 downto 0) := (others => '0');
  signal cnt_q      : unsigned(7 downto 0)  := (others => '0'); -- we use 7 LSBs
  signal prod_q     : unsigned(24 downto 0) := (others => '0'); -- 16+8-1 bits safe
  signal mean_q     : unsigned(15 downto 0) := (others => '0');
  
    -- Reciprocal table (Q8): inv_cnt[k] ≈ round(256/k), inv_cnt[0] unused
  type u8_arr is array (0 to 255) of unsigned(7 downto 0);
  constant inv_cnt : u8_arr := (
    0   => (others=>'0'),
    1   => to_unsigned(256,8),  2 => to_unsigned(128,8),  3 => to_unsigned(85,8),
    4   => to_unsigned(64,8),   5 => to_unsigned(51,8),   6 => to_unsigned(43,8),
    7   => to_unsigned(37,8),   8 => to_unsigned(32,8),   9 => to_unsigned(28,8),
    10  => to_unsigned(26,8),  11 => to_unsigned(23,8),  12 => to_unsigned(21,8),
    13  => to_unsigned(20,8),  14 => to_unsigned(18,8),  15 => to_unsigned(17,8),
    16  => to_unsigned(16,8),  17 => to_unsigned(15,8),  18 => to_unsigned(14,8),
    19  => to_unsigned(13,8),  20 => to_unsigned(13,8),  21 => to_unsigned(12,8),
    22  => to_unsigned(12,8),  23 => to_unsigned(11,8),  24 => to_unsigned(11,8),
    25  => to_unsigned(10,8),  26 => to_unsigned(10,8),  27 => to_unsigned(9,8),
    28  => to_unsigned(9,8),   29 => to_unsigned(9,8),   30 => to_unsigned(9,8),
    31  => to_unsigned(8,8),   32 => to_unsigned(8,8),   33 => to_unsigned(8,8),
    34  => to_unsigned(8,8),   35 => to_unsigned(7,8),   36 => to_unsigned(7,8),
    37  => to_unsigned(7,8),   38 => to_unsigned(7,8),   39 => to_unsigned(7,8),
    40  => to_unsigned(6,8),   41 => to_unsigned(6,8),   42 => to_unsigned(6,8),
    43  => to_unsigned(6,8),   44 => to_unsigned(6,8),   45 => to_unsigned(6,8),
    46  => to_unsigned(6,8),   47 => to_unsigned(5,8),   48 => to_unsigned(5,8),
    49  => to_unsigned(5,8),   50 => to_unsigned(5,8),   51 => to_unsigned(5,8),
    52  => to_unsigned(5,8),   53 => to_unsigned(5,8),   54 => to_unsigned(5,8),
    55  => to_unsigned(5,8),   56 => to_unsigned(5,8),   57 => to_unsigned(4,8),
    58  => to_unsigned(4,8),   59 => to_unsigned(4,8),   60 => to_unsigned(4,8),
    61  => to_unsigned(4,8),   62 => to_unsigned(4,8),   63 => to_unsigned(4,8),
    64  => to_unsigned(4,8),
    others => (others=>'0')
  );
  --signal sum   : u16_array;
  --signal count : u7_array;

  -- outputs
  signal busy_r, done_r, thr_we_r : std_logic := '0';
  signal thr_addr_r : unsigned(THR_AW-1 downto 0) := (others => '0');
  signal thr_data_r : std_logic_vector(7 downto 0) := (others => '0');

begin
  rd_en   <= rd_en_s;
  rd_addr <= rd_addr_s;

  thr_wr_en   <= thr_we_r;
  thr_wr_addr <= resize(thr_addr_r, thr_wr_addr'length); -- allow unconstrained port
  thr_wr_data <= thr_data_r;

  busy       <= busy_r;
  done_pulse <= done_r;

  -- model 1-cycle read latency
  process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        rd_en_q    <= '0';
        data_valid <= '0';
      else
        rd_en_q    <= rd_en_s;
        data_valid <= rd_en_q;
      end if;
    end if;
  end process;

  main_fsm : process(clk)
    variable pix_u8    : unsigned(7 downto 0);
    variable bx, by    : integer;
    variable bidx      : integer;
    variable mean_i    : integer;
    variable thr_i     : integer;
  begin
  -- external connections
  rd_en       <= rd_en_s;
  rd_addr     <= rd_addr_s;
  thr_wr_en   <= thr_we_r;
  thr_wr_addr <= resize(thr_addr_r, thr_wr_addr'length);
  thr_wr_data <= thr_data_r;
  busy        <= busy_r;
  done_pulse  <= done_r;
  end process;
  -- data BRAM read latency model (1 cycle)
  process(clk)
  begin
    if rising_edge(clk) then
      if rst='1' then
        rd_en_q    <= '0';
        data_valid <= '0';
      else
        rd_en_q    <= rd_en_s;
        data_valid <= rd_en_q;
      end if;
    end if;
  end process;

  -- =========================
  -- SUM BRAM (16-bit)
  -- =========================
  u_sum : entity work.dp_ram_simple
    generic map ( ADDR_W => THR_AW, DATA_W => 16 )
    port map (
      clk    => clk,
      a_en   => sum_a_en,
      a_we   => sum_a_we,
      a_addr => sum_a_addr,
      a_din  => sum_a_din,
      a_dout => sum_a_dout,
      b_en   => sum_b_en,
      b_addr => sum_b_addr,
      b_dout => sum_b_dout
    );

  -- =========================
  -- COUNT BRAM (8-bit)
  -- =========================
  u_cnt : entity work.dp_ram_simple
    generic map ( ADDR_W => THR_AW, DATA_W => 8 )
    port map (
      clk    => clk,
      a_en   => cnt_a_en,
      a_we   => cnt_a_we,
      a_addr => cnt_a_addr,
      a_din  => cnt_a_din,
      a_dout => cnt_a_dout,
      b_en   => cnt_b_en,
      b_addr => cnt_b_addr,
      b_dout => cnt_b_dout
    );

  -- =========================
  -- Main FSM
  -- =========================
  process(clk)
    variable pix_u8  : unsigned(7 downto 0);
    variable bx, by  : integer;
    variable thr_i   : integer;
  begin
    if rising_edge(clk) then
      if rst='1' then
        state       <= IDLE;
        busy_r      <= '0';  done_r <= '0';
        rd_en_s     <= '0';  rd_addr_s <= (others => '0');
        addr_issued <= 0;    row_cnt <= 0; col_cnt <= 0;
        idx_thr     <= (others => '0'); bidx <= (others => '0');

        -- clear BRAM control
        sum_a_en <= '0'; sum_a_we <= '0'; sum_a_addr <= (others=>'0'); sum_a_din <= (others=>'0');
        sum_b_en <= '0'; sum_b_addr <= (others=>'0');
        cnt_a_en <= '0'; cnt_a_we <= '0'; cnt_a_addr <= (others=>'0'); cnt_a_din <= (others=>'0');
        cnt_b_en <= '0'; cnt_b_addr <= (others=>'0');

        thr_we_r <= '0'; thr_addr_r <= (others=>'0'); thr_data_r <= (others=>'0');
        sum_q <= (others=>'0'); cnt_q <= (others=>'0'); prod_q <= (others=>'0'); mean_q <= (others=>'0');

      else
        done_r  <= '0';
        thr_we_r <= '0';

        -- defaults each cycle
        sum_a_en <= '0'; sum_a_we <= '0';
        cnt_a_en <= '0'; cnt_a_we <= '0';
        sum_b_en <= '0'; cnt_b_en <= '0';

        case state is

          when IDLE =>
            busy_r      <= '0';
            rd_en_s     <= '0';
            addr_issued <= 0;
            row_cnt     <= 0; col_cnt <= 0;
            idx_thr     <= (others => '0');
            if start='1' then
              busy_r  <= '1';
              -- CLEAR_ACC: zero all blocks
              state   <= CLEAR_ACC;
            end if;

          when CLEAR_ACC =>
            if to_integer(idx_thr) < NBLOCKS then
              -- write zeros to both BRAMs at address = idx_thr
              sum_a_en   <= '1';
              sum_a_we   <= '1';
              sum_a_addr <= std_logic_vector(idx_thr);
              sum_a_din  <= (others => '0');

              cnt_a_en   <= '1';
              cnt_a_we   <= '1';
              cnt_a_addr <= std_logic_vector(idx_thr);
              cnt_a_din  <= (others => '0');

              idx_thr <= idx_thr + 1;
            else
              -- begin PASS1 reads
              rd_en_s     <= '1';
              rd_addr_s   <= (others => '0');
              addr_issued <= 1;
              row_cnt     <= 0;
              col_cnt     <= 0;
              state       <= PASS1_READ;
            end if;

          when PASS1_READ =>
            -- issue next pixel address if any
            if addr_issued < TOTAL_PIXELS then
              rd_en_s   <= '1';
              rd_addr_s <= to_unsigned(addr_issued, ADDR_WIDTH);
              addr_issued <= addr_issued + 1;
            else
              rd_en_s <= '0';
            end if;

            -- when data_valid, capture current block index and read SUM/COUNT at Port A
            if data_valid='1' then
              pix_u8 := unsigned(rd_dout);

              bx := col_cnt / BLOCK_SIZE;
              by := row_cnt / BLOCK_SIZE;
              bidx <= to_unsigned(by * NBX + bx, THR_AW);

              -- request READ of SUM/COUNT at bidx (A port), data available next cycle
              sum_a_en   <= '1';
              sum_a_we   <= '0';
              sum_a_addr <= std_logic_vector(to_unsigned(by * NBX + bx, THR_AW));

              cnt_a_en   <= '1';
              cnt_a_we   <= '0';
              cnt_a_addr <= std_logic_vector(to_unsigned(by * NBX + bx, THR_AW));

              -- advance pixel counters
              if col_cnt = IMG_WIDTH - 1 then
                col_cnt <= 0;
                if row_cnt = IMG_HEIGHT - 1 then
                  row_cnt <= IMG_HEIGHT - 1; -- hold
                else
                  row_cnt <= row_cnt + 1;
                end if;
              else
                col_cnt <= col_cnt + 1;
              end if;

              -- stash current pixel for the writeback stage via sum_a_dout/cnt_a_dout next cycle
              -- We'll perform the WRITE in PASS1_WRITE.
              state <= PASS1_WRITE;
            end if;

          when PASS1_WRITE =>
            -- Perform RMW: write back sum += pix, count += 1 to same bidx
            sum_a_en   <= '1';
            sum_a_we   <= '1';
            sum_a_addr <= std_logic_vector(bidx);
            sum_a_din  <= std_logic_vector(unsigned(sum_a_dout) + resize(unsigned(rd_dout),16));

            cnt_a_en   <= '1';
            cnt_a_we   <= '1';
            cnt_a_addr <= std_logic_vector(bidx);
            cnt_a_din  <= std_logic_vector(unsigned(cnt_a_dout) + 1);

            -- If more pixels are coming, go back to READ; else transition to MAKE_THR
            if addr_issued < TOTAL_PIXELS or rd_en_q='1' then
              state <= PASS1_READ;
            else
              -- prepare MAKE_THR over all blocks
              idx_thr   <= (others => '0');
              state     <= MAKE_THR_READ;
            end if;

          -- ==============================
          -- MAKE_THR 3-stage pipeline
          -- ==============================
          when MAKE_THR_READ =>
            if to_integer(idx_thr) < NBLOCKS then
              -- Drive BRAM Port B to fetch sum and count for idx_thr
              sum_b_en   <= '1';
              sum_b_addr <= std_logic_vector(idx_thr);
              cnt_b_en   <= '1';
              cnt_b_addr <= std_logic_vector(idx_thr);
              state <= MAKE_THR_MUL;
            else
              busy_r <= '0';
              done_r <= '1';
              state  <= DONE;
            end if;

          when MAKE_THR_MUL =>
            -- Register BRAM outputs and compute product = sum * inv_cnt[count]
            sum_q <= unsigned(sum_b_dout);
            cnt_q <= unsigned(cnt_b_dout);
            -- prod_q width 25 bits is enough (16+8+1 headroom)
            if cnt_b_dout = x"00" then
              -- handle empty block; set mean to 128 directly next cycle
              prod_q <= to_unsigned(128*256, prod_q'length); -- 128 << 8
            else
             prod_q <= resize(unsigned(sum_b_dout) *
                 inv_cnt(to_integer(cnt_q(6 downto 0))),
                 prod_q'length);
            end if;
            state <= MAKE_THR_WRITE;

          when MAKE_THR_WRITE =>
            -- mean_q = prod >> 8 (Q8)
            mean_q <= resize( prod_q(24 downto 8), 16);
            -- Subtract offset and clamp to 0..255
            thr_i := to_integer(mean_q) - THRESH_OFFSET;
            if    thr_i < 0   then thr_data_r <= (others => '0');
            elsif thr_i > 255 then thr_data_r <= (others => '1');
            else                   thr_data_r <= std_logic_vector(to_unsigned(thr_i,8));
            end if;

            thr_addr_r <= idx_thr;
            thr_we_r   <= '1';
            idx_thr    <= idx_thr +1;
            state      <= MAKE_THR_READ;

          when DONE =>
            -- one-cycle done pulse, then idle
            state <= IDLE;

        end case;
      end if;
    end if;
  end process;

end architecture;