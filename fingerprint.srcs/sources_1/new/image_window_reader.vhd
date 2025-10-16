----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 16.09.2025 10:43:10
-- Design Name: 
-- Module Name: image_window_reader - Behavioral
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

entity image_window_reader is
  generic (
    DATA_WIDTH : integer := 8;    -- bits per pixel
    ADDR_WIDTH : integer := 13;   -- must address IMG_WIDTH*IMG_HEIGHT (68*118=8024 -> 13 bits OK)
    IMG_WIDTH  : integer := 68;
    IMG_HEIGHT : integer := 118
  );
  port (
    -- Clock & reset (reader / processing clock domain)
    bram_rd_clk : in  std_logic;
    rst         : in  std_logic;   -- active-high synchronous reset to bram_rd_clk

    -- Control
    start       : in  std_logic;   -- pulse/high to start processing a frame

    -- data_bram Port B (reader)
    bram_rd_en   : out std_logic;
    bram_rd_addr : out unsigned(ADDR_WIDTH-1 downto 0);
    bram_rd_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);

    -- 3x3 window outputs (valid for interior pixels only)
    pixel_valid : out std_logic; -- '1' when (w00..w22) & center_x/center_y are valid
    center_x    : out std_logic_vector(7 downto 0); -- 0..IMG_WIDTH-1
    center_y    : out std_logic_vector(7 downto 0); -- 0..IMG_HEIGHT-1

    -- Window arranged as:
    -- w00 w01 w02
    -- w10 w11 w12
    -- w20 w21 w22
    w00 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w01 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w02 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w10 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w11 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w12 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w20 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w21 : out std_logic_vector(DATA_WIDTH-1 downto 0);
    w22 : out std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Status
    busy       : out std_logic;  -- '1' while streaming the frame
    done_pulse : out std_logic   -- one-cycle pulse when the frame stream is complete
  );
end entity image_window_reader;

architecture rtl of image_window_reader is

  ----------------------------------------------------------------------------
  -- Types & constants
  ----------------------------------------------------------------------------
  constant TOTAL_PIXELS : integer := IMG_WIDTH * IMG_HEIGHT;

  -- A row buffer type: holds one full row of pixels
  type row_type is array (0 to IMG_WIDTH-1) of std_logic_vector(DATA_WIDTH-1 downto 0);

  ----------------------------------------------------------------------------
  -- Three physical row buffers (circularly rotated as top/mid/cur)
  ----------------------------------------------------------------------------
  signal buf0, buf1, buf2 : row_type;

  -- Which physical buffer currently serves as top, mid, and current row
  signal top_idx : integer range 0 to 2 := 0;
  signal mid_idx : integer range 0 to 2 := 1;
  signal cur_idx : integer range 0 to 2 := 2;

  ----------------------------------------------------------------------------
  -- Read address issuance vs data return (1-cycle latency model)
  ----------------------------------------------------------------------------
  signal rd_en_s     : std_logic := '0';    -- internal driver for bram_rd_en
  signal rd_addr_s   : unsigned(ADDR_WIDTH-1 downto 0) := (others => '0');
  signal addr_count  : integer range 0 to TOTAL_PIXELS := 0; -- number of addresses issued so far
  signal rd_en_q     : std_logic := '0';    -- rd_en_s delayed by one clock
  signal data_valid  : std_logic := '0';    -- '1' when bram_rd_dout holds a valid pixel this cycle

  ----------------------------------------------------------------------------
  -- Row/column counters for where the incoming pixel (data_valid=1) belongs
  ----------------------------------------------------------------------------
  signal row_cnt : integer range 0 to IMG_HEIGHT-1 := 0;
  signal col_cnt : integer range 0 to IMG_WIDTH-1  := 0;

  ----------------------------------------------------------------------------
  -- Temporary combinational reads from 'top' and 'mid' buffers at current col
  ----------------------------------------------------------------------------
  signal tmp_top_pix : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal tmp_mid_pix : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

  ----------------------------------------------------------------------------
  -- 3x3 column shift registers to assemble the window
  -- Each cycle (when data_valid='1'):
  --   shift left:  *_left <= *_mid; *_mid <= *_right
  --   load  right: top_right<=tmp_top_pix; mid_right<=tmp_mid_pix; bot_right<=bram_rd_dout
  ----------------------------------------------------------------------------
  signal top_left, top_mid, top_right : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal mid_left, mid_mid, mid_right : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
  signal bot_left, bot_mid, bot_right : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

  ----------------------------------------------------------------------------
  -- FSM
  ----------------------------------------------------------------------------
  type state_type is (IDLE, FILL_ROW0, FILL_ROW1, STREAM_ROWS, FINISH);
  signal state : state_type := IDLE;

  ----------------------------------------------------------------------------
  -- Outputs (registered)
  ----------------------------------------------------------------------------
  signal pixel_valid_r : std_logic := '0';
  signal center_x_r    : std_logic_vector(7 downto 0) := (others => '0');
  signal center_y_r    : std_logic_vector(7 downto 0) := (others => '0');
  signal busy_r        : std_logic := '0';
  signal done_pulse_r  : std_logic := '0';

begin

  ----------------------------------------------------------------------------
  -- Drive external BRAM Port B signals
  ----------------------------------------------------------------------------
  bram_rd_en   <= rd_en_s;
  bram_rd_addr <= rd_addr_s;

  ----------------------------------------------------------------------------
  -- rd_en_s delayed by 1 clock -> data_valid (models 1-cycle BRAM read latency)
  ----------------------------------------------------------------------------
  latency_proc : process (bram_rd_clk)
  begin
    if rising_edge(bram_rd_clk) then
      if rst = '1' then
        rd_en_q    <= '0';
        data_valid <= '0';
      else
        rd_en_q    <= rd_en_s;
        data_valid <= rd_en_q;  -- valid data one cycle after enabling a read
      end if;
    end if;
  end process latency_proc;

  ----------------------------------------------------------------------------
  -- Combinational read from selected top/mid buffers at current column
  ----------------------------------------------------------------------------
  buf_read_comb : process (top_idx, mid_idx, col_cnt, buf0, buf1, buf2)
  begin
    -- Default
    tmp_top_pix <= (others => '0');
    tmp_mid_pix <= (others => '0');

    -- Select which physical buffer is "top" at this moment
    case top_idx is
      when 0 => tmp_top_pix <= buf0(col_cnt);
      when 1 => tmp_top_pix <= buf1(col_cnt);
      when others => tmp_top_pix <= buf2(col_cnt);
    end case;

    -- Select which physical buffer is "mid" at this moment
    case mid_idx is
      when 0 => tmp_mid_pix <= buf0(col_cnt);
      when 1 => tmp_mid_pix <= buf1(col_cnt);
      when others => tmp_mid_pix <= buf2(col_cnt);
    end case;
  end process buf_read_comb;

  ----------------------------------------------------------------------------
  -- Main FSM: issue BRAM addresses, capture returned pixels into row buffers,
  -- rotate buffers per row, and control streaming state.
  ----------------------------------------------------------------------------
  fsm_proc : process (bram_rd_clk)
    variable t : integer range 0 to 2;  -- for rotating indices safely
  begin
    if rising_edge(bram_rd_clk) then
      if rst = '1' then
        -- Reset everything
        state        <= IDLE;
        rd_en_s      <= '0';
        rd_addr_s    <= (others => '0');
        addr_count   <= 0;

        row_cnt      <= 0;
        col_cnt      <= 0;

        top_idx      <= 0;
        mid_idx      <= 1;
        cur_idx      <= 2;

        busy_r       <= '0';
        done_pulse_r <= '0';

      else
        -- defaults each cycle
        done_pulse_r <= '0';

        case state is

          --------------------------------------------------------------------
          when IDLE =>
            busy_r    <= '0';
            rd_en_s   <= '0';
            addr_count<=  0;
            row_cnt   <=  0;
            col_cnt   <=  0;
            -- Wait for start pulse (you can assert it after data_bram.frame_done)
            if start = '1' then
              busy_r   <= '1';
              state    <= FILL_ROW0;
              rd_en_s  <= '1'; -- start issuing sequential reads
              rd_addr_s<= to_unsigned(0, ADDR_WIDTH);
              addr_count <= 1; -- we've just issued address 0
            end if;

          --------------------------------------------------------------------
          when FILL_ROW0 =>
            -- Keep issuing addresses sequentially until we cover the whole frame.
            if (addr_count < TOTAL_PIXELS) then
              rd_en_s   <= '1';
              rd_addr_s <= to_unsigned(addr_count, ADDR_WIDTH);
              addr_count<= addr_count + 1;
            else
              rd_en_s   <= '0';
            end if;

            -- When data_valid=1, a pixel for the current col of row 0 arrives
            if data_valid = '1' then
              -- write into the physical buffer currently mapped as "top"
              if top_idx = 0 then
                buf0(col_cnt) <= bram_rd_dout;
              elsif top_idx = 1 then
                buf1(col_cnt) <= bram_rd_dout;
              else
                buf2(col_cnt) <= bram_rd_dout;
              end if;

              if col_cnt = IMG_WIDTH - 1 then
                -- Row 0 complete -> move to fill row 1
                col_cnt <= 0;
                row_cnt <= 1;
                state   <= FILL_ROW1;
              else
                col_cnt <= col_cnt + 1;
              end if;
            end if;

          --------------------------------------------------------------------
          when FILL_ROW1 =>
            -- Continue issuing addresses if any remain
            if (addr_count < TOTAL_PIXELS) then
              rd_en_s   <= '1';
              rd_addr_s <= to_unsigned(addr_count, ADDR_WIDTH);
              addr_count<= addr_count + 1;
            else
              rd_en_s   <= '0';
            end if;

            if data_valid = '1' then
              -- write into the physical buffer currently mapped as "mid"
              if mid_idx = 0 then
                buf0(col_cnt) <= bram_rd_dout;
              elsif mid_idx = 1 then
                buf1(col_cnt) <= bram_rd_dout;
              else
                buf2(col_cnt) <= bram_rd_dout;
              end if;

              if col_cnt = IMG_WIDTH - 1 then
                -- Row 1 complete -> start streaming rows 2..HEIGHT-1
                col_cnt <= 0;
                row_cnt <= 2;
                state   <= STREAM_ROWS;
              else
                col_cnt <= col_cnt + 1;
              end if;
            end if;

          --------------------------------------------------------------------
          when STREAM_ROWS =>
            -- Continue issuing addresses if any remain
            if (addr_count < TOTAL_PIXELS) then
              rd_en_s   <= '1';
              rd_addr_s <= to_unsigned(addr_count, ADDR_WIDTH);
              addr_count<= addr_count + 1;
            else
              rd_en_s   <= '0';
            end if;

            if data_valid = '1' then
              -- Store the current pixel (row_cnt) into the buffer mapped as "cur"
              if cur_idx = 0 then
                buf0(col_cnt) <= bram_rd_dout;
              elsif cur_idx = 1 then
                buf1(col_cnt) <= bram_rd_dout;
              else
                buf2(col_cnt) <= bram_rd_dout;
              end if;

              -- Advance along the row
              if col_cnt = IMG_WIDTH - 1 then
                -- End of this row: rotate buffer indices so that:
                --   top <- mid, mid <- cur, cur <- top(old)
                t       := top_idx;
                top_idx <= mid_idx;
                mid_idx <= cur_idx;
                cur_idx <= t;

                col_cnt <= 0;

                if row_cnt = IMG_HEIGHT - 1 then
                  -- That was the last row: finish after this pixel
                  state   <= FINISH;
                else
                  row_cnt <= row_cnt + 1;
                end if;

              else
                col_cnt <= col_cnt + 1;
              end if;
            end if;

          --------------------------------------------------------------------
          when FINISH =>
            -- Stop issuing reads; report one-cycle done_pulse and return to IDLE
            rd_en_s      <= '0';
            busy_r       <= '0';
            done_pulse_r <= '1';
            state        <= IDLE;

        end case;
      end if;
    end if;
  end process fsm_proc;

  ----------------------------------------------------------------------------
  -- 3x3 Window shift + valid/center coordinates
  -- On every valid pixel (data_valid='1'):
  --   - shift left previous 3 columns
  --   - load "right" column from {top, mid buffers} and current pixel (bottom)
  --   - assert pixel_valid when we have at least 3 columns and 3 rows processed
  ----------------------------------------------------------------------------
  window_proc : process (bram_rd_clk)
  begin
    if rising_edge(bram_rd_clk) then
      if rst = '1' then
        top_left  <= (others => '0'); top_mid <= (others => '0'); top_right <= (others => '0');
        mid_left  <= (others => '0'); mid_mid <= (others => '0'); mid_right <= (others => '0');
        bot_left  <= (others => '0'); bot_mid <= (others => '0'); bot_right <= (others => '0');
        pixel_valid_r <= '0';
        center_x_r    <= (others => '0');
        center_y_r    <= (others => '0');

      else
        -- default: no valid window unless set below
        pixel_valid_r <= '0';

        if data_valid = '1' then
          -- Shift previous columns left
          top_left <= top_mid;  top_mid <= top_right;
          mid_left <= mid_mid;  mid_mid <= mid_right;
          bot_left <= bot_mid;  bot_mid <= bot_right;

          -- Load "right" column from buffers and current pixel
          -- tmp_top_pix = pixel at (row_cnt-2, col_cnt) from top buffer
          -- tmp_mid_pix = pixel at (row_cnt-1, col_cnt) from mid buffer
          -- bram_rd_dout= pixel at (row_cnt,   col_cnt) = bottom row value
          top_right <= tmp_top_pix;
          mid_right <= tmp_mid_pix;
          bot_right <= bram_rd_dout;

          -- We can emit a valid 3x3 window when the window center exists,
          -- i.e., we have at least 3 rows and 3 cols:
          -- Center is (x = col_cnt-1, y = row_cnt-1)
          if (row_cnt >= 2) and (col_cnt >= 2) then
            pixel_valid_r <= '1';
            center_x_r    <= std_logic_vector(to_unsigned(col_cnt - 1, 8));
            center_y_r    <= std_logic_vector(to_unsigned(row_cnt - 1, 8));
          end if;
        end if;
      end if;
    end if;
  end process window_proc;

  ----------------------------------------------------------------------------
  -- Drive outputs
  ----------------------------------------------------------------------------
  pixel_valid <= pixel_valid_r;
  center_x    <= center_x_r;
  center_y    <= center_y_r;

  w00 <= top_left;  w01 <= top_mid;  w02 <= top_right;
  w10 <= mid_left;  w11 <= mid_mid;  w12 <= mid_right;
  w20 <= bot_left;  w21 <= bot_mid;  w22 <= bot_right;

  busy       <= busy_r;
  done_pulse <= done_pulse_r;

end architecture rtl;

