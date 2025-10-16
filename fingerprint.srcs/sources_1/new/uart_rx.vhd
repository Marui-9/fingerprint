----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 10.09.2025 15:47:41
-- Design Name: 
-- Module Name: uart_rx - Behavioral
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

entity uart_rx is
    generic (
        CLK_FREQ   : integer := 50_000_000; -- System clock frequency (Hz) = 50 MHz
        BAUD_RATE  : integer := 9600;       -- UART baud rate
        OVERSAMPLE : integer := 16          -- Oversampling factor (16 typical)
    );
    port (
        clk        : in  std_logic;
        reset      : in  std_logic;
        rx         : in  std_logic;  -- Serial input line

        data_out   : out std_logic_vector(7 downto 0);
        done       : out std_logic;  -- Pulse high when a byte is received
        parity_err : out std_logic;  -- High if parity error detected
        frame_err  : out std_logic   -- High if stop bit not valid
    );
end uart_rx;

architecture rtl of uart_rx is

  
    -- Baud tick generator
    -- Divides system clock into oversampling ticks (e.g., 16 per bit)
   
    constant BAUD_TICK_HZ   : integer := BAUD_RATE * OVERSAMPLE;
    constant BAUD_TICK_COUNT: integer := CLK_FREQ / BAUD_TICK_HZ;
    -- Guard against division by zero
    function safe_divisor(val : integer) return integer is
    begin
        if val = 0 then
            return 1;
        else
            return val;
        end if;
    end function;

    constant BAUD_DIV : integer := safe_divisor(BAUD_TICK_COUNT);

    signal baud_cnt   : integer range 0 to BAUD_DIV := 0;
    signal baud_tick  : std_logic := '0';

    -- UART FSM states
    type state_type is (IDLE, START, DATA, PARITY, STOP);
    signal state       : state_type := IDLE;

    -- Internal signals
    signal sample_cnt  : integer range 0 to OVERSAMPLE-1 := 0; -- oversample counter
    signal bit_idx     : integer range 0 to 7 := 0;            -- which data bit
    signal shift_reg   : std_logic_vector(7 downto 0) := (others => '0');
    signal parity_calc : std_logic := '0';
    signal parity_bit  : std_logic := '0';

    -- Outputs
    signal data_reg    : std_logic_vector(7 downto 0) := (others => '0');
    signal done_reg    : std_logic := '0';
    signal parity_err_reg : std_logic := '0';
    signal frame_err_reg  : std_logic := '0';

begin
    -- Output assignment
    data_out   <= data_reg;
    done       <= done_reg;
    parity_err <= parity_err_reg;
    frame_err  <= frame_err_reg;

    -- Baud tick process
    -- Generates a pulse at oversampling frequency
    baud_gen : process(clk, reset)
    begin
        if reset = '1' then
            baud_cnt  <= 0;
            baud_tick <= '0';
        elsif rising_edge(clk) then
            if baud_cnt = BAUD_DIV then
                baud_cnt  <= 0;
                baud_tick <= '1';
            else
                baud_cnt  <= baud_cnt + 1;
                baud_tick <= '0';
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------
    -- UART Receiver FSM
    ----------------------------------------------------------------------
    uart_fsm : process(clk, reset)
    begin
        if reset = '1' then
            state          <= IDLE;
            sample_cnt     <= 0;
            bit_idx        <= 0;
            shift_reg      <= (others => '0');
            parity_calc    <= '0';
            data_reg       <= (others => '0');
            done_reg       <= '0';
            parity_err_reg <= '0';
            frame_err_reg  <= '0';

        elsif rising_edge(clk) then
            done_reg <= '0'; -- default, only pulse for one cycle

            if baud_tick = '1' then
                case state is

                    when IDLE =>
                        -- Wait for start bit (falling edge)
                        if rx = '0' then
                            state      <= START;
                            sample_cnt <= 0;
                        end if;

                    when START =>
                        -- Sample in middle of start bit
                        if sample_cnt = OVERSAMPLE/2 then
                            if rx = '0' then
                                -- Valid start bit
                                state      <= DATA;
                                bit_idx    <= 0;
                                parity_calc<= '0';
                            else
                                -- False start
                                state <= IDLE;
                            end if;
                        end if;
                        sample_cnt <= (sample_cnt + 1) mod OVERSAMPLE;

                    when DATA =>
                        if sample_cnt = OVERSAMPLE-1 then
                            -- Sample bit at the end of oversample cycle
                            shift_reg(bit_idx) <= rx;
                            parity_calc <= parity_calc xor rx;
                            if bit_idx = 7 then
                                state <= PARITY;
                            else
                                bit_idx <= bit_idx + 1;
                            end if;
                        end if;
                        sample_cnt <= (sample_cnt + 1) mod OVERSAMPLE;

                    when PARITY =>
                        if sample_cnt = OVERSAMPLE-1 then
                            parity_bit <= rx;
                            -- check parity (expect even parity here)
                            if parity_calc /= rx then
                                parity_err_reg <= '1';
                            else
                                parity_err_reg <= '0';
                            end if;
                            state <= STOP;
                        end if;
                        sample_cnt <= (sample_cnt + 1) mod OVERSAMPLE;

                    when STOP =>
                        if sample_cnt = OVERSAMPLE-1 then
                            if rx = '1' then
                                -- Valid stop bit
                                frame_err_reg <= '0';
                            else
                                frame_err_reg <= '1';
                            end if;
                            data_reg <= shift_reg;
                            done_reg <= '1';
                            state    <= IDLE;
                        end if;
                        sample_cnt <= (sample_cnt + 1) mod OVERSAMPLE;

                end case;
            end if; 
        end if;
    end process;

end rtl;
