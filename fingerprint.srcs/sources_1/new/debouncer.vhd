----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 24.09.2025 15:31:56
-- Design Name: 
-- Module Name: debouncer - Behavioral
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


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY debouncer IS
  GENERIC (
    counter_size : INTEGER := 12
  );
  PORT (
    clock, reset : IN STD_LOGIC; --! clock and reset
    bouncy : IN STD_LOGIC; --! input that can bounce even in less than one clock cycle (the debouncer can be connected to a slow clock)
    pulse : OUT STD_LOGIC --! send a pulse as soon as the stable state of the button touch is verified
  );
END debouncer;

ARCHITECTURE behavioral OF debouncer IS

  -- The counter that keeps track of the when the signal is stable
  SIGNAL counter : unsigned(counter_size - 1 DOWNTO 0);
  -- Keep track of the candidate stable value
  SIGNAL candidate_value : STD_LOGIC;
  -- Keep track of the actual stable value
  SIGNAL stable_value : STD_LOGIC;
  -- A delayed version of the stable value allows us to check when it has a transition
  SIGNAL delayed_stable_value : STD_LOGIC;

BEGIN

  PROCESS (clock, reset) BEGIN
    IF reset = '1' THEN
      counter <= (OTHERS => '1');
      candidate_value <= '0';
      stable_value <= '0';
    ELSIF rising_edge(clock) THEN
      -- See if the signal is stable
      IF bouncy = candidate_value THEN
        -- It is. Check if it has been stable for long time
        IF counter = 0 THEN
          -- The signal is stable. Update the stable signal value
          stable_value <= candidate_value;
        ELSE
          -- We still need to wait for the counter to go down
          counter <= counter - 1;
        END IF;
      ELSE
        -- The signal is not stable. Reset the counter and the candidate stable value
        candidate_value <= bouncy;
        counter <= (OTHERS => '1');
      END IF;
    END IF;
  END PROCESS;

  -- Create a delayed version of the stable signal
  PROCESS (clock, reset) BEGIN
    IF reset = '1' THEN
      delayed_stable_value <= '0';
    ELSIF rising_edge(clock) THEN
      delayed_stable_value <= stable_value;
    END IF;
  END PROCESS;

  -- Generate the pulse
  pulse <= '1' WHEN stable_value = '1' AND delayed_stable_value = '0' ELSE
    '0';
END behavioral;
