----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 22.09.2025 22:50:34
-- Design Name: 
-- Module Name: template_bram - Behavioral
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

entity template_bram is
  generic (
    DATA_WIDTH : integer := 32;
    ADDR_WIDTH : integer := 13  -- 2^11 doublewords(32 bits) * 4B = 65 kb
  );
  port (
    -- Port A (pipeline/control unit)
    a_clk  : in  std_logic;
    a_en   : in  std_logic;
    a_we   : in  std_logic_vector((DATA_WIDTH/8)-1 downto 0);
    a_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    a_din  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    a_dout : out std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Port B (CPU)
    b_clk  : in  std_logic;
    b_en   : in  std_logic;
    b_we   : in  std_logic_vector((DATA_WIDTH/8)-1 downto 0);
    b_addr : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    b_din  : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    b_dout : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
end entity;

architecture rtl of template_bram is
  type ram_t is array (0 to (2**ADDR_WIDTH)-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal ram : ram_t;
begin

  -- Port A
  process(a_clk)
  begin
    if rising_edge(a_clk) then
      if a_en = '1' then
       -- if a_we /= (others => '0') then
       if unsigned(a_we) /= 0 then
          for i in 0 to (DATA_WIDTH/8)-1 loop
            if a_we(i) = '1' then
              ram(to_integer(unsigned(a_addr)))(8*i+7 downto 8*i) <= a_din(8*i+7 downto 8*i);
            end if;
          end loop;
        end if;
        a_dout <= ram(to_integer(unsigned(a_addr)));
      end if;
    end if;
  end process;

  -- Port B
  process(b_clk)
  begin
    if rising_edge(b_clk) then
      if b_en = '1' then
    --    if b_we /= (b_we'range => '0') then
        if unsigned(a_we) /= 0 then
          for i in 0 to (DATA_WIDTH/8)-1 loop
            if b_we(i) = '1' then
              ram(to_integer(unsigned(b_addr)))(8*i+7 downto 8*i) <= b_din(8*i+7 downto 8*i);
            end if;
          end loop;
        end if;
        b_dout <= ram(to_integer(unsigned(b_addr)));
      end if;
    end if;
  end process;

end architecture;

