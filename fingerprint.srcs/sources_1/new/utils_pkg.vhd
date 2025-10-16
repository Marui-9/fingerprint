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

package utils_pkg is
  function ceil_div(a : integer; b : integer) return integer;
  function clog2(n : integer) return integer;
end package;

package body utils_pkg is
  function ceil_div(a : integer; b : integer) return integer is
    variable q : integer;
  begin
    if b = 0 then return 0; end if;
    q := a / b;
    if (a mod b) /= 0 then q := q + 1; end if;
    return q;
  end function;

  function clog2(n : integer) return integer is
    variable v : integer := 0;
    variable t : integer := n-1;
  begin
    if n <= 1 then return 0; end if;
    while t > 0 loop
      t := t / 2;
      v := v + 1;
    end loop;
    return v;
  end function;
end package body;
