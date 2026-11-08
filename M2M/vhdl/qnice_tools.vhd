----------------------------------------------------------------------------------
-- Miscellaneous Tools for QNICE
-- 
-- done in July 2020 by sy2002
-- From QNICE-FPGA (github.com/sy2002/QNICE-FPGA), vhdl/tools.vhd, kept for f_log2
-- when the QNICE submodule was removed.
--
-- Copyright 2006-2020 The QNICE Development Team
--
-- Redistribution and use in source and binary forms, with or without modification, are
-- permitted provided that the following conditions are met:
-- 1. Redistributions of source code must retain the above copyright notice, this list of
--    conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list
--    of conditions and the following disclaimer in the documentation and/or other
--    materials provided with the distribution.
-- 3. Neither the name of the copyright holder nor the names of its contributors may be
--    used to endorse or promote products derived from this software without specific
--    prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
-- THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
-- THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package qnice_tools is

-- calculate the width (amount of bits) needed to store x
function f_log2(x: positive) return natural;

end qnice_tools;

package body qnice_tools is

function f_log2(x: positive) return natural is
variable i : natural;
begin
   i := 0;  
   while (2**i < x) and i < 127 loop
      i := i + 1;
   end loop;
   return i;
end function;

end qnice_tools;