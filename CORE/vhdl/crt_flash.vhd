----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- crt_flash: the two AM29F040 flash chips of an EasyFlash, ROML's and ROMH's, as the CPU sees
-- them. It follows their command sequences, answers ID and status reads, and hands each program
-- or erase to the memory side as a job (crt_reader on R4 and later), which changes the file.
-- After VICE's flash040core.c (AM29F040B) and Sorgelig's am29f040.sv for MiSTer.
--
-- A chip only sees a write while it is selected for one, i.e. in Ultimax mode (main.vhd).
-- erase_i erases a sector as the chip's own command does, for UCI's EASYFLASH command
-- (Gideon's control target), unless that chip is busy.
-- While it is busy or in autoselect it answers reads itself (*_oe_o). A job's status shows
-- until the memory side reports it done and the next PHI2 rise has passed: a CPU read whose
-- byte was fetched before the change is latched by then.
--
-- Runs on the core clock.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity crt_flash is
   generic (
      G_PROG_CLKS  : natural := 256;                        -- a program shows busy at least this long
      G_ERASE_CLKS : natural := 65536                       -- an erase too (~2 ms; a real one takes ~1 s)
   );
   port (
      clk_i       : in  std_logic;
      rst_i       : in  std_logic;                          -- C128 reset or no EasyFlash: back to read
      wr_i        : in  std_logic;                          -- one clock per write that selects a chip
      rd_i        : in  std_logic;                          -- one clock per read that selects a chip
      hi_i        : in  std_logic;                          -- the access is to ROMH's chip
      bank_lo_i   : in  std_logic_vector( 5 downto 0);
      bank_hi_i   : in  std_logic_vector( 5 downto 0);
      addr_i      : in  std_logic_vector(12 downto 0);
      data_i      : in  std_logic_vector( 7 downto 0);
      phi2_i      : in  std_logic;

      -- Index 0 is ROML's chip, 1 ROMH's. While oe is set, the chip answers reads with data.
      oe_o        : out std_logic_vector(1 downto 0);
      lo_data_o   : out std_logic_vector(7 downto 0);
      hi_data_o   : out std_logic_vector(7 downto 0);

      -- Jobs: op (01 program, 10 sector erase, 11 chip erase), bank, offset, data. A new job
      -- toggles req; done follows req when it is finished, with fail set if the byte didn't take.
      req_o       : out std_logic_vector(1 downto 0);
      lo_job_o    : out std_logic_vector(28 downto 0);
      hi_job_o    : out std_logic_vector(28 downto 0);
      done_i      : in  std_logic_vector(1 downto 0);
      fail_i      : in  std_logic_vector(1 downto 0);
      -- One clock: erase the 64 kB sector (8 banks) bank is in, of ROMH's chip (hi) or ROML's
      erase_i     : in  std_logic := '0';
      erase_hi_i  : in  std_logic := '0';
      erase_bank_i : in std_logic_vector(5 downto 0) := (others => '0');
      busy_o      : out std_logic_vector(1 downto 0)   -- a job of the chip is under way
   );
end entity crt_flash;

architecture synthesis of crt_flash is

   constant C_MANUF_ID  : std_logic_vector(7 downto 0) := x"01";   -- AMD
   constant C_DEVICE_ID : std_logic_vector(7 downto 0) := x"A4";   -- AM29F040

   type t_state is (READ_ST, UNLOCK1_ST, UNLOCK2_ST, PROG_ST, ERASE1_ST, ERASE2_ST, ERASE3_ST,
                    BUSY_ST, ERROR_ST);
   type t_states is array (0 to 1) of t_state;
   type t_jobs   is array (0 to 1) of std_logic_vector(28 downto 0);
   type t_bytes  is array (0 to 1) of std_logic_vector(7 downto 0);
   type t_timers is array (0 to 1) of natural range 0 to G_PROG_CLKS + G_ERASE_CLKS;

   signal state    : t_states := (others => READ_ST);
   signal autosel  : std_logic_vector(1 downto 0) := "00";   -- autoselect is the base state
   signal erasing  : std_logic_vector(1 downto 0) := "00";   -- the busy job is an erase
   signal toggle   : std_logic_vector(1 downto 0) := "00";   -- DQ6, flips with each status read
   signal prog_d7  : std_logic_vector(1 downto 0) := "00";   -- bit 7 of the byte being programmed
   signal timer    : t_timers := (others => 0);
   signal req_r    : std_logic_vector(1 downto 0) := "00";
   signal job_r    : t_jobs := (others => (others => '0'));
   signal oe_r     : std_logic_vector(1 downto 0) := "00";
   signal data_r   : t_bytes := (others => (others => '1'));
   signal phi2_d   : std_logic := '0';

begin

   req_o     <= req_r;
   lo_job_o  <= job_r(0);
   hi_job_o  <= job_r(1);
   oe_o      <= oe_r;
   busy_o(0) <= '1' when state(0) = BUSY_ST else '0';
   busy_o(1) <= '1' when state(1) = BUSY_ST else '0';
   lo_data_o <= data_r(0);
   hi_data_o <= data_r(1);

   p_flash : process (clk_i)
      variable v_chip : natural range 0 to 1;
      variable v_sel  : boolean;
      variable v_bank : std_logic_vector(5 downto 0);
      variable v_555  : boolean;
      variable v_2aa  : boolean;
   begin
      if rising_edge(clk_i) then
         phi2_d <= phi2_i;

         for c in 0 to 1 loop
            v_sel  := (hi_i = '1') = (c = 1);
            v_bank := bank_lo_i;
            if c = 1 then
               v_bank := bank_hi_i;
            end if;
            -- The AM29F040B decodes A10-A0 of its command addresses
            v_555  := addr_i(10 downto 0) = "101" & x"55";
            v_2aa  := addr_i(10 downto 0) = "010" & x"AA";

            if timer(c) /= 0 then
               timer(c) <= timer(c) - 1;
            end if;
            if rd_i = '1' and v_sel and (state(c) = BUSY_ST or state(c) = ERROR_ST) then
               toggle(c) <= not toggle(c);
            end if;

            if wr_i = '1' and v_sel then
               case state(c) is
                  when READ_ST =>
                     if autosel(c) = '1' and data_i = x"F0" then
                        autosel(c) <= '0';
                     elsif v_555 and data_i = x"AA" then
                        state(c) <= UNLOCK1_ST;
                     end if;

                  when UNLOCK1_ST =>
                     state(c) <= UNLOCK2_ST when v_2aa and data_i = x"55" else READ_ST;

                  when UNLOCK2_ST =>
                     state(c) <= READ_ST;
                     if v_555 then
                        case data_i is
                           when x"90"  => autosel(c) <= '1';
                           when x"F0"  => autosel(c) <= '0';
                           when x"A0"  => state(c) <= PROG_ST;
                           when x"80"  => state(c) <= ERASE1_ST;
                           when others => null;
                        end case;
                     end if;

                  when PROG_ST =>
                     job_r(c)   <= "01" & v_bank & addr_i & data_i;
                     req_r(c)   <= not req_r(c);
                     prog_d7(c) <= data_i(7);
                     erasing(c) <= '0';
                     timer(c)   <= G_PROG_CLKS;
                     state(c)   <= BUSY_ST;

                  when ERASE1_ST =>
                     state(c) <= ERASE2_ST when v_555 and data_i = x"AA" else READ_ST;

                  when ERASE2_ST =>
                     state(c) <= ERASE3_ST when v_2aa and data_i = x"55" else READ_ST;

                  when ERASE3_ST =>
                     state(c) <= READ_ST;
                     if (v_555 and data_i = x"10") or data_i = x"30" then
                        -- Chip erase, or erase the 64 kB sector (8 banks) the address is in
                        job_r(c)   <= "10" & v_bank & addr_i & data_i;
                        if data_i = x"10" then
                           job_r(c)(28 downto 27) <= "11";
                        end if;
                        req_r(c)   <= not req_r(c);
                        erasing(c) <= '1';
                        timer(c)   <= G_ERASE_CLKS;
                        state(c)   <= BUSY_ST;
                     end if;

                  when BUSY_ST =>
                     null;

                  when ERROR_ST =>
                     -- A 0 programmed back to 1: status with DQ5 set until a reset command
                     if data_i = x"F0" then
                        autosel(c) <= '0';
                        state(c)   <= READ_ST;
                     elsif v_555 and data_i = x"AA" then
                        state(c) <= UNLOCK1_ST;
                     end if;
               end case;
            end if;

            if erase_i = '1' and (erase_hi_i = '1') = (c = 1) and state(c) /= BUSY_ST then
               job_r(c)   <= "10" & erase_bank_i & '0' & x"000" & x"30";
               req_r(c)   <= not req_r(c);
               erasing(c) <= '1';
               timer(c)   <= G_ERASE_CLKS;
               state(c)   <= BUSY_ST;
            end if;

            if state(c) = BUSY_ST and done_i(c) = req_r(c) and timer(c) = 0 and
               phi2_i = '1' and phi2_d = '0' then
               state(c) <= ERROR_ST when fail_i(c) = '1' and erasing(c) = '0' else READ_ST;
            end if;

            if rst_i = '1' then
               autosel(c) <= '0';
               if state(c) /= BUSY_ST then
                  state(c) <= READ_ST;
               end if;
            end if;

            -- What a read of this chip returns, if not its memory
            oe_r(c)   <= '1';
            data_r(c) <= x"00";
            case state(c) is
               when BUSY_ST =>
                  -- DQ7 inverted data (0 while erasing), DQ6 toggle, DQ3 erase timer running
                  data_r(c)(7) <= not prog_d7(c) and not erasing(c);
                  data_r(c)(6) <= toggle(c);
                  data_r(c)(3) <= erasing(c);
               when ERROR_ST =>
                  data_r(c)(7) <= not prog_d7(c);
                  data_r(c)(6) <= toggle(c);
                  data_r(c)(5) <= '1';
               when READ_ST =>
                  oe_r(c) <= autosel(c);
                  if addr_i(7 downto 0) = x"00" then
                     data_r(c) <= C_MANUF_ID;
                  elsif addr_i(7 downto 0) = x"01" then
                     data_r(c) <= C_DEVICE_ID;
                  end if;
               when others =>
                  oe_r(c) <= '0';
            end case;
         end loop;
      end if;
   end process p_flash;

end architecture synthesis;
