----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- cmdhd_scsi: the SCSI disk at ID 0 behind the CMD HD's controller, bus side (core clock).
--
-- The bus moves here, the commands are the firmware's (VICE's scsi.c ported to C):
--
--   * Selection: SEL with only bit 0 among the IDs (the initiator's own bit 7 aside) on the
--     data lines: BSY. When SEL drops: COMMAND phase, REQ.
--   * COMMAND: 6, 10 or 12 bytes by the opcode's group, into the buffer at C_CDB. Then REQ
--     drops and cmd_tgl_o flips: the firmware reads the bytes and answers with go_tgl_i.
--   * go: DATA IN (go_count_i bytes from go_base_i), DATA OUT (into go_base_i), or STATUS
--     (go_status_i). A data phase ends with REQ low and done_tgl_o flipped, and waits for
--     the next go; STATUS ends at bus free with free_tgl_o flipped (no MESSAGE IN, as VICE
--     does for the CMD HD).
--
-- REQ stays up for all the bytes of a phase, as in VICE: the DOS checks it before each
-- byte, and it waits (as long as BSY stays) while REQ is low, so the firmware may take as
-- long as it needs. ACK is the controller's VIA (U9) pulsing CA2 on each port A access.
-- ATN is ignored, as in VICE. RST from the controller ends everything at once.
--
-- go_* cross from the firmware's clock: the values are stable before go_tgl_i flips.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmdhd_scsi is
port (
   clk_i       : in  std_logic;
   rst_i       : in  std_logic;                     -- the drive's reset

   -- the bus, as the controller drives it (logical levels, 1 = asserted)
   sel_i       : in  std_logic;
   rst_bus_i   : in  std_logic;
   ack_i       : in  std_logic;                     -- one clock per CA2 pulse
   dbus_i      : in  std_logic_vector(7 downto 0);  -- what the controller puts on the data lines

   -- the bus, as this disk drives it
   bsy_o       : out std_logic;
   req_o       : out std_logic;
   io_o        : out std_logic;                     -- the disk drives the data lines
   phase_o     : out std_logic_vector(2 downto 0);  -- U13's code: 0 DATA OUT, 1 COMMAND, 4 DATA IN, 5 STATUS
   dbus_o      : out std_logic_vector(7 downto 0);

   -- the buffer (port A of a dual-clock RAM, read data one clock after the address)
   buf_addr_o  : out std_logic_vector(10 downto 0);
   buf_we_o    : out std_logic;
   buf_wdata_o : out std_logic_vector(7 downto 0);
   buf_rdata_i : in  std_logic_vector(7 downto 0);

   -- to the firmware
   cmd_tgl_o   : out std_logic;                     -- a command is in the buffer
   cdb_len_o   : out std_logic_vector(3 downto 0);  -- its length
   done_tgl_o  : out std_logic;                     -- a data phase moved all its bytes
   free_tgl_o  : out std_logic;                     -- bus free after STATUS, or a bus reset
   busy_o      : out std_logic;                     -- BSY

   -- from the firmware (its clock domain)
   go_tgl_i    : in  std_logic;
   go_phase_i  : in  std_logic_vector(1 downto 0);  -- 0 DATA IN, 1 DATA OUT, 2 STATUS
   go_count_i  : in  std_logic_vector(10 downto 0);
   go_base_i   : in  std_logic_vector(9 downto 0);
   go_status_i : in  std_logic_vector(7 downto 0)
);
end entity cmdhd_scsi;

architecture synthesis of cmdhd_scsi is

   constant C_CDB : unsigned(10 downto 0) := to_unsigned(1024, 11);

   type t_state is (S_FREE, S_SELECTED, S_CMD, S_WAIT, S_DIN, S_DOUT, S_STAT);
   signal state    : t_state := S_FREE;

   signal bsy, req, io : std_logic := '0';
   signal phase    : std_logic_vector(2 downto 0) := "101";
   signal n        : unsigned(3 downto 0) := (others => '0');   -- command byte
   signal len      : unsigned(3 downto 0) := to_unsigned(6, 4);
   signal p        : unsigned(10 downto 0) := (others => '0');  -- data byte
   signal cnt      : unsigned(10 downto 0) := (others => '0');
   signal base     : unsigned(9 downto 0) := (others => '0');
   signal status   : std_logic_vector(7 downto 0) := x"00";
   signal cmd_tgl, done_tgl, free_tgl : std_logic := '0';

   signal go_s     : std_logic_vector(2 downto 0) := "000";     -- go_tgl_i, 2 FFs + previous
   attribute async_reg : string;
   attribute async_reg of go_s : signal is "true";

begin

   bsy_o      <= bsy;
   req_o      <= req;
   io_o       <= io;
   phase_o    <= phase;
   dbus_o     <= status when state = S_STAT else buf_rdata_i;
   cmd_tgl_o  <= cmd_tgl;
   cdb_len_o  <= std_logic_vector(len);
   done_tgl_o <= done_tgl;
   free_tgl_o <= free_tgl;
   busy_o     <= bsy;

   buf_addr_o  <= std_logic_vector(C_CDB + resize(n, 11)) when state = S_CMD else
                  std_logic_vector(resize(base, 11) + p);
   buf_wdata_o <= dbus_i;
   buf_we_o    <= ack_i when state = S_CMD or state = S_DOUT else '0';

   p_bus : process (clk_i)
      variable l : unsigned(3 downto 0);
   begin
      if rising_edge(clk_i) then
         go_s <= go_s(1 downto 0) & go_tgl_i;

         case state is
            when S_FREE =>
               io    <= '0';
               req   <= '0';
               phase <= "101";
               if sel_i = '1' and (dbus_i and x"7F") = x"01" then
                  bsy   <= '1';
                  state <= S_SELECTED;
               end if;

            when S_SELECTED =>
               if sel_i = '0' then
                  n     <= (others => '0');
                  phase <= "001";
                  req   <= '1';
                  state <= S_CMD;
               end if;

            when S_CMD =>
               if ack_i = '1' then
                  l := len;
                  if n = 0 then
                     case dbus_i(7 downto 5) is
                        when "001" | "010" => l := to_unsigned(10, 4);
                        when "101"         => l := to_unsigned(12, 4);
                        when others        => l := to_unsigned(6, 4);
                     end case;
                     len <= l;
                  end if;
                  n <= n + 1;
                  if n + 1 = l then
                     req     <= '0';
                     cmd_tgl <= not cmd_tgl;
                     state   <= S_WAIT;
                  end if;
               end if;

            when S_WAIT =>
               if go_s(2) /= go_s(1) then
                  p    <= (others => '0');
                  cnt  <= unsigned(go_count_i);
                  base <= unsigned(go_base_i);
                  req  <= '1';
                  case go_phase_i is
                     when "00" =>
                        io    <= '1';
                        phase <= "100";
                        state <= S_DIN;
                     when "01" =>
                        io    <= '0';
                        phase <= "000";
                        state <= S_DOUT;
                     when others =>
                        status <= go_status_i;
                        io     <= '1';
                        phase  <= "101";
                        state  <= S_STAT;
                  end case;
               end if;

            when S_DIN | S_DOUT =>
               if ack_i = '1' then
                  p <= p + 1;
                  if p + 1 >= cnt then
                     req      <= '0';
                     done_tgl <= not done_tgl;
                     state    <= S_WAIT;
                  end if;
               end if;

            when S_STAT =>
               if ack_i = '1' then
                  bsy      <= '0';
                  req      <= '0';
                  io       <= '0';
                  free_tgl <= not free_tgl;
                  state    <= S_FREE;
               end if;
         end case;

         if rst_bus_i = '1' or rst_i = '1' then
            if state /= S_FREE and state /= S_SELECTED then
               free_tgl <= not free_tgl;
            end if;
            bsy   <= '0';
            req   <= '0';
            io    <= '0';
            phase <= "101";
            state <= S_FREE;
         end if;
      end if;
   end process p_bus;

end architecture synthesis;
