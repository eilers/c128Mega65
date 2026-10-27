----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- HyperRAM for the RISC-V system CPU (rv_soc.vhd). CPU byte address A is HyperRAM
-- word A/2, low byte first, so one CPU word is two HyperRAM words.
--
-- CPU side: NEORV32 XBUS (pipelined Wishbone). A cache line fill (cti "010" on its
-- first request) is read in bursts of 16 HyperRAM words, one after the other, so the
-- HDMI scaler gets its turns (doc/swiftlink-modem.md, "Short HyperRAM bursts"). The
-- cache's further requests, each for the next word, are answered in order. Other
-- reads fetch their CPU word; writes write the halves their byte enables select. The
-- CPU has no d-cache, so writes come one at a time.
-- HyperRAM side: Avalon master on the same clock; the framework's avm_fifo crosses to
-- the HyperRAM clock.
--
-- C128 core: RISC-V replacement for QNICE, licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rv_hr_bridge is
   generic (
      G_LINE_BYTES : natural                                -- NEORV32 cache line size
   );
   port (
      clk_i                 : in  std_logic;
      rst_i                 : in  std_logic;

      -- XBUS: this bridge's requests only
      stb_i                 : in  std_logic;
      adr_i                 : in  std_logic_vector(23 downto 0);    -- byte address in HyperRAM
      we_i                  : in  std_logic;
      sel_i                 : in  std_logic_vector(3 downto 0);
      dat_i                 : in  std_logic_vector(31 downto 0);
      cti_i                 : in  std_logic_vector(2 downto 0);
      dat_o                 : out std_logic_vector(31 downto 0);
      ack_o                 : out std_logic;
      err_o                 : out std_logic;

      -- HyperRAM
      avm_write_o           : out std_logic;
      avm_read_o            : out std_logic;
      avm_address_o         : out std_logic_vector(31 downto 0);
      avm_writedata_o       : out std_logic_vector(15 downto 0);
      avm_byteenable_o      : out std_logic_vector(1 downto 0);
      avm_burstcount_o      : out std_logic_vector(7 downto 0);
      avm_readdata_i        : in  std_logic_vector(15 downto 0);
      avm_readdatavalid_i   : in  std_logic;
      avm_waitrequest_i     : in  std_logic
   );
end entity rv_hr_bridge;

architecture synthesis of rv_hr_bridge is

   constant C_LINE_WORDS : natural := G_LINE_BYTES / 4;      -- CPU words per line
   constant C_BURST      : natural := 16;                    -- HyperRAM words per burst

   type t_state is (S_IDLE, S_READ, S_WRITE);
   type t_words is array (0 to C_LINE_WORDS - 1) of std_logic_vector(31 downto 0);

   signal state       : t_state := S_IDLE;

   -- Read: HyperRAM words still to come, in all and in this burst
   signal rd_left     : natural range 0 to 2 * C_LINE_WORDS := 0;
   signal burst_left  : natural range 0 to C_BURST := 0;
   signal half        : std_logic := '0';                    -- the low half is in lo_word
   signal lo_word     : std_logic_vector(15 downto 0);

   -- Read: CPU words that came in and wait for their request
   signal words       : t_words;
   signal wr_ptr      : natural range 0 to C_LINE_WORDS - 1 := 0;
   signal rd_ptr      : natural range 0 to C_LINE_WORDS - 1 := 0;
   signal count       : natural range 0 to C_LINE_WORDS := 0;

   -- Read: the CPU's requests
   signal requested   : natural range 0 to C_LINE_WORDS := 0; -- asked for, not yet answered
   signal expect      : natural range 0 to C_LINE_WORDS := 0; -- still to answer
   signal beats_left  : natural range 0 to C_LINE_WORDS := 0; -- line requests still to come
   signal next_adr    : unsigned(23 downto 0);

   -- Write: the high half is still to go
   signal wr_hi       : std_logic := '0';
   signal wr_data_hi  : std_logic_vector(15 downto 0);
   signal wr_be_hi    : std_logic_vector(1 downto 0);

   -- After a reset, read data of a burst from before it is dropped. A request that comes
   -- meanwhile waits for it (XBUS has no stall); the CPU sends no other until it's answered.
   signal settle      : natural range 0 to 63 := 63;
   signal held        : std_logic := '0';
   signal held_adr    : std_logic_vector(23 downto 0);
   signal held_we     : std_logic;
   signal held_sel    : std_logic_vector(3 downto 0);
   signal held_dat    : std_logic_vector(31 downto 0);
   signal held_cti    : std_logic_vector(2 downto 0);

begin

   p_bridge : process (clk_i)
      variable v_requested : natural range 0 to C_LINE_WORDS;
      variable v_count     : natural range 0 to C_LINE_WORDS;
      variable v_expect    : natural range 0 to C_LINE_WORDS;
      variable v_words     : natural range 0 to 2 * C_LINE_WORDS;
      variable v_stb       : std_logic;
      variable v_adr       : std_logic_vector(23 downto 0);
      variable v_we        : std_logic;
      variable v_sel       : std_logic_vector(3 downto 0);
      variable v_dat       : std_logic_vector(31 downto 0);
      variable v_cti       : std_logic_vector(2 downto 0);
   begin
      if rising_edge(clk_i) then
         ack_o       <= '0';
         err_o       <= '0';
         v_requested := requested;
         v_count     := count;
         v_expect    := expect;

         if avm_waitrequest_i = '0' then
            avm_read_o <= '0';
         end if;

         if avm_readdatavalid_i = '1' and rd_left = 0 then
            settle <= 63;
         elsif settle > 0 then
            settle <= settle - 1;
         end if;

         -- HyperRAM read data
         if avm_readdatavalid_i = '1' and rd_left > 0 then
            if half = '0' then
               lo_word <= avm_readdata_i;
               half    <= '1';
            else
               words(wr_ptr) <= avm_readdata_i & lo_word;
               wr_ptr        <= (wr_ptr + 1) mod C_LINE_WORDS;
               v_count       := v_count + 1;
               half          <= '0';
            end if;
            rd_left <= rd_left - 1;
            if burst_left = 1 and rd_left > 1 then
               -- The next burst of the line, once this one is in
               avm_read_o       <= '1';
               avm_address_o    <= std_logic_vector(unsigned(avm_address_o) + C_BURST);
               avm_burstcount_o <= std_logic_vector(to_unsigned(minimum(C_BURST, rd_left - 1), 8));
               burst_left       <= minimum(C_BURST, rd_left - 1);
            else
               burst_left <= burst_left - 1;
            end if;
         end if;

         -- HyperRAM writes
         if state = S_WRITE and avm_waitrequest_i = '0' then
            if wr_hi = '1' then
               avm_writedata_o  <= wr_data_hi;
               avm_byteenable_o <= wr_be_hi;
               wr_hi            <= '0';
            else
               avm_write_o <= '0';
               ack_o       <= '1';
               state       <= S_IDLE;
            end if;
         end if;

         -- CPU requests, or the one held while settling
         if held = '1' then
            v_stb := '1';
            v_adr := held_adr;
            v_we  := held_we;
            v_sel := held_sel;
            v_dat := held_dat;
            v_cti := held_cti;
         else
            v_stb := stb_i;
            v_adr := adr_i;
            v_we  := we_i;
            v_sel := sel_i;
            v_dat := dat_i;
            v_cti := cti_i;
         end if;

         if v_stb = '1' then
            held <= '0';
            if state = S_READ and v_we = '0' and beats_left > 0 and unsigned(v_adr) = next_adr then
               -- The next word of the line being filled
               v_requested := v_requested + 1;
               beats_left  <= beats_left - 1;
               next_adr    <= next_adr + 4;
            elsif state = S_IDLE and settle > 0 then
               held     <= '1';
               held_adr <= v_adr;
               held_we  <= v_we;
               held_sel <= v_sel;
               held_dat <= v_dat;
               held_cti <= v_cti;
            elsif state = S_IDLE and v_we = '0' and
                  (v_cti /= "010" or (unsigned(v_adr) and to_unsigned(G_LINE_BYTES - 1, 24)) = 0) then
               -- A line fill starts at the line's start: a stray word from the middle of
               -- one gets an error below, so it can't leave the bridge waiting for the rest
               if v_cti = "010" then
                  v_words    := 2 * C_LINE_WORDS;
                  v_expect   := C_LINE_WORDS;
                  beats_left <= C_LINE_WORDS - 1;
               else
                  v_words    := 2;
                  v_expect   := 1;
                  beats_left <= 0;
               end if;
               v_requested      := 1;
               next_adr         <= (unsigned(v_adr(23 downto 2)) & "00") + 4;
               rd_left          <= v_words;
               burst_left       <= minimum(C_BURST, v_words);
               half             <= '0';
               avm_read_o       <= '1';
               avm_address_o    <= x"00" & '0' & v_adr(23 downto 2) & '0';
               avm_burstcount_o <= std_logic_vector(to_unsigned(minimum(C_BURST, v_words), 8));
               state            <= S_READ;
            elsif state = S_IDLE and v_we = '1' and v_sel /= "0000" then
               -- One word or two, depending on the byte enables
               avm_write_o <= '1';
               state       <= S_WRITE;
               wr_data_hi  <= v_dat(31 downto 16);
               wr_be_hi    <= v_sel(3 downto 2);
               if v_sel(1 downto 0) = "00" then
                  avm_address_o    <= x"00" & '0' & v_adr(23 downto 2) & '1';
                  avm_writedata_o  <= v_dat(31 downto 16);
                  avm_byteenable_o <= v_sel(3 downto 2);
                  avm_burstcount_o <= x"01";
                  wr_hi            <= '0';
               else
                  avm_address_o    <= x"00" & '0' & v_adr(23 downto 2) & '0';
                  avm_writedata_o  <= v_dat(15 downto 0);
                  avm_byteenable_o <= v_sel(1 downto 0);
                  if v_sel(3 downto 2) = "00" then
                     avm_burstcount_o <= x"01";
                     wr_hi            <= '0';
                  else
                     avm_burstcount_o <= x"02";
                     wr_hi            <= '1';
                  end if;
               end if;
            else
               -- Overlapping requests, a stray line word, or a write with no byte enabled
               ack_o <= '1';
               err_o <= '1';
            end if;
         end if;

         -- Answer the oldest request whose word came in before this clock
         if v_requested > 0 and count > 0 then
            ack_o       <= '1';
            dat_o       <= words(rd_ptr);
            rd_ptr      <= (rd_ptr + 1) mod C_LINE_WORDS;
            v_count     := v_count - 1;
            v_requested := v_requested - 1;
            v_expect    := v_expect - 1;
            if v_expect = 0 then
               state <= S_IDLE;
            end if;
         end if;

         requested <= v_requested;
         count     <= v_count;
         expect    <= v_expect;

         if rst_i = '1' then
            state       <= S_IDLE;
            avm_read_o  <= '0';
            avm_write_o <= '0';
            rd_left     <= 0;
            burst_left  <= 0;
            wr_ptr      <= 0;
            rd_ptr      <= 0;
            count       <= 0;
            requested   <= 0;
            expect      <= 0;
            beats_left  <= 0;
            wr_hi       <= '0';
            settle      <= 63;
            held        <= '0';
         end if;
      end if;
   end process p_bridge;

end architecture synthesis;
