----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- sd_dma: copies consecutive SD card blocks into memory through the SD controller's
-- engine port (sdhost.vhd), so that the Shell does not move a file byte by byte.
--
-- A transfer: `count` blocks from SD block `blk` to byte offset `dest` (even) of an
-- Avalon memory with 16-bit words, little endian (byte 2n in bits 7..0). Bytes at
-- offsets >= `length` are not written, so the tail of the file's last block does not
-- spill over the memory region. The blocks pass through a ring buffer of G_BUF_BLOCKS
-- blocks; the controller's limit keeps it from running ahead of the memory writes.
-- A QNICE SD command ends an engine transfer early; the rest is issued again.
--
-- Everything runs on the QNICE clock.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity sd_dma is
generic (
   G_BUF_BLOCKS : natural := 4                          -- power of two
);
port (
   clk_i             : in  std_logic;
   rst_i             : in  std_logic;

   start_i           : in  std_logic;                      -- pulse, while busy_o = '0'
   blk_i             : in  std_logic_vector(31 downto 0);  -- first SD block
   count_i           : in  std_logic_vector(15 downto 0);  -- number of blocks
   dest_i            : in  std_logic_vector(31 downto 0);  -- byte offset of the first block
   length_i          : in  std_logic_vector(31 downto 0);  -- bytes from here on are not written
   busy_o            : out std_logic;
   error_o           : out std_logic;                      -- the last transfer failed

   eng_o             : out t_sd_c2q;
   eng_i             : in  t_sd_q2c;

   -- word address relative to the region, single writes
   avm_write_o       : out std_logic;
   avm_address_o     : out std_logic_vector(31 downto 0);
   avm_writedata_o   : out std_logic_vector(15 downto 0);
   avm_byteenable_o  : out std_logic_vector( 1 downto 0);
   avm_waitrequest_i : in  std_logic
);
end entity sd_dma;

architecture synthesis of sd_dma is

   function ceil_log2(n : natural) return natural is
      variable r : natural := 0;
   begin
      while 2 ** r < n loop
         r := r + 1;
      end loop;
      return r;
   end function ceil_log2;

   constant C_SLOT_BITS : natural := ceil_log2(G_BUF_BLOCKS);

   type t_buf is array (0 to G_BUF_BLOCKS * 256 - 1) of std_logic_vector(15 downto 0);
   signal buf        : t_buf;

   signal busy       : std_logic := '0';
   signal err        : std_logic := '0';
   signal blk        : unsigned(31 downto 0);
   signal count      : unsigned(15 downto 0);
   signal dest_w     : unsigned(30 downto 0);                -- word offset of the first block
   signal length     : unsigned(31 downto 0);

   -- engine side
   signal eng_act    : std_logic := '0';                     -- a transfer runs
   signal eng_start  : std_logic := '0';
   signal xfer_base  : unsigned(15 downto 0);                -- first block of that transfer
   signal recv       : unsigned(15 downto 0);                -- blocks complete in the buffer
   signal lo_byte    : std_logic_vector(7 downto 0);
   signal buf_we     : std_logic := '0';
   signal buf_waddr  : unsigned(C_SLOT_BITS + 7 downto 0);
   signal buf_wdata  : std_logic_vector(15 downto 0);

   -- memory side
   type t_wstate is (W_IDLE, W_READ, W_WRITE);
   signal wstate     : t_wstate := W_IDLE;
   signal drained    : unsigned(15 downto 0);                -- blocks written to memory
   signal word       : unsigned(7 downto 0);                 -- word within the block
   signal buf_rdata  : std_logic_vector(15 downto 0);
   signal mem_write  : std_logic := '0';
   signal mem_addr   : unsigned(30 downto 0);
   signal mem_be     : std_logic_vector(1 downto 0);

begin

   busy_o  <= busy;
   error_o <= err;

   p_eng_o : process (all)
   begin
      eng_o            <= (others => '0');
      eng_o(C2Q_START) <= eng_start;
      eng_o(C2Q_ADDR)  <= std_logic_vector(blk + resize(xfer_base, 32));
      eng_o(C2Q_COUNT) <= std_logic_vector(count - xfer_base);
      -- blocks of this transfer that may be moved: as far as the buffer has room
      eng_o(C2Q_LIMIT) <= std_logic_vector(drained + G_BUF_BLOCKS - xfer_base);
   end process p_eng_o;

   avm_write_o      <= mem_write;
   avm_address_o    <= '0' & std_logic_vector(mem_addr);
   avm_writedata_o  <= buf_rdata;
   avm_byteenable_o <= mem_be;

   -- the ring buffer: written by the engine side, read by the memory side
   p_buf : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if buf_we = '1' then
            buf(to_integer(buf_waddr)) <= buf_wdata;
         end if;
         buf_rdata <= buf(to_integer(drained(C_SLOT_BITS - 1 downto 0) & word));
      end if;
   end process p_buf;

   p_engine : process (clk_i)
      variable b : unsigned(15 downto 0);
   begin
      if rising_edge(clk_i) then
         eng_start <= '0';
         buf_we    <= '0';

         if start_i = '1' and busy = '0' then
            busy      <= '1';
            err       <= '0';
            blk       <= unsigned(blk_i);
            count     <= unsigned(count_i);
            dest_w    <= unsigned(dest_i(31 downto 1));
            length    <= unsigned(length_i);
            recv      <= (others => '0');
            xfer_base <= (others => '0');
         end if;

         -- (re)issue the blocks that are still missing
         if busy = '1' and err = '0' and eng_act = '0' and eng_start = '0' and
            recv /= count and eng_i(Q2C_BUSY) = '0' then
            eng_start <= '1';
            eng_act   <= '1';
            xfer_base <= recv;
         end if;

         if eng_act = '1' then
            if eng_i(Q2C_RDWE) = '1' then
               if eng_i(Q2C_RDIDX)(Q2C_RDIDX'low) = '0' then
                  lo_byte <= eng_i(Q2C_RDDATA);
               else
                  b := xfer_base + unsigned(eng_i(Q2C_RDBLK));
                  buf_we    <= '1';
                  buf_waddr <= b(C_SLOT_BITS - 1 downto 0) & unsigned(eng_i(Q2C_RDIDX)(Q2C_RDIDX'high downto Q2C_RDIDX'low + 1));
                  buf_wdata <= eng_i(Q2C_RDDATA) & lo_byte;
               end if;
            end if;
            if eng_i(Q2C_BLK) = '1' then
               recv <= recv + 1;
            end if;
            if eng_i(Q2C_DONE) = '1' then
               eng_act <= '0';
               if eng_i(Q2C_ERROR) = '1' then
                  err <= '1';
               end if;
            end if;
         end if;

         -- finished: all blocks written, or failed and the memory side is idle
         if busy = '1' and eng_act = '0' and eng_start = '0' and wstate = W_IDLE and
            (drained = count or err = '1') then
            busy <= '0';
         end if;

         if rst_i = '1' then
            busy    <= '0';
            err     <= '0';
            eng_act <= '0';
         end if;
      end if;
   end process p_engine;

   p_memory : process (clk_i)
      variable ofs : unsigned(31 downto 0);
   begin
      if rising_edge(clk_i) then
         case wstate is
            when W_IDLE =>
               if start_i = '1' and busy = '0' then
                  drained <= (others => '0');
                  word    <= (others => '0');
               elsif busy = '1' and err = '0' and drained /= recv then
                  wstate <= W_READ;                -- buf_rdata is valid next clock
               end if;

            when W_READ =>
               -- byte offset of the word's low byte: skip what lies beyond the file
               ofs      := resize(dest_w + resize(drained & word, 31), 31) & '0';
               mem_addr <= dest_w + resize(drained & word, 31);
               if ofs + 1 < length then
                  mem_be    <= "11";
                  mem_write <= '1';
               elsif ofs < length then
                  mem_be    <= "01";
                  mem_write <= '1';
               end if;
               wstate <= W_WRITE;

            when W_WRITE =>
               if mem_write = '0' or avm_waitrequest_i = '0' then
                  mem_write <= '0';
                  word      <= word + 1;
                  if word = 255 then
                     drained <= drained + 1;
                  end if;
                  wstate <= W_IDLE;
               end if;
         end case;

         if rst_i = '1' then
            wstate    <= W_IDLE;
            mem_write <= '0';
            drained   <= (others => '0');
         end if;
      end if;
   end process p_memory;

end architecture synthesis;
