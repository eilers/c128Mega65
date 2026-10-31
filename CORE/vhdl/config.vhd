----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- Configuration data for the Shell
--
-- MiSTer2MEGA65 done by sy2002 and MJoergen in 2023 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.globals.all;

entity config is
generic (
   G_R3_MEMORY : boolean := false   -- no 2 MB or 16 MB REU: the menu leaves out those items
);
port (
   clk_i       : in std_logic;

   -- bits 27 .. 12:    select configuration data block; called "Selector" hereafter
   -- bits 11 downto 0: address the up to 4k the configuration data
   address_i   : in std_logic_vector(27 downto 0);

   -- config data
   data_o      : out std_logic_vector(15 downto 0)
);
end entity config;

architecture beh of config is

--------------------------------------------------------------------------------------------------------------------
-- String and character constants (specific for the Anikki-16x16 font)
--------------------------------------------------------------------------------------------------------------------

-- !!! DO NOT TOUCH !!!
constant CHR_LINE_1  : character := character'val(196);
constant CHR_LINE_5  : string := CHR_LINE_1 & CHR_LINE_1 & CHR_LINE_1 & CHR_LINE_1 & CHR_LINE_1;
constant CHR_LINE_10 : string := CHR_LINE_5 & CHR_LINE_5;
constant CHR_LINE_50 : string := CHR_LINE_10 & CHR_LINE_10 & CHR_LINE_10 & CHR_LINE_10 & CHR_LINE_10;

--------------------------------------------------------------------------------------------------------------------
-- Welcome and Help Screens (Selectors 0x1000 .. 0x1FFF)
--------------------------------------------------------------------------------------------------------------------

-- define the amount of WHS array elements: between 1 and 16
constant WHS_RECORDS   : natural := 2;

-- define the maximum amount of pages per WHS array element: between 1 and 256
-- (this is necessary because Vivado does not support unconstrained arrays in a record)
constant WHS_MAX_PAGES : natural := 3;

 -- !!! DO NOT TOUCH !!!
constant SEL_WHS           : std_logic_vector(15 downto 0) := x"1000";
type WHS_INDEX_TYPE is array (0 to WHS_MAX_PAGES - 1) of natural;
type WHS_RECORD_TYPE is record
   page_count  : natural;
   page_start  : WHS_INDEX_TYPE;
   page_length : WHS_INDEX_TYPE;
end record;
type WHS_RECORD_ARRAY_TYPE is array (0 to WHS_RECORDS - 1) of WHS_RECORD_TYPE;

-- START YOUR CONFIGURATION BELOW THIS LINE

-- Define all your screens as string constants. They will be synthesized as ROMs.
-- You can name these string constants as you want to, as long as you make them part of the WHS array (see below).
--
-- WHS array position 0 is defined as the "Welcome Screen" as controled by WELCOME_ACTIVE and WELCOME_AT_RESET.
-- If you are not using a Welcome Screen but only Help menu items, then you need to leave WHS array pos. 0 empty.
--
-- WHS array position 1 and onwards is for all the Option Menu items tagged as "Help": The first one in the
-- Options menu is WHS array pos. 1, the second one in the menu is WHS array pos. 2 and so on.
--
-- Maximum 16 WHS array positions: The selector's bits 11 downto 8 select the WHS array position; 0=Welcome Screen
-- That means a maximum of 15 menu items in the Options menu can be tagged as "Help"
-- The selector's bits 7 downto 0 are selecting the page within the WHS array, so maximum 256 pages per Welcome Screen or Help menu item
--
-- Within a selector's address range, address 0 is the beginning of the string itself, while address 0xFFF of the 4k
-- window contains the amount of pages, so each zero-terminated string can be up to 4095 bytes = 4094 characters long.

-- No Welcome Screen is used (WELCOME_ACTIVE is false), therefore WHS array position 0 stays empty.

-- Version of this core. It ends up inside a FAT32 path (see CFG_FILE), so keep it to characters
-- that are legal there and use "_" instead of spaces. CORE/scripts/build_release.sh refuses to
-- build unless this matches the release name that is being packaged.
constant CORE_VERSION : string := "V1.1_Beta1";

constant SCR_CREDITS : string :=

   "\n Commodore 128 for MEGA65 " & CORE_VERSION & "\n\n" &
   " Core port to the MEGA65\n" &
   "   Stefan Eilers\n\n" &
   " Special thanks for contributions:\n" &
   "   Jeff Flitton\n\n" &
   " MiSTer C128 core\n" &
   "   Erik Scheffers, based on C64_MiSTer\n" &
   "   by sorgelig and on FPGA64 by\n" &
   "   Peter Wendrich\n\n" &

   -- We are not insisting. But it would be nice if you gave us credit for MiSTer2MEGA65 by leaving these lines in
   " MiSTer2MEGA65 framework\n" &
   "   sy2002 and MJoergen\n\n" &

   " Licensed under GPL v3\n\n" &
   " ROM files required on SD card:\n" &
   "   /c128/boot0.rom\n" &
   "   /c128/boot1.rom\n\n" &
   " Press Space to close this screen.";


-- Concatenate all your Welcome and Help screens into one large string, so that during synthesis one large string ROM can be build.
constant WHS_DATA : string := SCR_CREDITS;

-- The WHS array needs the start address of each page. As a best practice: Just define some constants, that you can name for example
-- just like you named the string constants and then add _START. Use the 'length attribute of VHDL to add up all previous strings
-- so that the Synthesis tool can calculate the start addresses: Your first string starts at zero, your next one at the address which
-- is equal to the length of the first one, your next one at the address which is equal to the sum of the previous ones, and so on.
constant SCR_CREDITS_START : natural := 0;

-- Fill the WHS array with page start addresses and the length of each page.
-- Make sure that array element 0 is always your Welcome page. If you don't use a welcome page, fill everything with zeros.
constant WHS : WHS_RECORD_ARRAY_TYPE := (
   --- Welcome Screen: not used
   (page_count    => 0,
    page_start    => (0, 0, 0),
    page_length   => (0, 0, 0)),

   --- Help pages
   (page_count    => 1,
    page_start    => (SCR_CREDITS_START,  0, 0),
    page_length   => (SCR_CREDITS'length, 0, 0))
);

--------------------------------------------------------------------------------------------------------------------
-- Set start folder for file browser and specify config file for menu persistence (Selectors 0x0100 and 0x0101)
--------------------------------------------------------------------------------------------------------------------

-- !!! DO NOT TOUCH !!!
constant SEL_DIR_START     : std_logic_vector(15 downto 0) := x"0100";
constant SEL_CFG_FILE      : std_logic_vector(15 downto 0) := x"0101";

-- START YOUR CONFIGURATION BELOW THIS LINE

-- The file browser opens here, and this is where the ROM files and the disk images live.
constant DIR_START         : string := "/c128";

-- The saved-settings file carries the core version, so that a menu layout change automatically
-- retires the old file instead of loading it with a mismatched length. See doc/KNOWN_BUGS.md.
-- R3-memory builds have two menu items fewer, so they have their own file.
function cfg_file_name return string is
begin
   if G_R3_MEMORY then
      return "/c128/c128mega65-" & CORE_VERSION & "-r3.cfg";
   end if;
   return "/c128/c128mega65-" & CORE_VERSION & ".cfg";
end function cfg_file_name;

constant CFG_FILE          : string := cfg_file_name;

--------------------------------------------------------------------------------------------------------------------
-- General configuration settings: Reset, Pause, OSD behavior, Ascal, etc. (Selector 0x0110)
--------------------------------------------------------------------------------------------------------------------

constant SEL_GENERAL       : std_logic_vector(15 downto 0) := x"0110";  -- !!! DO NOT TOUCH !!!

-- START YOUR CONFIGURATION BELOW THIS LINE

-- at a minimum, keep the reset line active for this amount of "QNICE loops" (see gencfg.asm).
-- "0" means: deactivate this feature
constant RESET_COUNTER     : natural := 100;

-- put the core in PAUSE state if any OSD opens
constant OPTM_PAUSE        : boolean := false;

-- show the welcome screen in general
constant WELCOME_ACTIVE    : boolean := false;

-- shall the welcome screen also be shown after the core is reset?
-- (only relevant if WELCOME_ACTIVE is true)
constant WELCOME_AT_RESET  : boolean := false;

-- keyboard and joystick connection during reset and OSD
constant KEYBOARD_AT_RESET : boolean := false;
constant JOY_1_AT_RESET    : boolean := false;
constant JOY_2_AT_RESET    : boolean := false;

constant KEYBOARD_AT_OSD   : boolean := false;
constant JOY_1_AT_OSD      : boolean := false;
constant JOY_2_AT_OSD      : boolean := false;

-- Avalon Scaler settings (see ascal.vhd, used for HDMI output only)
-- 0=set ascal mode (via QNICE's ascal_mode_o) to the value of the config.vhd constant ASCAL_MODE
-- 1=do nothing, leave ascal mode alone, custom QNICE assembly code can still change it via M2M$ASCAL_MODE
--               and QNICE's CSR will be set to not automatically sync ascal_mode_i
-- 2=keep ascal mode in sync with the QNICE input register ascal_mode_i:
--   use this if you want to control the ascal mode for example via the Options menu
--   where you would wire the output of certain options menu bits with ascal_mode_i
constant ASCAL_USAGE       : natural := 2;
constant ASCAL_MODE        : natural := 0;   -- see ascal.vhd for the meaning of this value

-- Save on-screen-display settings if the file specified by CFG_FILE exists and if it has
-- the length of OPTM_SIZE bytes. If the first byte of the file has the value 0xFF then it
-- is considered as "default", i.e. the menu items specified by OPTM_G_STDSEL are selected.
-- If the file does not exists, then settings are not saved and OPTM_G_STDSEL always denotes the standard settings.
constant SAVE_SETTINGS     : boolean := true;

-- Delay in ms between the last write request to a virtual drive from the core and the start of the
-- cache flushing (i.e. writing to the SD card). Since every new write from the core invalidates the cache,
-- and therefore leads to a completely new writing of the cache (flushing), this constant prevents thrashing.
-- The default is 2 seconds (2000 ms). Should be reasonable for many systems, but if you have a very fast
-- or very slow system, you might need to change this constant.
--
-- Constraint (@TODO): Currently we have only one constant for all virtual drives, i.e. the delay is
-- the same for all virtual drives. This might be absolutely OK; future will tell. If we need to have
-- more flexibility: vdrives.vhd already supports one delay per virtual drive. All what would need
-- to be done in such a case is: Enhance config.vhd to have more constants plus enhance the initialization
-- routine VD_INIT in vdrives.asm (tagged by @TODO) to store different values in the appropriate registers.
constant VD_ANTI_THRASHING_DELAY : natural := 2000;

-- Amount of bytes saved in one iteration of the background saving (buffer flushing) process
-- Constraint (@TODO): Similar constraint as in VD_ANTI_THRASHING_DELAY: Only one value for all drives.
-- shell.asm and shell_vars.asm already supports distinct values per drive; config.vhd and VD_INIT would
-- needs to be updated in case we would need this feature in future
constant VD_ITERATION_SIZE       : natural := 100;

--------------------------------------------------------------------------------------------------------------------
-- Name and version of the core  (Selector 0x0200)
--------------------------------------------------------------------------------------------------------------------

-- !!! DO NOT TOUCH !!!
constant SEL_CORENAME      : std_logic_vector(15 downto 0) := x"0200";

-- START YOUR CONFIGURATION BELOW THIS LINE

-- Currently this is only used in the debug console. Use the welcome screen and the
-- help system to display the name and version of your core to the end user
constant CORENAME          : string := "C128MEGA65 R6 WIP";

--------------------------------------------------------------------------------------------------------------------
-- "Help" menu / Options menu  (Selectors 0x0300 .. 0x0312): DO NOT TOUCH
--------------------------------------------------------------------------------------------------------------------

-- !!! DO NOT TOUCH !!! Selectors for accessing the menu configuration data
constant SEL_OPTM_ITEMS       : std_logic_vector(15 downto 0) := x"0300";
constant SEL_OPTM_GROUPS      : std_logic_vector(15 downto 0) := x"0301";
constant SEL_OPTM_STDSEL      : std_logic_vector(15 downto 0) := x"0302";
constant SEL_OPTM_LINES       : std_logic_vector(15 downto 0) := x"0303";
constant SEL_OPTM_START       : std_logic_vector(15 downto 0) := x"0304";
constant SEL_OPTM_ICOUNT      : std_logic_vector(15 downto 0) := x"0305";
constant SEL_OPTM_MOUNT_DRV   : std_logic_vector(15 downto 0) := x"0306";
constant SEL_OPTM_SINGLESEL   : std_logic_vector(15 downto 0) := x"0307";
constant SEL_OPTM_MOUNT_STR   : std_logic_vector(15 downto 0) := x"0308";
constant SEL_OPTM_DIMENSIONS  : std_logic_vector(15 downto 0) := x"0309";
constant SEL_OPTM_SAVING_STR  : std_logic_vector(15 downto 0) := x"030A";
constant SEL_OPTM_HELP        : std_logic_vector(15 downto 0) := x"0310";
constant SEL_OPTM_CRTROM      : std_logic_vector(15 downto 0) := x"0311";
constant SEL_OPTM_CRTROM_STR  : std_logic_vector(15 downto 0) := x"0312";

-- !!! DO NOT TOUCH !!! Configuration constants for OPTM_GROUPS (shell.asm and menu.asm expect them to be like this)
constant OPTM_G_TEXT       : integer := 16#00000#;         -- text that cannot be selected
constant OPTM_G_CLOSE      : integer := 16#000FF#;        -- menu items that closes menu
constant OPTM_G_STDSEL     : integer := 16#00100#;        -- item within a group that is selected by default
constant OPTM_G_LINE       : integer := 16#00200#;        -- draw a line at this position
constant OPTM_G_START      : integer := 16#00400#;        -- selector / cursor position after startup (only use once!)
                                                          -- 16#00800# is used in OPTM_G_MOUNT_DRV (OPTM_G_SINGLESEL)
constant OPTM_G_HEADLINE   : integer := 16#01000#;        -- like OPTM_G_TEXT but will be shown in a brigher color
                                                          -- 16#02000# is used in OPTM_G_HELP (plus OPTM_G_SINGLESEL)
                                                          -- 16#04000# is used in OPTM_G_SUBMENU
constant OPTM_G_SINGLESEL  : integer := 16#08000#;        -- single select item
constant OPTM_G_MOUNT_DRV  : integer := 16#08800#;        -- line item means: mount drive; first occurance = drive 0, second = drive 1, ...
constant OPTM_G_HELP       : integer := 16#0A000#;        -- line item means: help screen; first occurance = WHS(1), second = WHS(2), ...
constant OPTM_G_SUBMENU    : integer := 16#0C000#;        -- starts/ends a section that is treated as submenu
constant OPTM_G_LOAD_ROM   : integer := 16#18000#;        -- line item means: load ROM; first occurance = rom 0, second = rom 1, ...

constant OPTM_GTC          : natural := 17;                -- Amount of significant bits in OPTM_G_* constants

-- @TODO/REMINDER: If we added in future more configuration constants that are not meant to be saved in the
-- configuration file, such as OPTM_G_MOUNT_DRV and OPTM_G_LOAD_ROM, then we need to make sure that we
-- also extend _ROSMS_4A and _ROSMC_NEXTBIT in options.asm accordingly.
-- Also: Right now OPTM_G_SUBMENU cannot have a "selected" state (and therefore cannot be saved in the config file)
-- and therefore _ROSMS_4A and _ROSMC_NEXTBIT are not yet handling the situation. If we decided to change that in future,
-- we would need to define the right semantics everywhere.

--------------------------------------------------------------------------------------------------------------------
-- "Help" menu / Options menu: START YOUR CONFIGURATION BELOW THIS LINE
--------------------------------------------------------------------------------------------------------------------

-- Strings with which %s will be replaced in case the menu item is of type OPTM_G_MOUNT_DRV
constant OPTM_S_MOUNT      : string := "<Mount Drive>";     -- no disk image mounted, yet
constant OPTM_S_CRTROM     : string := "<Load>";            -- no ROM loaded, yet
constant OPTM_S_SAVING     : string := "<Saving>";          -- the internal write cache is dirty and not yet written back to the SD card

-- Size of menu and menu items
-- CAUTION: 1. End each line (also the last one) with a \n and make sure empty lines / separator lines are only consisting of a "\n"
--             Do use a lower case \n. If you forget one of them or if you use upper case, you will run into undefined behavior.
--          2. Start each line that contains an actual menu item (multi- or single-select) with a Space character,
--             otherwise you will experience visual glitches.
constant OPTM_SIZE_ALL     : natural := 177; -- amount of items including empty lines:
                                             -- needs to be equal to the number of lines in OPTM_ITEMS_ALL and amount of items in OPTM_GROUPS_ALL
                                             -- IMPORTANT: If SAVE_SETTINGS is true and OPTM_SIZE changes: Make sure to re-generate and
                                             -- and re-distribute the config file. You can make a new one using M2M/tools/make_config.sh
                                             -- The firmware takes at most 254 items (MENU_MAX in M2M/firmware/fw/menu.h).

-- Net size of the Options menu on the screen in characters (excluding the frame, which is hardcoded to two characters).
-- One height is used for the main menu and every submenu, so this must be the
-- tallest page. The main menu is the tallest at 31 rows. The Video submenu is
-- 20 rows and the HDMI submenu 14 (headline through Back to main menu; the "%s"
-- opener is hidden inside the submenu); both leave empty space below.
-- The screen is CHARS_DY = 33 rows tall and the frame adds two, so OPTM_DY must
-- stay at or below 31.
constant OPTM_DX           : natural := 23;
constant OPTM_DY           : natural := 31;

-- R3-memory builds leave out C_MENU_R3_DROP items from C_MENU_REU_2M on, and OPTM_ITEMS and
-- OPTM_GROUPS are the menu without them
constant OPTM_SIZE         : natural := OPTM_SIZE_ALL - C_MENU_R3_DROP * boolean'pos(G_R3_MEMORY);

constant OPTM_ITEMS_ALL    : string :=

   " Drives\n"              &
   "\n"                     &
   " %s\n"                  &    -- drive A: its ID:model and image (core.c); its submenu has the disk,
   " Drive A\n"            &    -- bus ID, model and ROMs, as Gideon's Drive A Settings
   "\n"                     &
   " Disk:%s\n"             &    -- OPTM_S_MOUNT or the image; Enter picks one, Space takes it out
   "\n"                     &
   " Bus ID\n"              &    -- its ID jumpers: a change resets the drive
   " 8\n"                   &
   " 9\n"                   &
   " 10\n"                  &
   " 11\n"                  &
   "\n"                     &
   " Model\n"               &    -- a D71 or D81, or a D64 in a 1581, runs as fits (core.c)
   " 1541\n"                &
   " 1571\n"                &
   " 1581\n"                &
   " Internal 1581\n"       &    -- the MEGA65's floppy drive, a 1581 (one drive at a time)
   "\n"                     &
   " 1541 ROM:%s\n"         &    -- 16 or 32 KB, Gideon's "ROM for 1541 mode"
   " 1571 ROM:%s\n"         &
   " 1581 ROM:%s\n"         &    -- also the internal floppy's
   " Space: back to stock\n" &
   "\n"                     &
   " Back to main menu\n"   &
   " %s\n"                  &    -- drive B: its ID:model and image (core.c); its submenu has the disk,
   " Drive B\n"            &    -- bus ID, model and ROMs, as Gideon's Drive B Settings
   "\n"                     &
   " Disk:%s\n"             &    -- OPTM_S_MOUNT or the image; Enter picks one, Space takes it out
   "\n"                     &
   " Bus ID\n"              &    -- its ID jumpers: a change resets the drive
   " 8\n"                   &
   " 9\n"                   &
   " 10\n"                  &
   " 11\n"                  &
   "\n"                     &
   " Model\n"               &    -- a D71 or D81, or a D64 in a 1581, runs as fits (core.c)
   " 1541\n"                &
   " 1571\n"                &
   " 1581\n"                &
   " Internal 1581\n"       &    -- the MEGA65's floppy drive, a 1581 (one drive at a time)
   "\n"                     &
   " 1541 ROM:%s\n"         &    -- 16 or 32 KB, Gideon's "ROM for 1541 mode"
   " 1571 ROM:%s\n"         &
   " 1581 ROM:%s\n"         &    -- also the internal floppy's
   " Space: back to stock\n" &
   "\n"                     &
   " Back to main menu\n"   &
   "\n"                     &

   -- Display Settings follows the C64 core: one submenu per output, each
   -- showing its current choice on the main menu.
   " Display Settings\n"    &
   "\n"                     &

   " Video: %s\n"           &    -- video submenu; %s is the Video Output choice
   " Video Output\n"        &
   "\n"                     &
   " Follow 40/80\n"        &    -- the first three drive both ports
   " 40 columns\n"          &
   " 80 columns\n"          &
   "\n"                     &
   " Dual screen\n"         &
   "\n"                     &
   " HDMI 40, VGA 80\n"     &    -- longest summary: " Video: HDMI 40, VGA 80" is exactly OPTM_DX
   " HDMI 80, VGA 40\n"     &
   " Swap with 40/80\n"     &    -- HDMI follows the 40/80 key, VGA shows the other chip
   "\n"                     &
   " VIC-II Jailbars\n"     &
   "\n"                     &
   " Off\n"                 &
   " Low\n"                 &
   " Medium\n"              &
   " High\n"                &
   "\n"                     &
   " Back to main menu\n"   &

   " HDMI: %s\n"            &    -- HDMI submenu; %s is the HDMI mode
   " HDMI Display Mode\n"   &
   "\n"                     &
   " 720p 50 Hz 16:9\n"     &
   " 720p 60 Hz 16:9\n"     &
   " 576p 50 Hz 4:3\n"      &
   " 576p 50 Hz 5:4\n"      &
   " 640x480 60 Hz\n"       &
   " 720x480 59.94 Hz\n"    &    -- longest summary: " HDMI: 720x480 59.94 Hz" is exactly OPTM_DX
   " 800x600 60 Hz\n"       &
   "\n"                     &
   " Flicker-free\n"        &
   " HDMI audio\n"          &    -- default on; off means DVI, no HDMI audio
   "\n"                     &
   " Back to main menu\n"   &

   " HDMI: CRT emulation\n" &

   " VGA: %s\n"             &    -- VGA submenu; %s is the VGA mode
   " VGA Display Mode\n"    &
   "\n"                     &
   " Standard\n"            &    -- scandoubled ~31 kHz, separate HS/VS
   "\n"                     &
   " Retro 15 kHz mode\n"   &
   "\n"                     &
   " 15 kHz with HS/VS\n"   &
   " 15 kHz with CSYNC\n"   &    -- composite sync for RGB monitors and SCART
   "\n"                     &
   " Back to main menu\n"   &
   "\n"                     &

   " Keyboard\n"            &
   "\n"                     &
   " Caps Lock: %s\n"       &    -- Caps Lock submenu; %s is 40/80 or ASCII/DIN
   " Caps Lock Key\n"       &
   "\n"                     &
   " 40/80\n"               &    -- default: real Caps Lock is the 40/80 key, F9 latches Caps Lock
   " ASCII/DIN\n"           &    -- real Caps Lock is ASCII/DIN, F9 latches 40/80
   "\n"                     &
   " F9 toggles the other\n" &
   "\n"                     &
   " Back to main menu\n"   &
   " Help menu on F13\n"    &    -- default off: Help opens the menu

   "\n"                     &
   " External Hardware\n"   &
   "\n"                     &
   " Use IEC port\n"        &
   " REU: %s\n"             &    -- REU submenu; %s is the size or Off
   " RAM Expansion Unit\n"  &
   "\n"                     &
   " Off\n"                 &    -- default
   " 512 KB\n"              &    -- Commodore 1750
   " 2 MB\n"                &
   " 16 MB\n"               &    -- R4 and later: in SDRAM
   "\n"                     &
   " File:%s\n"             &    -- the REU's image file: picking one loads it (LOAD_REU, SAVE_REU)
   "\n"                     &
   " Back to main menu\n"   &
   " Cartridge: %s\n"       &    -- Cartridge submenu; %s is the selected source
   " Cartridge\n"           &
   "\n"                     &
   " Hardware\n"            &    -- default: the physical cartridge slot
   " CRT file\n"            &    -- virtual cartridge
   " CRT:%s\n"              &    -- the .crt file; loading one selects CRT file
   " Boot:%s\n"             &    -- a .crt from /c128/carts loaded at every start (Gideon's "Cartridge")
   " Space clears Boot\n"   &    -- Space on the Boot line: no boot cartridge (core.c)
   "\n"                     &
   " Back to main menu\n"   &
   " Modem: %s\n"           &    -- Modem submenu; %s is the SwiftLink's address or Off
   " SwiftLink Modem\n"     &
   "\n"                     &
   " Off\n"                 &    -- default
   " $DE00\n"               &    -- SwiftLink's usual address
   " $DF00\n"               &    -- switches the REU off
   " $D700\n"               &    -- the C128's internal I/O slot
   "\n"                     &
   " Back to main menu\n"   &
   " UCI at $DF1C\n"        &    -- default off: Ultimate Command Interface
   " Net: %s\n"             &    -- Network submenu; %s is the IP address (core.c)
   " Network\n"             &
   "\n"                     &
   " REST API\n"            &    -- default on: Gideon's REST API on port 80
   " Host:%s\n"             &    -- typed in the menu, as Password and IP to DNS are (core.c)
   " Password:%s\n"         &    -- REST's X-Password, <none> without one
   "\n"                     &
   " DHCP\n"                &    -- default on
   " IP:%s\n"               &    -- what DHCP gave while it is on; typing one turns it off
   " Mask:%s\n"             &
   " GW:%s\n"               &
   " DNS:%s\n"              &
   "\n"                     &
   " Back to main menu\n"   &
   "\n"                     &

   " System ROMs: %s\n"     &    -- System ROMs submenu; %s is Stock or Custom (core.c). Each line
   " System ROMs\n"         &    -- picks a file in /c128/roms of the right size, kept on the card
   "\n"                     &
   " C128 KERNAL:%s\n"      &    -- 16 KB: editor, Z80 BIOS and KERNAL (U35)
   " C128 BASIC:%s\n"       &    -- 32 KB (U33 + U34)
   " C64 KERNAL:%s\n"       &    -- 8 KB, or 16 KB with C64 BASIC (U32): Gideon's "Alternate Kernal"
   " C64 BASIC:%s\n"        &    -- 8 KB
   " Characters:%s\n"       &    -- 8 KB, both sets
   " Function ROM:%s\n"     &    -- 16 or 32 KB, the internal function ROM socket (U36)
   "\n"                     &
   " Space: back to stock\n" &
   "\n"                     &
   " Back to main menu\n"   &

   " Credits\n"             &
   " Close Menu\n";

-- define your own constants here and choose meaningful names
-- make sure that your first group uses the value 1 (0 means "no menu item", such as text and line),
-- and be aware that you can only have a maximum of 254 groups (255 means "Close Menu");
-- also make sure that your group numbers are monotonic increasing (e.g. 1, 2, 3, 4, ...)
-- single-select items and therefore also drive mount items need to have unique identifiers
constant OPTM_G_VIDEO_OUT  : integer := 1;
constant OPTM_G_HDMI       : integer := 2;
constant OPTM_G_FF         : integer := 3;
constant OPTM_G_CRT        : integer := 4;
constant OPTM_G_JAILBARS   : integer := 5;
constant OPTM_G_CREDITS    : integer := 6;
constant OPTM_G_CART       : integer := 7;    -- Cartridge source: hardware slot or virtual cartridge
constant OPTM_G_MOUNT_D8   : integer := 8;
constant OPTM_G_MOUNT_D9   : integer := 9;
constant OPTM_G_DRVA_MODEL : integer := 10;   -- Drive A's model: 1541, 1571, 1581, Internal 1581
constant OPTM_G_IEC        : integer := 11;
constant OPTM_G_HELP_F13   : integer := 12;   -- OSM on F13 so C128 can use Help
constant OPTM_G_HDMI_AUDIO : integer := 13;   -- HDMI audio; off selects DVI output
constant OPTM_G_KEYS       : integer := 14;   -- Which physical key is 40/80, and which is Caps Lock
constant OPTM_G_VGA_MODES  : integer := 15;   -- VGA: Standard, 15 kHz with HS/VS, 15 kHz with CSYNC
constant OPTM_G_REU        : integer := 16;   -- REU: Off, 512 KB, 2 MB, 16 MB
constant OPTM_G_MOUNT_CRT  : integer := 17;   -- CRT:%s, loads the virtual cartridge's .crt file
constant OPTM_G_DRVB_MODEL : integer := 18;   -- Drive B's
constant OPTM_G_MODEM      : integer := 19;   -- Modem: Off, $DE00, $DF00, $D700
constant OPTM_G_UCI        : integer := 20;   -- Ultimate Command Interface at $DF1C
constant OPTM_G_REU_FILE   : integer := 21;   -- File:%s, loads the REU's image file
constant OPTM_G_NET_REST   : integer := 22;   -- REST API on port 80
constant OPTM_G_NET_HOST   : integer := 23;   -- Host:%s; it, Password and IP to DNS are text, typed in the menu
constant OPTM_G_NET_PASS   : integer := 24;   -- Password:%s
constant OPTM_G_NET_DHCP   : integer := 25;   -- DHCP, else the static address below
constant OPTM_G_NET_IP     : integer := 26;   -- IP:%s
constant OPTM_G_NET_MASK   : integer := 27;   -- Mask:%s
constant OPTM_G_NET_GW     : integer := 28;   -- GW:%s
constant OPTM_G_NET_DNS    : integer := 29;   -- DNS:%s
constant OPTM_G_CRT_BOOT   : integer := 30;   -- Boot:%s, the .crt loaded at every start
constant OPTM_G_ROM_K128   : integer := 31;   -- the System ROMs submenu's file lines, in its order
constant OPTM_G_ROM_B128   : integer := 32;
constant OPTM_G_ROM_K64    : integer := 33;
constant OPTM_G_ROM_B64    : integer := 34;
constant OPTM_G_ROM_CHAR   : integer := 35;
constant OPTM_G_ROM_FUNC   : integer := 36;
constant OPTM_G_ROM_D8_41  : integer := 37;   -- drive A's: 1541, 1571, 1581
constant OPTM_G_ROM_D8_71  : integer := 38;
constant OPTM_G_ROM_D8_81  : integer := 39;
constant OPTM_G_ROM_D9_41  : integer := 40;   -- drive B's
constant OPTM_G_ROM_D9_71  : integer := 41;
constant OPTM_G_ROM_D9_81  : integer := 42;
constant OPTM_G_DRVA_ID    : integer := 43;   -- Drive A's bus ID: 8, 9, 10, 11
constant OPTM_G_DRVB_ID    : integer := 44;   -- Drive B's

-- !!! DO NOT TOUCH !!! (unconstrained, for R3-memory builds' shorter menu)
type OPTM_GTYPE is array (natural range <>) of integer range 0 to 2**OPTM_GTC- 1;

-- define your menu groups: which menu items are belonging together to form a group?
-- where are separator lines? which items should be selected by default?
-- make sure that you have exactly the same amount of entries here than in OPTM_ITEMS_ALL and defined by OPTM_SIZE_ALL
constant OPTM_GROUPS_ALL   : OPTM_GTYPE(0 to OPTM_SIZE_ALL - 1) := ( OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Drives
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_SUBMENU + OPTM_G_START,                       -- Drive A submenu START, its line
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Drive A
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_MOUNT_D8 + OPTM_G_MOUNT_DRV,                  -- Disk:%s (virtual drive 0)
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Bus ID
                                             OPTM_G_DRVA_ID + OPTM_G_STDSEL,                      -- 8
                                             OPTM_G_DRVA_ID,                                      -- 9
                                             OPTM_G_DRVA_ID,                                      -- 10
                                             OPTM_G_DRVA_ID,                                      -- 11
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Model
                                             OPTM_G_DRVA_MODEL,                                   -- 1541
                                             OPTM_G_DRVA_MODEL + OPTM_G_STDSEL,                   -- 1571
                                             OPTM_G_DRVA_MODEL,                                   -- 1581
                                             OPTM_G_DRVA_MODEL,                                   -- Internal 1581
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_ROM_D8_41 + OPTM_G_LOAD_ROM,                  -- 1541 ROM:%s
                                             OPTM_G_ROM_D8_71 + OPTM_G_LOAD_ROM,                  -- 1571 ROM:%s
                                             OPTM_G_ROM_D8_81 + OPTM_G_LOAD_ROM,                  -- 1581 ROM:%s
                                             OPTM_G_TEXT,                                         -- Space: back to stock
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_SUBMENU,                                      -- Drive B submenu START, its line
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Drive B
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_MOUNT_D9 + OPTM_G_MOUNT_DRV,                  -- Disk:%s (virtual drive 1)
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Bus ID
                                             OPTM_G_DRVB_ID,                                      -- 8
                                             OPTM_G_DRVB_ID + OPTM_G_STDSEL,                      -- 9
                                             OPTM_G_DRVB_ID,                                      -- 10
                                             OPTM_G_DRVB_ID,                                      -- 11
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Model
                                             OPTM_G_DRVB_MODEL,                                   -- 1541
                                             OPTM_G_DRVB_MODEL + OPTM_G_STDSEL,                   -- 1571
                                             OPTM_G_DRVB_MODEL,                                   -- 1581
                                             OPTM_G_DRVB_MODEL,                                   -- Internal 1581
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_ROM_D9_41 + OPTM_G_LOAD_ROM,                  -- 1541 ROM:%s
                                             OPTM_G_ROM_D9_71 + OPTM_G_LOAD_ROM,                  -- 1571 ROM:%s
                                             OPTM_G_ROM_D9_81 + OPTM_G_LOAD_ROM,                  -- 1581 ROM:%s
                                             OPTM_G_TEXT,                                         -- Space: back to stock
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_LINE,                                         -- Line

                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Display Settings
                                             OPTM_G_LINE,                                         -- Line

                                             OPTM_G_SUBMENU,                                      -- Video submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Video Output
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_VIDEO_OUT + OPTM_G_STDSEL,                    -- Follow 40/80
                                             OPTM_G_VIDEO_OUT,                                    -- 40 columns
                                             OPTM_G_VIDEO_OUT,                                    -- 80 columns
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Dual screen
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_VIDEO_OUT,                                    -- HDMI 40, VGA 80
                                             OPTM_G_VIDEO_OUT,                                    -- HDMI 80, VGA 40
                                             OPTM_G_VIDEO_OUT,                                    -- Swap with 40/80
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- VIC-II Jailbars
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_JAILBARS + OPTM_G_STDSEL,                     -- Off
                                             OPTM_G_JAILBARS,                                     -- Low
                                             OPTM_G_JAILBARS,                                     -- Medium
                                             OPTM_G_JAILBARS,                                     -- High
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_SUBMENU,                                      -- HDMI submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- HDMI Display Mode
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_HDMI + OPTM_G_STDSEL,                         -- 720p 50 Hz 16:9
                                             OPTM_G_HDMI,                                         -- 720p 60 Hz 16:9
                                             OPTM_G_HDMI,                                         -- 576p 50 Hz 4:3
                                             OPTM_G_HDMI,                                         -- 576p 50 Hz 5:4
                                             OPTM_G_HDMI,                                         -- 640x480 60 Hz
                                             OPTM_G_HDMI,                                         -- 720x480 59.94 Hz
                                             OPTM_G_HDMI,                                         -- 800x600 60 Hz
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_FF      + OPTM_G_SINGLESEL,                   -- Flicker-free
                                             OPTM_G_HDMI_AUDIO + OPTM_G_SINGLESEL + OPTM_G_STDSEL, -- HDMI audio (default on)
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_CRT     + OPTM_G_SINGLESEL,                   -- HDMI: CRT emulation

                                             OPTM_G_SUBMENU,                                      -- VGA submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- VGA Display Mode
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_VGA_MODES + OPTM_G_STDSEL,                    -- Standard
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Retro 15 kHz mode
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_VGA_MODES,                                    -- 15 kHz with HS/VS
                                             OPTM_G_VGA_MODES,                                    -- 15 kHz with CSYNC
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_LINE,                                         -- Line

                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Keyboard
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_SUBMENU,                                      -- Caps Lock submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Caps Lock Key
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_KEYS + OPTM_G_STDSEL,                         -- 40/80
                                             OPTM_G_KEYS,                                         -- ASCII/DIN
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- F9 toggles the other
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu
                                             OPTM_G_HELP_F13 + OPTM_G_SINGLESEL,                  -- Help menu on F13 (default off)

                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- External Hardware
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_IEC + OPTM_G_SINGLESEL + OPTM_G_STDSEL,       -- Use IEC port
                                             OPTM_G_SUBMENU,                                      -- REU submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- RAM Expansion Unit
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_REU + OPTM_G_STDSEL,                          -- Off
                                             OPTM_G_REU,                                          -- 512 KB
                                             OPTM_G_REU,                                          -- 2 MB
                                             OPTM_G_REU,                                          -- 16 MB
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_REU_FILE + OPTM_G_LOAD_ROM,                   -- File:%s
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu
                                             OPTM_G_SUBMENU,                                      -- Cartridge submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Cartridge
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CART + OPTM_G_STDSEL,                         -- Hardware
                                             OPTM_G_CART,                                         -- CRT file
                                             OPTM_G_MOUNT_CRT + OPTM_G_LOAD_ROM,                  -- CRT:%s
                                             OPTM_G_CRT_BOOT + OPTM_G_LOAD_ROM,                   -- Boot:%s
                                             OPTM_G_TEXT,                                         -- Space clears Boot
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu
                                             OPTM_G_SUBMENU,                                      -- Modem submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- SwiftLink Modem
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_MODEM + OPTM_G_STDSEL,                        -- Off
                                             OPTM_G_MODEM,                                        -- $DE00
                                             OPTM_G_MODEM,                                        -- $DF00
                                             OPTM_G_MODEM,                                        -- $D700
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu
                                             OPTM_G_UCI + OPTM_G_SINGLESEL,                       -- UCI at $DF1C (default off)
                                             OPTM_G_SUBMENU,                                      -- Network submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- Network
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_NET_REST + OPTM_G_SINGLESEL + OPTM_G_STDSEL,  -- REST API (default on)
                                             OPTM_G_NET_HOST + OPTM_G_SINGLESEL,                  -- Host:%s
                                             OPTM_G_NET_PASS + OPTM_G_SINGLESEL,                  -- Password:%s
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_NET_DHCP + OPTM_G_SINGLESEL + OPTM_G_STDSEL,  -- DHCP (default on)
                                             OPTM_G_NET_IP + OPTM_G_SINGLESEL,                    -- IP:%s
                                             OPTM_G_NET_MASK + OPTM_G_SINGLESEL,                  -- Mask:%s
                                             OPTM_G_NET_GW + OPTM_G_SINGLESEL,                    -- GW:%s
                                             OPTM_G_NET_DNS + OPTM_G_SINGLESEL,                   -- DNS:%s
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu
                                             OPTM_G_LINE,                                         -- Line

                                             OPTM_G_SUBMENU,                                      -- System ROMs submenu START
                                             OPTM_G_TEXT + OPTM_G_HEADLINE,                       -- System ROMs
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_ROM_K128 + OPTM_G_LOAD_ROM,                   -- C128 KERNAL:%s
                                             OPTM_G_ROM_B128 + OPTM_G_LOAD_ROM,                   -- C128 BASIC:%s
                                             OPTM_G_ROM_K64 + OPTM_G_LOAD_ROM,                    -- C64 KERNAL:%s
                                             OPTM_G_ROM_B64 + OPTM_G_LOAD_ROM,                    -- C64 BASIC:%s
                                             OPTM_G_ROM_CHAR + OPTM_G_LOAD_ROM,                   -- Characters:%s
                                             OPTM_G_ROM_FUNC + OPTM_G_LOAD_ROM,                   -- Function ROM:%s
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_TEXT,                                         -- Space: back to stock
                                             OPTM_G_LINE,                                         -- Line
                                             OPTM_G_CLOSE + OPTM_G_SUBMENU,                       -- Back to main menu

                                             OPTM_G_CREDITS + OPTM_G_HELP,                        -- Credits (WHS(1))
                                             OPTM_G_CLOSE                                         -- Close Menu
                                           );

-- The menu without items n to n + count - 1. Each line of OPTM_ITEMS_ALL ends with the two
-- characters \n.
function drop_lines(items : string; n : natural; count : natural) return string is
   variable lines : natural := 0;           -- lines so far
   variable first : natural := items'low;   -- where line n starts
begin
   for i in items'low to items'high - 1 loop
      if items(i) = '\' and items(i + 1) = 'n' then
         lines := lines + 1;
         if lines = n then
            first := i + 2;
         end if;
         if lines = n + count then
            return items(items'low to first - 1) & items(i + 2 to items'high);
         end if;
      end if;
   end loop;
   return items;
end function drop_lines;

function drop_groups(groups : OPTM_GTYPE; n : natural; count : natural) return OPTM_GTYPE is
   variable result : OPTM_GTYPE(0 to groups'length - count - 1);
begin
   for i in result'range loop
      if i < n then
         result(i) := groups(i);
      else
         result(i) := groups(i + count);
      end if;
   end loop;
   return result;
end function drop_groups;

function menu_items return string is
begin
   if G_R3_MEMORY then
      return drop_lines(OPTM_ITEMS_ALL, C_MENU_REU_2M, C_MENU_R3_DROP);
   end if;
   return OPTM_ITEMS_ALL;
end function menu_items;

function menu_groups return OPTM_GTYPE is
begin
   if G_R3_MEMORY then
      return drop_groups(OPTM_GROUPS_ALL, C_MENU_REU_2M, C_MENU_R3_DROP);
   end if;
   return OPTM_GROUPS_ALL;
end function menu_groups;

constant OPTM_ITEMS        : string := menu_items;
constant OPTM_GROUPS       : OPTM_GTYPE(0 to OPTM_SIZE - 1) := menu_groups;

--------------------------------------------------------------------------------------------------------------------
-- !!! CAUTION: M2M FRAMEWORK CODE !!! DO NOT TOUCH ANYTHING BELOW THIS LINE !!!
--------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------
-- Address Decoding
--------------------------------------------------------------------------------------------------------------------

begin

addr_decode : process(clk_i)
   -- return ASCII value of given string at the position defined by index (zero-based)
   pure function str2data(str : string; index : integer) return std_logic_vector is
   variable strpos : integer;
   begin
      strpos := index + 1;
      if strpos <= str'length then
         return std_logic_vector(to_unsigned(character'pos(str(strpos)), 16));
      else
         return X"0000"; -- zero terminated strings
      end if;
   end function str2data;

   -- return the dimensions of the Options menu
   pure function getDXDY(dx, dy, index: natural) return std_logic_vector is
   begin
      case index is
         when 0 => return std_logic_vector(to_unsigned(dx + 2, 16));
         when 1 => return std_logic_vector(to_unsigned(dy + 2, 16));
         when others => return X"0000";
      end case;
   end function getDXDY;

   -- convert bool to std_logic_vector
   pure function bool2slv(b: boolean) return std_logic_vector is
   begin
      if b then
         return x"0001";
      else
         return x"0000";
      end if;
   end function bool2slv;

   -- return the General Configuration settings
   function getGenConf(index: natural) return std_logic_vector is
   begin
      case index is
         when 1      => return std_logic_vector(to_unsigned(RESET_COUNTER, 16));
         when 2      => return bool2slv(OPTM_PAUSE);
         when 3      => return bool2slv(WELCOME_ACTIVE);
         when 4      => return bool2slv(WELCOME_AT_RESET);
         when 5      => return bool2slv(KEYBOARD_AT_RESET);
         when 6      => return bool2slv(JOY_1_AT_RESET);
         when 7      => return bool2slv(JOY_2_AT_RESET);
         when 8      => return bool2slv(KEYBOARD_AT_OSD);
         when 9      => return bool2slv(JOY_1_AT_OSD);
         when 10     => return bool2slv(JOY_2_AT_OSD);
         when 11     => return std_logic_vector(to_unsigned(ASCAL_USAGE, 16));
         when 12     => return std_logic_vector(to_unsigned(ASCAL_MODE, 16));
         when 13     => return std_logic_vector(to_unsigned(VD_ANTI_THRASHING_DELAY, 16));
         when 14     => return std_logic_vector(to_unsigned(VD_ITERATION_SIZE, 16));
         when 15     => return bool2slv(SAVE_SETTINGS);
         when others => return x"0000";
      end case;
   end function getGenConf;

   variable index           : integer;
   variable whs_page_index  : integer;
   variable whs_array_index : integer;

begin

   if falling_edge(clk_i) then

      index := to_integer(unsigned(address_i(11 downto 0)));
      whs_page_index  := to_integer(unsigned(address_i(19 downto 12)));
      whs_array_index := to_integer(unsigned(address_i(23 downto 20)));

      data_o <= x"EEEE";

      -----------------------------------------------------------------------------------
      -- Welcome & Help System: upper 4 bits of address equal SEL_WHS' upper 4 bits
      -----------------------------------------------------------------------------------

      if address_i(27 downto 24) = SEL_WHS(15 downto 12) then

         if  whs_array_index < WHS_RECORDS then
            if index = 4095 then
               data_o <= std_logic_vector(to_unsigned(WHS(whs_array_index).page_count, 16));
            else
               if index < WHS(whs_array_index).page_length(whs_page_index) then
                  data_o <= str2data(WHS_DATA, WHS(whs_array_index).page_start(whs_page_index) + index);
               else
                  data_o <= (others => '0'); -- zero-terminated strings
               end if;
            end if;
         end if;

      -----------------------------------------------------------------------------------
      -- All other selectors, which are 16-bit values
      -----------------------------------------------------------------------------------

      else

         case address_i(27 downto 12) is
            when SEL_GENERAL           => data_o <= getGenConf(index);
            when SEL_DIR_START         => data_o <= str2data(DIR_START, index);
            when SEL_CFG_FILE          => data_o <= str2data(CFG_FILE, index);
            when SEL_CORENAME          => data_o <= str2data(CORENAME, index);
            when SEL_OPTM_ITEMS        => data_o <= str2data(OPTM_ITEMS, index);
            when SEL_OPTM_MOUNT_STR    => data_o <= str2data(OPTM_S_MOUNT, index);
            when SEL_OPTM_CRTROM_STR   => data_o <= str2data(OPTM_S_CRTROM, index);
            when SEL_OPTM_SAVING_STR   => data_o <= str2data(OPTM_S_SAVING, index);
            when SEL_OPTM_GROUPS       => data_o <= std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(15)) &
                                                    std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(14)) & "0" &
                                                    std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(12)) & "0000" &
                                                    std_logic_vector(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(7 downto 0));
            when SEL_OPTM_STDSEL       => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(8));
            when SEL_OPTM_LINES        => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(9));
            when SEL_OPTM_START        => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(10));
            when SEL_OPTM_MOUNT_DRV    => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(11));
            when SEL_OPTM_HELP         => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(13));
            when SEL_OPTM_SINGLESEL    => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(15));
            when SEL_OPTM_CRTROM       => data_o <= x"000" & "000" & std_logic(to_unsigned(OPTM_GROUPS(index), OPTM_GTC)(16));
            when SEL_OPTM_ICOUNT       => data_o <= x"00" & std_logic_vector(to_unsigned(OPTM_SIZE, 8));
            when SEL_OPTM_DIMENSIONS   => data_o <= getDXDY(OPTM_DX, OPTM_DY, index);

            when others                => null;
         end case;
      end if;
   end if;
end process;

end architecture beh;
