// Gideon's configuration stores as the core has them: the stores and items of his firmware
// that the core has, with his names and types (the choices the core's, under his names where
// he has the same), and what setting each one does, and a store of the core's own for the
// C128's ROMs. LOAD_CONFIG (uci_ctrl.c) and the settings file read them from a file in his
// format; REST's configs routes (net/rest_configs.c) read and set them. A change is a list
// of wanted values (stores_begin, stores_want) that stores_apply puts into effect at once,
// as his effectuate does.
//
// The settings file keeps what the menu's own settings file can't: the strings (network,
// REU image, ROMs, boot cartridge) and drive power set by REST or LOAD_CONFIG.
#pragma once
#include <stdint.h>
#include "ff.h"

#define SETTINGS_FILE "/c128/settings.cfg"

enum { ST_C64, ST_DRIVE_A, ST_DRIVE_B, ST_MODEM, ST_ETH, ST_NET, ST_ROMS, ST_COUNT };
// His CFG_TYPE_ENUM, _VALUE, _STRING, _STRPASS, _STRFUNC (here a file of rompick.c's folder)
enum { IT_ENUM, IT_VALUE, IT_STRING, IT_PASS, IT_FILE };

typedef struct {
   uint8_t            store, id, type;
   const char        *name;
   const char *const *choices;   // IT_ENUM, ended by NULL
   int16_t            min, max;  // IT_VALUE's range
   int16_t            def;       // IT_ENUM: the default's index; IT_VALUE: the number
   const char        *sdef;      // a string's default
} item_t;

extern const item_t stores_items[];
extern const int    stores_count;   // items

const char *store_name(int s);
int         store_find(const char *name);   // his find_store: any case, or his other name; -1

// An item's value now: an IT_ENUM's index or an IT_VALUE's number, or a string's text
int         item_get(const item_t *it);
int         item_pick(const item_t *it);      // an IT_FILE's rompick.h pick
const char *item_text(const item_t *it);
int         item_choices(const item_t *it);   // an IT_ENUM's choices on this board (REU sizes)
// The drives' submenus: a model (DM_*) and a bus ID (8-11) each. The internal floppy is the
// model of one drive at a time.
enum { DM_1541, DM_1571, DM_1581, DM_INTERNAL };
int         stores_drive_model(int d);
void        stores_drive_set_model(int d, int m);
int         stores_drive_id(int d);
void        stores_drive_set_id(int d, int id);   // swaps with the other drive if that has it
int         stores_drive_of_id(int id);     // -1 for none
int         stores_drive_internal(int d);
// The model nearest m that the drive's image runs: a D64 a 1541 or 1571, a D71 a 1571, a D81
// a 1581. Without an image, and for the internal floppy, m.
int         stores_drive_fit(int d, int m);
// A model REST or a config asks for: 0 if the drive's image can't run it. The internal
// floppy's drive stays it for a 1581 (his names can't say it).
int         stores_drive_ask_model(int d, int m);
// What a drive runs as, as his get_drive_type() gives it: 0 1541, 1 1571, 2 1581
int         stores_drive_type(int d);

// What a change wants (stores_want's results)
enum { WANT_OK, WANT_CHOICE, WANT_RANGE, WANT_NONUM };

void stores_begin(void);
// An item's new value as text. loadcfg: LOAD_CONFIG's reading (spaces around a choice don't
// count, a number as his sscanf takes it); else REST's (a choice exactly, in any case; a
// number as strtol takes it). WANT_RANGE: *n is the number.
int  stores_want(const item_t *it, const char *v, int loadcfg, int *n);
int  stores_apply(void);       // 0 if main.vhd never took the REU's new setting
// The items a store had changed by stores_apply since the last stores_saved(s)
int  stores_stale(int s);
void stores_saved(int s);

// The default of every item of store s, wanted
void stores_want_defaults(int s);
// What the menu's settings file and the settings file hold for store s, wanted
void stores_want_saved(int s);
// netcfg's strings, their defaults
void stores_net_defaults(void);

// The settings file: at the start, its items in effect (the defaults without it): 0, or -1
// without the file. And the items it keeps, written: 0, or -1.
int  stores_load(void);
int  stores_save(void);

// A settings file as his ConfigIO::S_read_from_file reads it: a [store] line, then its
// item=value lines; # or ; starts a comment. Stores outside mask (a bit per ST_*) are
// skipped without a word. Wants what it reads, logging as his does; 0 after an error, when
// the caller should apply nothing.
int  stores_read(FIL *f, unsigned mask, void (*log)(const char *fmt, ...));
