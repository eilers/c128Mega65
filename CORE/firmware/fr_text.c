// Gideon's FileSystem::get_error_string for a FatFs result (uci.h): UCI's DOS targets and
// the REST API answer with it.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include "ff.h"
#include "uci.h"

// FileSystem::get_error_string: a status without a number
const char *uci_fr_text(int r)
{
   switch (r) {
   case FR_OK:                  return "OK!";
   case FR_DISK_ERR:            return "DISK ERROR";
   case FR_INT_ERR:             return "INTERNAL ERROR";
   case FR_NOT_READY:           return "DEVICE NOT READY";
   case FR_NO_FILE:             return "FILE DOESN'T EXIST";
   case FR_NO_PATH:             return "PATH DOESN'T EXIST";
   case FR_INVALID_NAME:        return "INVALID NAME";
   case FR_DENIED:              return "ACCESS DENIED";
   case FR_EXIST:               return "FILE EXISTS";
   case FR_INVALID_OBJECT:      return "INVALID OBJECT";
   case FR_WRITE_PROTECTED:     return "WRITE PROTECTED";
   case FR_INVALID_DRIVE:       return "INVALID DRIVE";
   case FR_NOT_ENABLED:         return "NOT ENABLED";
   case FR_NO_FILESYSTEM:       return "NO FILESYSTEM";
   case FR_MKFS_ABORTED:        return "MAKE FILESYSTEM ABORTED";
   case FR_TIMEOUT:             return "I/O TIMEOUT";
   case FR_LOCKED:              return "LOCKED";
   case FR_NOT_ENOUGH_CORE:     return "OUT OF MEMORY";
   default:                     return "UNKNOWN ERROR";
   }
}
