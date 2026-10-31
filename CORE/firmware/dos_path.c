// Paths as Gideon's Path has them, for UCI DOS, REST and the FTP server (uci.h): '/' or '\'
// between names, '.' and '..', a name from '/' or from the folder. '/' holds one folder, SD,
// which is the active card.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include "uci.h"

int uci_resolve(const char *cwd, const char *name, char *out)
{
   int n = 0;
   if (*name == '/' || *name == '\\')
      name++;
   else {
      n = strlen(cwd) - 1;
      memcpy(out, cwd, n);
   }
   out[n] = 0;
   while (*name) {
      const char *e = name;
      while (*e && *e != '/' && *e != '\\')
         e++;
      int k = e - name;
      if (k == 2 && name[0] == '.' && name[1] == '.') {
         while (n > 0 && out[--n] != '/')
            ;
         out[n] = 0;
      } else if (k && !(k == 1 && name[0] == '.')) {
         if (n + 1 + k >= UCI_PATH_MAX - 1)
            return 0;
         out[n++] = '/';
         memcpy(out + n, name, k);
         n += k;
         out[n] = 0;
      }
      name = *e ? e + 1 : e;
   }
   return 1;
}

const char *uci_on_card(const char *p)
{
   if (p[0] != '/' || (p[1] | 0x20) != 's' || (p[2] | 0x20) != 'd' || (p[3] && p[3] != '/'))
      return NULL;
   return p[3] ? p + 3 : "/";
}

const char *uci_card_path(const char *name, char *out)
{
   return uci_resolve("/", name, out) ? uci_on_card(out) : NULL;
}
