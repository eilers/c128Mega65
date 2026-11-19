// test_shell -live: host_net.c's web server hands a request on to the real server it was
// meant for (its Host header), over this Mac's network, and gives its whole answer back.
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

unsigned char *host_live_web(const char *req, int *len)
{
   char host[128] = "";
   const char *h = strstr(req, "\r\nHost: ");
   struct addrinfo hints = { .ai_family = AF_INET, .ai_socktype = SOCK_STREAM }, *ai;
   if (!h || sscanf(h + 8, "%127[^\r\n]", host) != 1 || getaddrinfo(host, "80", &hints, &ai))
      return NULL;
   int s = socket(ai->ai_family, ai->ai_socktype, 0), cap = 1 << 16, n = 0, k;
   unsigned char *buf = malloc(cap);
   if (s < 0 || connect(s, ai->ai_addr, ai->ai_addrlen) || write(s, req, strlen(req)) < 0) {
      perror(host);
      free(buf);
      buf = NULL;
   } else {
      while ((k = read(s, buf + n, cap - n)) > 0)
         if ((n += k) == cap)
            buf = realloc(buf, cap *= 2);
      fprintf(stderr, "live: %.60s -> %d bytes\n", req, n);
   }
   close(s);
   freeaddrinfo(ai);
   *len = n;
   return buf;
}
