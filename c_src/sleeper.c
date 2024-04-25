#include <unistd.h>
#include <arpa/inet.h>

int main(int argc, char* argv[]) {
  fd_set rfds;
  struct timeval tv;
  int rc;
  int32_t len, us;

  tv.tv_sec  = 10000000;
  tv.tv_usec = 0;

  while(1) {
    FD_ZERO(&rfds);
    FD_SET(0, &rfds);
    rc = select(1, &rfds, NULL, NULL, &tv);
    if (rc == -1)
      return -1;
    if (rc == 1) {
      rc = read(0, &len, 4);
      len = ntohl(len);
      if (rc < 4 || len != 4)
        return -1;
      rc = read(0, &us, 4);
      us = ntohl(us);
      if (rc < 4 || us < 0)
        return -1;
      tv.tv_sec = 0;
      tv.tv_usec = us;
    } else {
      len = 0;
      write(1, &len, 4);
      tv.tv_sec = 10000000;
      tv.tv_usec = 0;
    }
  }
  return 0;
}
