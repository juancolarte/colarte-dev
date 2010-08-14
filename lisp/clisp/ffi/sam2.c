#include <stdio.h>
#include <magic.h>

int main() {
  magic_t myt = magic_open(MAGIC_CONTINUE|MAGIC_ERROR/*|MAGIC_DEBUG*/|MAGIC_MIME);
  magic_load(myt,NULL);
  printf("magic output: '%s'\n",magic_file(myt,"/etc/passwd"));
  magic_close(myt);
  return 0;
}
