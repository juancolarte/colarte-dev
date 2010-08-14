#include <stdio.h>
#include <magic.h>

int main() {
  struct magic_set *magic = magic_open(MAGIC_MIME|MAGIC_CHECK);
  magic_load(magic,NULL); // loads the default mime type definition
  printf("magic output: '%s'\n",magic_file(magic,"/some/file"));

  return 0;
}
