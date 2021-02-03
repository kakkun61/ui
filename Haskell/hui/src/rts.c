#include <Rts.h>

void HuiStart()
{
  int argc = 1;
  char* argv[] = { "ghcDll", NULL };
  char** args = argv;

  hs_init(&argc, &args);
}

void HuiEnd()
{
  hs_exit();
}
