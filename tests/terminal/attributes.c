/* Regression coverage for the libvterm versions used by source and AppImage builds.
 * Part of the MIT-licensed Lem substrate; see ../../LICENSE.
 */
#include "../../extensions/terminal/terminal.c"

int main(void)
{
  struct terminal terminal = {0};
  assert(terminal_last_cell_attrs_conceal(&terminal) == 0);
  assert(terminal_last_cell_attrs_small(&terminal) == 0);
  assert(terminal_last_cell_attrs_baseline(&terminal) == 0);
#if VTERM_VERSION_MAJOR > 0 || VTERM_VERSION_MINOR >= 2
  terminal.lastCell.attrs.conceal = 1;
  assert(terminal_last_cell_attrs_conceal(&terminal) == 1);
#endif
#if VTERM_VERSION_MAJOR > 0 || VTERM_VERSION_MINOR >= 3
  terminal.lastCell.attrs.small = 1;
  terminal.lastCell.attrs.baseline = 2;
  assert(terminal_last_cell_attrs_small(&terminal) == 1);
  assert(terminal_last_cell_attrs_baseline(&terminal) == 2);
#endif
  return 0;
}
