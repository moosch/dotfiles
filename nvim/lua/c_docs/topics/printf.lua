-- printf / scanf conversion reference.
--
-- A section carries either `rows` (aligned columns) or `text` (free lines),
-- never both. Column widths are computed at render time, so nothing here is
-- pre-padded.

return {
  id = "printf",
  title = "printf / scanf placeholders",
  summary = "specifiers, length modifiers, width, precision",
  sections = {
    {
      heading = "Conversion specifiers",
      rows = {
        { "%d %i", "int", "signed decimal" },
        { "%u", "unsigned int", "unsigned decimal" },
        { "%o", "unsigned int", "octal, no leading 0 unless # is given" },
        { "%x %X", "unsigned int", "hex, lower / upper case" },
        { "%f %F", "double", "fixed point, 6 decimals by default" },
        { "%e %E", "double", "scientific, 1.234560e+03" },
        { "%g %G", "double", "shortest of %e / %f, trailing zeros dropped" },
        { "%a %A", "double", "hex float, 0x1.91eb86p+1, exact round-trip" },
        { "%c", "int", "one character, promoted from char" },
        { "%s", "char *", "NUL-terminated string" },
        { "%p", "void *", "pointer, cast the argument: (void *)p" },
        { "%n", "int *", "stores the count written so far, usually disabled" },
        { "%%", "-", "a literal percent sign" },
      },
    },
    {
      heading = "Length modifiers",
      rows = {
        { "%hhd %hhu", "signed/unsigned char", "argument still arrives as int" },
        { "%hd %hu", "short / unsigned short", "argument still arrives as int" },
        { "%ld %lu", "long / unsigned long", "" },
        { "%lld %llu", "long long", "" },
        { "%zu %zd", "size_t / ssize_t", "what sizeof and strlen return" },
        { "%td", "ptrdiff_t", "result of pointer subtraction" },
        { "%jd %ju", "intmax_t / uintmax_t", "" },
        { "%Lf", "long double", "" },
        { "%lc %ls", "wint_t / wchar_t *", "wide character and wide string" },
      },
    },
    {
      heading = "Flags, width, precision",
      rows = {
        { "%-8d", "left align", "pad on the right instead" },
        { "%+d", "always sign", "print + for non-negative values" },
        { "% d", "space for sign", "a blank where the + would be" },
        { "%#x %#o", "alternate form", "prefix 0x / 0, keep the decimal point" },
        { "%08.3f", "zero pad", "0 is ignored when - is also present" },
        { "%8s", "min width 8", "pads, never truncates" },
        { "%.4s", "precision 4", "truncates a string to 4 chars" },
        { "%.3f", "precision 3", "3 digits after the point" },
        { "%*d", "width from arg", 'printf("%*d", 8, n)' },
        { "%.*f", "precision from arg", 'printf("%.*f", 3, x)' },
      },
    },
    {
      heading = "Fixed-width types: inttypes.h",
      text = {
        'There is no portable specifier for int32_t, so the macros expand to one:',
        '',
        '  printf("%" PRId64 "\\n", (int64_t)x);',
        '  printf("value=%" PRIu32 " at %" PRIxPTR "\\n", u, (uintptr_t)p);',
        '  scanf("%" SCNd64, &big);',
        '',
        'PRId8/16/32/64  PRIu..  PRIx..  PRIo..   printing',
        'SCNd..  SCNu..  SCNx..                   scanning',
        'PRIdPTR PRIxPTR PRIdMAX PRIuMAX          pointer- and max-width',
      },
    },
    {
      heading = "scanf differs from printf",
      text = {
        'Pass the address:            scanf("%d", &n)',
        '%s stops at whitespace and never bounds itself; always give a width',
        'that is one less than the buffer:   char b[32]; scanf("%31s", b);',
        '%[^\\n] reads up to the newline, %[a-z] reads a set, %*d reads and',
        'discards a field.',
        '',
        'A blank in the format skips any run of whitespace. Every other literal',
        'character must match the input exactly or the scan stops there.',
        '',
        'The return value is the number of fields assigned, not the bytes read.',
        'Check it: if (scanf("%d %d", &a, &b) != 2) { ... }   EOF returns EOF.',
        '',
        'Floats: printf takes %f for a double, scanf needs %lf for a double and',
        '%f only for a float. Getting this wrong is undefined behaviour.',
      },
    },
  },
}
