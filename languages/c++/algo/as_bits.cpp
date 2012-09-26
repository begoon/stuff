int as_bits(char ch) {
  switch (ch) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': return 1 << (ch - '0');
    case 'a': return 0x0F;
    case 'b': return 0x0F << 1;
    case 'c': return 0x0F << 2;
    case 'd': return 0x0F << 3;
    case 'e': return 0x0F << 4;
    case 'f': return 0x0F << 5;
    case 'g': return 0x0F << 6;
    case '?': return (1 << 10) - 1;
  }
  assert(0);
}

