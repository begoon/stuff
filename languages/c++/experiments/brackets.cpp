#include <iostream>
#include <string>
#include <algorithm>

void generate(std::string& s, int pos, int n, int open, int close) {
  if (pos == s.length()) {
    std::cout << s << std::endl;
    return;
  }
  if (open > close) {
    s[pos] = ')';
    generate(s, pos + 1, n, open, close + 1);
  }
  if (open < n) {
    s[pos] = '(';
    generate(s, pos + 1, n, open + 1, close);
  }
}

void generate_brackets(int n) {
  std::string s(n * 2, ')');
  std::fill_n(s.begin(), n, '(');

  std::cout << s << std::endl;

  bool more;
  do {
    more = false;
    for (int i = s.length() - 1, open = 0, close = 0; i >= 0; --i) {
      if (s[i] == '(') open += 1;
      if (s[i] == ')') close += 1;
      if (s[i] == '(' && open < close) {
        s[i++] = ')';
        for (; open > 0; --open, ++i) s[i] = '(';
        for (--close; close > 0; --close, ++i) s[i] = ')';
        std::cout << s << std::endl;
        more = true;
        break;
      }
    }
  } while (more);
}

int main(int argc, char* argv[]) {
  int n = argc > 1 ? std::atoi(argv[1]) : 4;
  std::string s(n * 2, '?');
  generate(s, 0, n, 0, 0);
  std::cout << std::endl;

  generate_brackets(n);

  return 0;
}


