#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <iomanip>
#include <iterator>

using namespace std;

vector<int> kmp(string s) {
  int n = (int) s.length();
  vector<int> pi (n);
  for (int i=1; i<n; ++i) {
    int j = pi[i-1];
    while (j > 0 && s[i] != s[j])
      j = pi[j-1];
    if (s[i] == s[j])  ++j;
    pi[i] = j;
  }
  return pi;
}

int main(int argc, char* argv[]) {
  vector<int> p = kmp(argv[1]);
  copy(p.begin(), p.end(), ostream_iterator<int>(cout, " "));
  return 0;
}
