vector<string> tokenize(string s, string ch) {
  vector<string> ret;
  for (int p = 0, p2; p < s.size(); p = p2 + 1) {
    p2 = s.find_first_of(ch, p);
    if (p2 == -1) p2 = s.size();
    if (p2 - p > 0) ret.push_back(s.substr(p, p2 - p));
  }
  return ret;
}

set<string> tokenize_set(string s, string ch) {
  set<string> ret;
  for (int p = 0, p2; p < s.size(); p = p2 + 1) {
    p2 = s.find_first_of(ch, p);
    if (p2 == -1) p2 = s.size();
    if (p2 - p > 0) ret.insert(s.substr(p, p2 - p));
  }
  return ret;
}
