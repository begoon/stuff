void remove_spaces(string& s) {
  s.erase(0, s.find_first_not_of(" "));
  s.resize(s.find_last_not_of(" ") + 1);
}

