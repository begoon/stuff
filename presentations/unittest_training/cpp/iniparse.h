#ifndef _INIPARSER_H
#define _INIPARSER_H

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <cassert>

namespace string {

static const char* kDefaultTrimSet = " \t\r\n";

void trimLeft(std::string& s, const std::string& junk) {
  size_t i = s.find_first_not_of(junk);
  s.erase(0, i);
}

void trimLeft(std::string& s) {  // NOLINT
  trimLeft(s, std::string(kDefaultTrimSet));
}

void trimRight(std::string& s, const std::string& junk) {
  size_t i = s.find_last_not_of(junk);
  if (i == std::string::npos)
    s.clear();
  else
    s.erase(i + 1);
}

void trimRight(std::string& s) {  // NOLINT
  trimRight(s, std::string(kDefaultTrimSet));
}

void trim(std::string& s, const std::string& junk) {
  trimLeft(s, junk);
  trimRight(s, junk);
}

void trim(std::string& s) {  // NOLINT
  trimLeft(s);
  trimRight(s);
}
}  // string


class IniParser {
 public:
  bool ok_;
 public:
  IniParser(const char* name) {
    std::ifstream is(name);
    load(is);
    ok_ = true;
  }

  IniParser(std::istream& is) {
    load(is);
    ok_ = true;
  }

  bool ok() const { return ok_; }

  const std::string& get(const std::string& name) {
    return values_[name];
  }

  friend std::ostream& operator<<(std::ostream& os, const IniParser& parser);
 private:
  typedef std::map<std::string, std::string> Values;
  Values values_;

  void load(std::istream& is) {
    while (is) {
      std::string line;
      std::getline(is, line);
      size_t i = line.find('=');
      if (i == std::string::npos) continue;
      std::string name = line.substr(0, i);
      std::string value = line.substr(i + 1);
      string::trim(value);
      values_[name] = value;
    }
  }
};

std::ostream& operator<<(std::ostream& os, const IniParser& parser) {
  for (IniParser::Values::const_iterator i = parser.values_.begin();
                                         i != parser.values_.end(); ++i) {
    os << i->first << " = " << i->second << std::endl;
  }
  return os;
}

#endif
