#ifndef _STRING_HEX_H
#define _STRING_HEX_H

#include <iostream>
#include <sstream>
#include <iterator>
#include <string>

namespace extra {
namespace string {

template <typename Iterator>
class basic_hex_iterator: public std::iterator<std::forward_iterator_tag, 
                                               void, void, void, void> {
 public:
  basic_hex_iterator(Iterator i, const char* sep = 0, const char* prefix = 0) :
    i_(i), sep_(sep), sep_iterator_(sep), 
    prefix_(prefix), prefix_iterator_(prefix),
    state_(prefix && *prefix ? PREFIX : HIGH) 
    {}
  bool operator==(const basic_hex_iterator& rhs) { return i_ == rhs.i_; }
  bool operator!=(const basic_hex_iterator& rhs) { return i_ != rhs.i_; }
  void operator++() {
    switch (state_) {
      case PREFIX:
        if (*++prefix_iterator_ == 0) {
          state_ = HIGH;
          prefix_iterator_ = prefix_;
        }
        break;
      case HIGH:
        state_ = LOW;
        break;
      case LOW:
        if (sep_iterator_ && *sep_iterator_) {
          state_ = SEP;
        } else {
          state_ = prefix_iterator_ && *prefix_iterator_ ? PREFIX : HIGH;
          ++i_;
        }
        break;
      case SEP:
        if (*++sep_iterator_ == 0) {
          ++i_;
          state_ = prefix_iterator_ && *prefix_iterator_ ? PREFIX : HIGH;
          sep_iterator_ = sep_;
        }
        break;
    }
  }
  std::string::value_type operator*() { 
    static const char hex[] = "0123456789ABCDEF";
    switch (state_) {
      case PREFIX: return *prefix_iterator_;
      case HIGH: return hex[(*i_ >> 4) & 0x0F];
      case LOW: return hex[*i_ & 0x0F];
      case SEP: return *sep_iterator_;
    }
    return 0;  // We should never reach this.
  }
 private:
  Iterator i_;
  const char* sep_;
  const char* sep_iterator_;
  const char* prefix_;
  const char* prefix_iterator_;
  enum State { PREFIX, HIGH, LOW, SEP } state_;
};

typedef basic_hex_iterator<std::string::const_iterator> hex_iterator;
typedef basic_hex_iterator<const char*> hex_c_iterator;

class hex {
 public:
  hex(const std::string& str, const char* sep = 0, const char* prefix = 0)
    : str_(str), sep_(sep), prefix_(prefix) {}
  friend std::ostream& operator<<(std::ostream& os, const hex& rhs);
 private:
  const std::string& str_;
  const char* sep_;
  const char* prefix_;
};

inline std::ostream& operator<<(std::ostream& os, const hex& i) {
  std::copy(hex_iterator(i.str_.begin(), i.sep_, i.prefix_),
            hex_iterator(i.str_.end()),
            std::ostream_iterator<char>(os));
  return os;
}

class hex_c {
 public:
  hex_c(const char* str, size_t sz, const char* sep = 0, const char* prefix = 0) 
    : str_(str), sz_(sz), sep_(sep), prefix_(prefix) {}
  friend std::ostream& operator<<(std::ostream& os, const hex_c& rhs);
 private:
  const char* str_;
  size_t sz_;
  const char* sep_;
  const char* prefix_;
};

inline std::ostream& operator<<(std::ostream& os, const hex_c& i) {
  std::copy(hex_c_iterator(i.str_, i.sep_, i.prefix_), 
            hex_c_iterator(i.str_ + i.sz_),
            std::ostream_iterator<char>(os));
  return os;
}

inline void make_hex(std::string& hex, const std::string& s, 
                                       const char* sep, const char* prefix) {
  hex.resize(s.length() * (2 + (sep ? strlen(sep) : 0)
                             + (prefix ? strlen(prefix) : 0)), 
             0);
  std::copy(hex_iterator(s.begin(), sep, prefix), 
            hex_iterator(s.end()), 
            hex.begin());
}

inline void make_hex(std::string& hex, const char* s, int sz, 
                                       const char* sep, const char* prefix) {
  hex.resize(sz * (2 + (sep ? strlen(sep) : 0) 
                     + (prefix ? strlen(prefix) : 0)),
             0);
  std::copy(hex_c_iterator(s, sep, prefix), 
            hex_c_iterator(s + sz), 
            hex.begin());
}

inline std::string make_hex(const std::string& s, const char* sep = 0,
                                                  const char* prefix = 0) {
  std::string hex;
  make_hex(hex, s, sep, prefix);
  return hex;
}

inline std::string make_hex(const char* s, size_t sz, 
                            const char* sep = 0, const char* prefix = 0) {
  std::string hex;
  make_hex(hex, s, sz, sep, prefix);
  return hex;
}
}  // namespace string
}  // namespace extra
#endif
