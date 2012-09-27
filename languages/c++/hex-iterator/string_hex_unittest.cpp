#include <gtest/gtest.h>

#include "string_hex.h"

#include <string>
#include <sstream>
#include <iterator>

using extra::string::basic_hex_iterator;
using extra::string::hex_iterator;
using extra::string::hex_c_iterator;
using extra::string::hex;
using extra::string::hex_c;
using extra::string::make_hex;

using std::copy;
using std::back_inserter;
using std::string;

// This test checks that in general hex_iterator works as appropriate.
TEST(String, HexIterator) {
  const std::string s("13\x00\xFF\x80 \x21", 7);

  std::string r(s.length() * 2, '?');
  copy(hex_iterator(s.begin()), hex_iterator(s.end()), r.begin());
  EXPECT_STREQ("313300FF802021", r.c_str());

  r.clear();
  copy(hex_iterator(s.begin()), hex_iterator(s.end()), back_inserter(r));
  EXPECT_STREQ("313300FF802021", r.c_str());

  r.clear();
  copy(hex_iterator(s.begin(), ", "), hex_iterator(s.end()), back_inserter(r));
  EXPECT_STREQ("31, 33, 00, FF, 80, 20, 21, ", r.c_str());

  r.resize(s.length() * (2 + 2 + 2), '?');
  copy(hex_iterator(s.begin(), ", ", "0x"), hex_iterator(s.end()), r.begin());
  EXPECT_STREQ("0x31, 0x33, 0x00, 0xFF, 0x80, 0x20, 0x21, ", r.c_str());

  r.resize(s.length() * (2 + 2), '?');
  copy(hex_iterator(s.begin(), 0, " $"), hex_iterator(s.end()), r.begin());
  EXPECT_STREQ(" $31 $33 $00 $FF $80 $20 $21", r.c_str());
}

// This test checks that in general hex_c_iterator works as appropriate.
TEST(String, HexCIterator) {
  const char* s = "13\x00\xFF\x80 \x21";
  size_t sz = 7;

  std::string r(sz * 2, '?');
  copy(hex_c_iterator(s), hex_c_iterator(s + sz), r.begin());
  EXPECT_STREQ("313300FF802021", r.c_str());

  r.clear();
  copy(hex_c_iterator(s), hex_c_iterator(s + sz), back_inserter(r));
  EXPECT_STREQ("313300FF802021", r.c_str());

  r.clear();
  copy(hex_c_iterator(s, ", "), hex_c_iterator(s + sz), back_inserter(r));
  EXPECT_STREQ("31, 33, 00, FF, 80, 20, 21, ", r.c_str());

  r.resize(sz * (2 + 2 + 2), '?');
  copy(hex_c_iterator(s, ", ", "0x"), hex_c_iterator(s + sz), r.begin());
  EXPECT_STREQ("0x31, 0x33, 0x00, 0xFF, 0x80, 0x20, 0x21, ", r.c_str());

  r.resize(sz * (2 + 2), '?');
  copy(hex_c_iterator(s, 0, " $"), hex_c_iterator(s + sz), r.begin());
  EXPECT_STREQ(" $31 $33 $00 $FF $80 $20 $21", r.c_str());
}

// This test checks that hex manipulator works as appropriate.
TEST(String, HexManipulator) {
  const string s("13\x00\xFF\x80 \x21", 7);
  std::stringstream fmt;

  EXPECT_TRUE((fmt.flags() & std::ios_base::hex) == 0);
  EXPECT_TRUE((fmt.flags() & std::ios_base::uppercase) == 0);
  fmt << hex(s, " ");
  EXPECT_TRUE((fmt.flags() & std::ios_base::hex) == 0);
  EXPECT_TRUE((fmt.flags() & std::ios_base::uppercase) == 0);
  EXPECT_EQ(' ', fmt.fill(' '));
  EXPECT_STREQ("31 33 00 FF 80 20 21 ", fmt.str().c_str());

  fmt.str("");
  fmt << hex(s, ", ");
  EXPECT_STREQ("31, 33, 00, FF, 80, 20, 21, ", fmt.str().c_str());

  fmt.str("");
  fmt << hex(s, ", ", "0x");
  EXPECT_STREQ("0x31, 0x33, 0x00, 0xFF, 0x80, 0x20, 0x21, ", fmt.str().c_str());

  fmt.str("");
  fmt << hex(s, 0, " $");
  EXPECT_STREQ(" $31 $33 $00 $FF $80 $20 $21", fmt.str().c_str());
}

// This test checks that hex_c manipulator works as appropriate.
TEST(String, HexCManipulator) {
  const char source[] = "13\x00\xFF\x80 \x21";
  const int source_sz = 7;
  std::stringstream fmt;

  EXPECT_TRUE((fmt.flags() & std::ios_base::hex) == 0);
  EXPECT_TRUE((fmt.flags() & std::ios_base::uppercase) == 0);
  fmt << hex_c(source, source_sz);
  EXPECT_TRUE((fmt.flags() & std::ios_base::hex) == 0);
  EXPECT_TRUE((fmt.flags() & std::ios_base::uppercase) == 0);
  EXPECT_EQ(' ', fmt.fill(' '));
  EXPECT_STREQ("313300FF802021", fmt.str().c_str());

  fmt.str("");
  fmt << hex_c(source, source_sz, ", ");
  EXPECT_STREQ("31, 33, 00, FF, 80, 20, 21, ", fmt.str().c_str());

  fmt.str("");
  fmt << hex_c(source, source_sz, ", ", "0x");
  EXPECT_STREQ("0x31, 0x33, 0x00, 0xFF, 0x80, 0x20, 0x21, ", fmt.str().c_str());

  fmt.str("");
  fmt << hex_c(source, source_sz, 0, " $");
  EXPECT_STREQ(" $31 $33 $00 $FF $80 $20 $21", fmt.str().c_str());

  fmt.str("");
}

// This test checks that make_hex functions work as appropriate.
TEST(String, MakeHex) {
  const string s("13\x00\xFF\x80 ", 6);
  EXPECT_EQ(string("313300FF8020"), make_hex(s));
  EXPECT_EQ(string("31-33-00-FF-80-20-"), make_hex(s, "-"));
  EXPECT_EQ(string("$31,$33,$00,$FF,$80,$20,"), make_hex(s, ",", "$"));
  EXPECT_EQ(string(" $31 $33 $00 $FF $80 $20"), make_hex(s, 0, " $"));

  const char* c = "13\x00\xFF\x80 ";
  const size_t sz = 6;
  EXPECT_EQ(string("313300FF8020"), make_hex(c, sz));
  EXPECT_EQ(string("31-33-00-FF-80-20-"), make_hex(c, sz, "-"));
  EXPECT_EQ(string("$31,$33,$00,$FF,$80,$20,"), make_hex(c, sz, ",", "$"));
  EXPECT_EQ(string(" $31 $33 $00 $FF $80 $20"), make_hex(c, sz, 0, " $"));
}

TEST(String, MakeHexDegenerateCases) {
  // A few high level tests and they do involve the entire mechanism.
  EXPECT_EQ(string(""), make_hex(string("")));
  EXPECT_EQ(string(""), make_hex(string(""), ""));
  EXPECT_EQ(string(""), make_hex(string(""), "", ""));
  EXPECT_EQ(string(""), make_hex(string(""), "x", ""));
  EXPECT_EQ(string(""), make_hex(string(""), "", "x"));
  EXPECT_EQ(string(" %31, "), make_hex(string("1"), ", ", " %"));
  EXPECT_EQ(string(" %31"), make_hex(string("1"), 0, " %"));
  EXPECT_EQ(string("31--"), make_hex(string("1"), "--", 0));
  EXPECT_EQ(string("31"), make_hex(string("1"), 0, 0));
}
