#include "gtest/gtest.h"
#include <string>
#include <sstream>
#include "iniparse.h"

TEST(IniParser, Ok) {
  IniParser parser("silly_file_name");
  EXPECT_TRUE(parser.ok());
}

TEST(IniParser, Generic) {
  std::stringstream fmt;
  fmt << "user=test1" << std::endl
      << "passwd=1234" << std::endl
      << "type=    admin" << std::endl
      << "type1=   admin  " << std::endl
  ;

  IniParser parser(fmt);

  EXPECT_EQ(std::string("test1"), parser.get("user"));
  EXPECT_EQ(std::string("1234"), parser.get("passwd"));
  EXPECT_EQ(std::string("admin"), parser.get("type"));
  EXPECT_EQ(std::string("admin"), parser.get("type1"));
}

using string::trimLeft;

TEST(Trim, StringTrimLeft) {
  std::string s = "123";
  trimLeft(s, "abc");
  EXPECT_EQ(std::string("123"), s);

  s = "aacc123";
  trimLeft(s, "abc");
  EXPECT_EQ(std::string("123"), s);

  s = "aacc1a2bc3a";
  trimLeft(s, "abc");
  EXPECT_EQ(std::string("1a2bc3a"), s);

  s = "";
  trimLeft(s, "abc");
  EXPECT_EQ(std::string(""), s);

  s = "aabbbaacc";
  trimLeft(s, "abc");
  EXPECT_EQ(std::string(""), s);
}

TEST(Trim, StringTrimLeftDefault) {
  std::string s = "123";
  trimLeft(s);
  EXPECT_EQ(std::string("123"), s);
  s = "\t\r\n  123";
  trimLeft(s);
  EXPECT_EQ(std::string("123"), s);
}

using string::trimRight;

TEST(Trim, StringTrimRight) {
  std::string s = "123";
  trimRight(s, "abc");
  EXPECT_EQ(std::string("123"), s);
  s = "123aacc";
  trimRight(s, "abc");
  EXPECT_EQ(std::string("123"), s);
  s = "1a2bc3aaacc";
  trimRight(s, "abc");
  EXPECT_EQ(std::string("1a2bc3"), s);
  s = "";
  trimRight(s, "abc");
  EXPECT_EQ(std::string(""), s);
  s = "aabbbaacc";
  trimRight(s, "abc");
  EXPECT_EQ(std::string(""), s);
}

TEST(Trim, StringTrimRightDefault) {
  std::string s = "123";
  trimRight(s);
  EXPECT_EQ(std::string("123"), s);
  s = "123\t\r\n  ";
  trimRight(s);
  EXPECT_EQ(std::string("123"), s);
}

using string::trim;

TEST(Trim, StringTrim) {
  std::string s = "123";
  trim(s, "abc");
  EXPECT_EQ(std::string("123"), s);
  s = "aa123aacc";
  trim(s, "abc");
  EXPECT_EQ(std::string("123"), s);
  s = "bbb1a2bc3aaacc";
  trim(s, "abc");
  EXPECT_EQ(std::string("1a2bc3"), s);
  s = "";
  trim(s, "abc");
  EXPECT_EQ(std::string(""), s);
  s = "aabbbaacc";
  trim(s, "ac");
  EXPECT_EQ(std::string("bbb"), s);
  s = "aabbbaacc";
  trim(s, "abc");
  EXPECT_EQ(std::string(""), s);
}

TEST(Trim, StringTrimDefault) {
  std::string s = "123";
  trim(s);
  EXPECT_EQ(std::string("123"), s);
  s = "       \t123\t\r\n  ";
  trim(s);
  EXPECT_EQ(std::string("123"), s);
}
