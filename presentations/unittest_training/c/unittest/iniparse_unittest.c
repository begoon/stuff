#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmockery.h>

#include <stdio.h>
#include <string.h>

#include "iniparse.h"

#define check_parse_string(expected, name, value) { \
  char buf_[256]; \
  char* name_; \
  char* value_;  \
  strcpy(buf_, expected); \
  parse_string(buf_, &name_, &value_); \
  assert_string_equal(name_, name); \
  assert_string_equal(value_, value); \
}
  
void parse_string_test(void **state) {
  check_parse_string("SID=testdb", "SID", "testdb");
  check_parse_string("user=test", "user", "test");
  check_parse_string("user=", "user", "");
  check_parse_string("=user", "", "user");
  check_parse_string("=", "", "");
  // check_parse_string("user", "user", "");  // BUG!!!
}

static char* mock_lines[] = {
  "SID=testdb\n",
  "host=localhost\n",
  "user=tester\n",
  "\n",
  NULL
};

static char** mock_lines_pointer = mock_lines;

int _mock_feof(FILE* f) {
  return *mock_lines_pointer == NULL;
}

int _mock_fgets(char* buf, int sz, FILE* f) {
  *buf = 0;
  if (_mock_feof(f)) return 0;
  strcpy(buf, *mock_lines_pointer);
  mock_lines_pointer += 1;
  return strlen(buf);
}

void parse_string_overall_test(void **state) {
  size_t sz;
  assert_int_equal(3, parse("iniparse.h"));
  assert_string_equal(options[0].name, "SID");
  assert_string_equal(options[0].value, "testdb");
  assert_string_equal(options[1].name, "host");
  assert_string_equal(options[1].value, "localhost");
  assert_string_equal(options[2].name, "user");
  assert_string_equal(options[2].value, "tester");
}

#define check_trim_trailing_crlf(expected, original) { \
  char s[256]; \
  strcpy(s, original); \
  trim_trailing_crlf(s); \
  assert_string_equal(expected, s); \
}

void trim_trailing_crlf_test(void **state) {
  check_trim_trailing_crlf("1234", "1234");
  check_trim_trailing_crlf("1234", "1234\r\n");
  check_trim_trailing_crlf("", "\n\r");
  check_trim_trailing_crlf("", "");
  // check_trim_trailing_crlf("", NULL);  // BUG!!!
  // check_trim_trailing_crlf("1234", "1234\r\n\t");  // NOT Implemented!
}

void parse_non_existing_file_test(void **state) {
  assert_int_equal(-1, parse("this_file_is_100%_non_existing"));
}
