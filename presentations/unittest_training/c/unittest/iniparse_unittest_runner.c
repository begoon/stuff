#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmockery.h>

#include "iniparse_unittest.c"

int main(int argc, char* argv[]) {
    const UnitTest tests[] = {
        unit_test(parse_string_test),
        unit_test(parse_string_overall_test),
        unit_test(trim_trailing_crlf_test),
        unit_test(parse_non_existing_file_test),
    };
    return run_tests(tests);
}
