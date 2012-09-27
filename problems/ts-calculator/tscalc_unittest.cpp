#include "gtest/gtest.h"

#include "tscalc.h"

TEST(TsTime, Contruct) {
  TsTime t("123456");
  EXPECT_DOUBLE_EQ(123456, t.toDouble());
  EXPECT_EQ(std::string("123456"), t.toString());
}
