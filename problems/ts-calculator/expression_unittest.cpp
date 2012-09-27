#include <gtest/gtest.h>

#include "expression.h"

using namespace expression;

TEST(Expression, Run) {
  Expression expression;
  expression.compile("(10 + 20 + 1.1)");
  Context context;
  EXPECT_EQ(31.1, expression.run(context));
  EXPECT_EQ(31.1, expression.run(context));
  EXPECT_EQ(31.1, expression.run(context));

  context["eur"] = 20.0;
  expression.compile("(10 + eur + 1.1)");
  EXPECT_EQ(31.1, expression.run(context));

  context["eur"] = 21.0;
  expression.compile("(10 + eur + 1.1)");
  EXPECT_EQ(32.1, expression.run(context));

  context.erase("eur");
  expression.compile("(10 + eur + 1.1)");
  EXPECT_EQ(11.1, expression.run(context));

  expression.compile("average(1, 2) * 21 + 1.1");
  EXPECT_EQ(32.6, expression.run(context));

  expression.compile("1 + 2*3");
  EXPECT_EQ(7, expression.run(context));

  expression.compile("(1 + 2)*3");
  EXPECT_EQ(9, expression.run(context));

  expression.compile("(3 - 1) / 0.4");
  EXPECT_EQ(5, expression.run(context));
}

TEST(Expression, Symbols) {
  Expression expression;
  EXPECT_EQ(0, expression.symbols().size());
  expression.compile("eur + gbp");
  ASSERT_EQ(2, expression.symbols().size());
  EXPECT_EQ(std::string("eur"), expression.symbols()[0]);
  EXPECT_EQ(std::string("gbp"), expression.symbols()[1]);
}

