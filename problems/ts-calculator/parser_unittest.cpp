#include "gtest/gtest.h"
#include <iostream>
#include <sstream>
#include <exception>

#include "tokenizer.h"
#include "parser.h"

using namespace grammar;

void checkGrammar(const char* expression, const char* parsed[]) {
  try {
    std::stringstream fmt;
    fmt.str(expression);
    Tokenizer tokenizer(fmt);
    Parser parser(fmt, tokenizer);
    Program program;
    parser.parse(&program);
    for (int i = 0; parsed[i]; ++i) {
      ASSERT_NE(i, (int)program.size());
      EXPECT_EQ(std::string(parsed[i]), program[i]);
    }
  } catch (const Parser::SyntaxError& e) {
    FAIL() << e.what();
  }
}

TEST(Parser, Expr_PlusPlus) {
  const char* parsed[] = {"50", "20.12", "@+", "100", "@+", 0};
  SCOPED_TRACE("");
  checkGrammar("50 + 20.12 + 100", parsed);
}

TEST(Parser, Expr_FunctionCall) {
  const char* parsed[] = {"50", "20.12", "@+", "@f", 0};
  SCOPED_TRACE("");
  checkGrammar("f( 50 + 20.12 )", parsed);
}

TEST(Parser, Expr_Eur) {
  const char* parsed[] = {"#eur", 0};
  SCOPED_TRACE("");
  checkGrammar("eur", parsed);
}

TEST(Parser, Expr_1_Div_eur) {
  const char* parsed[] = {"1", "#eur", "@/", 0};
  SCOPED_TRACE("");
  checkGrammar("1 / eur", parsed);
}

TEST(Parser, Expr_LP_bid_Plus_ask_RP_Div_2) {
  const char* parsed[] = {"#bid", "#ask", "@+", "2", "@/", 0};
  SCOPED_TRACE("");
  checkGrammar("( bid + ask ) / 2", parsed);
}

TEST(Parser, Expr_bucketed_avg_LP_ask_Minus_bid_Comma_15_Times_60_RP) {
  const char* parsed[] = {"#ask", "#bid", "@-",
                          "15", "60", "@*", 
                          "@bucketed_avg", 0};
  SCOPED_TRACE("");
  checkGrammar("bucketed_avg( ask - bid, 15*60 )", parsed);
}

TEST(Parser, Expr_moving_avg_LP_gbp_Comma_20_RP_Minus_moving_avg_LP_gbp_Comma_50_RP) {
  const char* parsed[] = {"#gbp", "20", "@moving_avg", 
                          "#gbp", "50", "@moving_avg", 
                          "@-", 0};
  SCOPED_TRACE("");
  checkGrammar("moving_avg( gbp, 20 ) - moving_avg( gbp, 50 )", parsed);
}
