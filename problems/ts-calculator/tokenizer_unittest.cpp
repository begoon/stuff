#include "gtest/gtest.h"
#include <sstream>

#include "tokenizer.h"

using namespace grammar;

void checkNextToken(int token, const char* value, Tokenizer& tokenizer) {
  Token next = tokenizer.next();
  EXPECT_EQ(token, next.token);
  EXPECT_STREQ(value, next.value);
}

TEST(Tokenizer, Expr_eur) {
  try {
    std::stringstream fmt;
    fmt.str("eur");
    Tokenizer tokenizer(fmt);
    checkNextToken(SYMBOL, "eur", tokenizer);
  } catch (const Tokenizer::eof&) {
    FAIL();
  } catch (const Tokenizer::unknown& e) {
    FAIL() << e.what();
  }
}

TEST(Tokenizer, Expr_1_Div_eur) {
  try {
    std::stringstream fmt;
    fmt.str("1 / eur");
    Tokenizer tokenizer(fmt);
    checkNextToken(CONSTANT, "1", tokenizer);
    checkNextToken(DIVIDE, "/", tokenizer);
    checkNextToken(SYMBOL, "eur", tokenizer);
  } catch (const Tokenizer::eof&) {
    FAIL();
  } catch (const Tokenizer::unknown& e) {
    FAIL() << e.what();
  }
}

TEST(Tokenizer, Expr_LP_bid_Plus_ask_RP_Div_2) {
  try {
    std::stringstream fmt;
    fmt.str("( bid + ask ) / 2");
    Tokenizer tokenizer(fmt);
    checkNextToken(LPARENT, "(", tokenizer);
    checkNextToken(SYMBOL, "bid", tokenizer);
    checkNextToken(PLUS, "+", tokenizer);
    checkNextToken(SYMBOL, "ask", tokenizer);
    checkNextToken(RPARENT, ")", tokenizer);
    checkNextToken(DIVIDE, "/", tokenizer);
    checkNextToken(CONSTANT, "2", tokenizer);
  } catch (const Tokenizer::eof&) {
    FAIL();
  } catch (const Tokenizer::unknown& e) {
    FAIL() << e.what();
  }
}

TEST(Tokenizer, Expr_bucketed_avg_LP_ask_Minus_bid_Comma_15_Times_60_RP) {
  try {
    std::stringstream fmt;
    fmt.str("bucketed_avg( ask - bid, 15*60 )");
    Tokenizer tokenizer(fmt);
    checkNextToken(SYMBOL, "bucketed_avg", tokenizer);
    checkNextToken(LPARENT, "(", tokenizer);
    checkNextToken(SYMBOL, "ask", tokenizer);
    checkNextToken(MINUS, "-", tokenizer);
    checkNextToken(SYMBOL, "bid", tokenizer);
    checkNextToken(COMMA, ",", tokenizer);
    checkNextToken(CONSTANT, "15", tokenizer);
    checkNextToken(TIMES, "*", tokenizer);
    checkNextToken(CONSTANT, "60", tokenizer);
    checkNextToken(RPARENT, ")", tokenizer);
  } catch (const Tokenizer::eof&) {
    FAIL();
  } catch (const Tokenizer::unknown& e) {
    FAIL() << e.what();
  }
}

TEST(Tokenizer, Expr_moving_avg_LP_gbp_Comma_20_RP_Minus_moving_avg_LP_gbp_Comma_50_RP) {
  try {
    std::stringstream fmt;
    fmt.str("moving_avg( gbp, 20 ) - moving_avg( gbp, 50 )");
    Tokenizer tokenizer(fmt);
    checkNextToken(SYMBOL, "moving_avg", tokenizer);
    checkNextToken(LPARENT, "(", tokenizer);
    checkNextToken(SYMBOL, "gbp", tokenizer);
    checkNextToken(COMMA, ",", tokenizer);
    checkNextToken(CONSTANT, "20", tokenizer);
    checkNextToken(RPARENT, ")", tokenizer);
    checkNextToken(MINUS, "-", tokenizer);
    checkNextToken(SYMBOL, "moving_avg", tokenizer);
    checkNextToken(LPARENT, "(", tokenizer);
    checkNextToken(SYMBOL, "gbp", tokenizer);
    checkNextToken(COMMA, ",", tokenizer);
    checkNextToken(CONSTANT, "50", tokenizer);
    checkNextToken(RPARENT, ")", tokenizer);
  } catch (const Tokenizer::eof&) {
    FAIL();
  } catch (const Tokenizer::unknown& e) {
    FAIL() << e.what();
  }
}

