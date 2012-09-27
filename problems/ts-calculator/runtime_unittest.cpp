#include "gtest/gtest.h"

#include "runtime.h"

using namespace runtime;

TEST(RuntimeCallbackDispatcher, UnknownFunction) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    r.call("func", args);
    FAIL();
  } catch (CallbackDispatcher::UnknownFunction& e) {
    EXPECT_EQ(std::string("func"), std::string(e.what()));
  }
}

TEST(RuntimeCallbackDispatcher, BacketedAverage) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("bucketed_avg", args);
    EXPECT_EQ(2, r.nb_args("bucketed_avg"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, MovingAverage) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(1);
    r.call("moving_avg", args);
    EXPECT_EQ(2, r.nb_args("moving_avg"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, Plus) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("+", args);
    EXPECT_EQ(2, r.nb_args("+"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, Minus) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("-", args);
    EXPECT_EQ(2, r.nb_args("-"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, Times) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("*", args);
    EXPECT_EQ(2, r.nb_args("*"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, Divide) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("/", args);
    EXPECT_EQ(2, r.nb_args("/"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}

TEST(RuntimeCallbackDispatcher, Average) {
  runtime::CallbackDispatcher r;
  try {
    function::Arguments args;
    args.push_back(0);
    args.push_back(0);
    r.call("average", args);
    EXPECT_EQ(2, r.nb_args("average"));
  } catch (CallbackDispatcher::UnknownFunction& e) {
    FAIL() << std::string(e.what());
  }
}
