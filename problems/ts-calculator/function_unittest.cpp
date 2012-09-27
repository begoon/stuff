#include "gtest/gtest.h"

#include "function.h"

using namespace function;

TEST(Function, BucketedAverage) {
  BucketedAverage f;
  EXPECT_EQ(std::string("bucketed_avg"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(2.2);  
  args.push_back(3.3);  
  EXPECT_DOUBLE_EQ(5.5, f.call(args));
}

TEST(Function, MovingAverage) {
  MovingAverage f;
  EXPECT_EQ(std::string("moving_avg"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(2.2);  
  args.push_back(1);  
  EXPECT_DOUBLE_EQ(2.2, f.call(args));
}

TEST(Function, Average) {
  Average f;
  EXPECT_EQ(std::string("average"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(1);  
  args.push_back(2);  
  EXPECT_DOUBLE_EQ(1.5, f.call(args));
}

TEST(Function, Plus) {
  Plus f;
  EXPECT_EQ(std::string("+"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(1);  
  args.push_back(2);  
  EXPECT_DOUBLE_EQ(3, f.call(args));
}

TEST(Function, Minus) {
  Minus f;
  EXPECT_EQ(std::string("-"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(1);  
  args.push_back(2);  
  EXPECT_DOUBLE_EQ(-1, f.call(args));
}

TEST(Function, Times) {
  Times f;
  EXPECT_EQ(std::string("*"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(3);  
  args.push_back(2);  
  EXPECT_DOUBLE_EQ(6, f.call(args));
}

TEST(Function, Divide) {
  Divide f;
  EXPECT_EQ(std::string("/"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(1);  
  args.push_back(2);  
  EXPECT_DOUBLE_EQ(0.5, f.call(args));
}

TEST(Function, DivideByZero) {
  Divide f;
  EXPECT_EQ(std::string("/"), f.name());
  EXPECT_EQ(2, f.nb_args());
  Arguments args;
  args.push_back(1);  
  args.push_back(0);  
  // TODO: This assert fails and needs to be fixed.
  // EXPECT_DOUBLE_EQ(0, f.call(args));
}
