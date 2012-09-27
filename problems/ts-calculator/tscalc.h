#ifndef _TS_CALC
#define _TS_CALC

#include "expression.h"

#include "tick_stream.h"

#include <string>

class TsTime {
 public:
  explicit TsTime(double t) { value_ = t; }  
  explicit TsTime(const char* s) { fromString(s); }

  // Use compiler generated implementations.
  // TsTime(const TsTime&);
  // void operator(const TsTime&);

  double toDouble() const { return value_; }
  std::string toString() const;

 private:
  double value_;

  void fromString(const char* s);
};

class TsExpression {
 public:
  TsExpression(const char* expr, const TsTime& start, const TsTime& stop) :
    expr_(expr), start_(start), stop_(stop), time_("0"), value_(0),
    context_(0) {}

  bool first();
  bool next();

  const TsTime& time() const { return time_; }
  double value() const { return value_; }

 private:
  std::string expr_;
  TsTime start_, stop_;
  TsTime time_;
  double value_;
  expression::Context* context_;
  expression::Expression expression_;
  tick_stream::TickStream ticks_;
};

#endif

