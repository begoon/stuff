#include "tscalc.h"
#include "expression.h"
#include "executor.h"

#include <sstream>

void TsTime::fromString(const char* s) {
  std::istringstream is(s);
  is >> value_;
}

std::string TsTime::toString() const {
  std::ostringstream os;
  os << value_;
  return os.str();
}

using tick_stream::TickStream;
using expression::Expression;
using expression::Context;

bool TsExpression::first() {
  expression_.compile(expr_.c_str());
  ticks_.loadData(expression_.symbols(), start_.toDouble(), stop_.toDouble());
  return next();
}

bool TsExpression::next() {
  context_ = ticks_.first();
  if (!context_) return false;
  value_ = expression_.run(*context_);
  time_ = TsTime(ticks_.time());
  return true;
}

