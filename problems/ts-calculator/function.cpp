#include "function.h"

#include <cassert>

namespace function {

double Function::call(const Arguments& args) {
  assert(args.size() == nb_args());
  return calc(args);
}

double BucketedAverage::calc(const Arguments& args) {
  // TODO: not implemented yet.
  // This implementation only sums the arguments.
  return args[0] + args[1];
}

double MovingAverage::calc(const Arguments& args) {
  double v = args[0];
  size_t sz = (int)(args[1] + 1e-9);
  while (window_.size() >= sz) {
    sum_ -= window_.front();
    window_.pop();
  }
  window_.push(v);
  sum_ += v;
  return sum_ / sz;
}

double Average::calc(const Arguments& args) {
  return (args[0] + args[1]) / 2.0;
}

double Plus::calc(const Arguments& args) {
  return args[0] + args[1];
}

double Minus::calc(const Arguments& args) {
  return args[0] - args[1];
}
double Times::calc(const Arguments& args) {
  return args[0] * args[1];
}
double Divide::calc(const Arguments& args) {
  return args[0] / args[1];
}

} // function
