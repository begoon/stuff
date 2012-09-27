#ifndef _TS_EXECUTOR
#define _TS_EXECUTOR

#include <string>
#include <vector>
#include <iostream>
#include <sstream>
#include <exception>

#include "parser.h"
#include "runtime.h"
#include "expression.h"

namespace executor {

namespace {
template <typename T> 
T fromString(const std::string& g) {
  std::istringstream is(g.c_str());
  double v;
  is >> v;
  return v;
}
} // anonymous namespace

class Executor {
 public:

  double run(const grammar::Program& program, 
             runtime::Dispatcher& dispatcher,
             const expression::Context& context);

  class EmptyStack: public std::exception {
   public:
    EmptyStack(const std::string& msg) : std::exception(msg.c_str()) {}
  };
};

} // executor

#endif
