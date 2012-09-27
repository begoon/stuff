#ifndef _TS_EXPRESSION
#define _TS_EXPRESSION

#include "parser.h"

#include <map>
#include <string>

namespace expression {

typedef std::map<std::string, double> Context;
typedef std::vector<std::string> Symbols;

class Expression {
 public:

  void compile(const char* expression);
  double run(const Context& context);

  const Symbols& symbols() const { return symbols_; }

 private:
  grammar::Program program_;
  Symbols symbols_;

  void parse_symbols();
};

} // expression

#endif
