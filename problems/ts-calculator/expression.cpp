#include "expression.h"

#include "parser.h"
#include "executor.h"
#include "runtime.h"

#include <set>
#include <string>
#include <cassert>

namespace expression {

void Expression::compile(const char* expression) {
  std::stringstream fmt;
  fmt.str(expression);

  grammar::Tokenizer tokenizer(fmt);
  grammar::Parser parser(fmt, tokenizer);
  parser.parse(&program_);

  parse_symbols();
}

double Expression::run(const Context& context) {
  runtime::CallbackDispatcher dispatcher;
  executor::Executor executor;
  return executor.run(program_, dispatcher, context);
}

void Expression::parse_symbols() {
  std::set<std::string> used;
  symbols_.clear();
  using grammar::Program;
  for (Program::const_iterator i = program_.begin();
                               i != program_.end(); ++i) {
    const std::string& g = *i;
    assert(!g.empty());
    if (g[0] != '#') continue;
    const std::string name = g.substr(1);
    if (used.find(name) != used.end()) continue;
    used.insert(name);
    symbols_.push_back(name);
  }
}

} // expression

