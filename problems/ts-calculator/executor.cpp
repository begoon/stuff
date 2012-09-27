#include "executor.h"
#include "expression.h"
#include "parser.h"

#include <set>
#include <stack>
#include <cassert>

namespace executor {

double Executor::run(const grammar::Program& program, 
                     runtime::Dispatcher& dispatcher,
                     const expression::Context& context) {
  std::stack<double> stack;
  for (grammar::Program::const_iterator i = program.begin(); 
                                        i != program.end(); ++i) {
    const std::string g = *i;
    assert(!g.empty());
    if (g[0] == '@') {
      const std::string func = g.substr(1);
      int nb_args = dispatcher.nb_args(func);
      function::Arguments args(nb_args);
      for (function::Arguments::reverse_iterator j = args.rbegin(); 
                                                 j != args.rend(); ++j) {
        if (stack.empty())
          throw EmptyStack(func);
        *j = stack.top();
        stack.pop();
      }
      stack.push(dispatcher.call(func, args));
    } else if (g[0] == '#') {
      const std::string name = g.substr(1);
      expression::Context::const_iterator j = context.find(name);
      double value = j != context.end() ? j->second : 0.0;
      stack.push(value);
    } else {
      stack.push(fromString<double>(g));
    }
  }
  assert(stack.size() == 1);
  double result = stack.top();
  stack.pop();
  return result;
}

} // executor
