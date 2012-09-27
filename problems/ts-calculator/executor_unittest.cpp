#include <gtest/gtest.h>

#include "executor.h"
#include "parser.h"
#include "runtime.h"
#include "expression.h"

using namespace executor;

void checkExecution(const char* snippet[], 
                    const expression::Context& context, double expected) {
  size_t n = 0;
  while (snippet[n]) ++n;
  grammar::Program program(snippet, snippet + n);
  runtime::CallbackDispatcher dispatcher;
  Executor executor;

  EXPECT_EQ(expected, executor.run(program, dispatcher, context));
}

TEST(Executor, RunFromParsedExpression) {
  const char* cmd[] = {"10", "#eur", "@+", "11", "@average", "#gbp", "@+", 0};
  expression::Context context;
  context["eur"] = 20.0;
  checkExecution(cmd, context, 20.5);
}
