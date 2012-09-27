#include <tscalc.h>
#include <iostream>
 
int main(int argc, char const* argv[]) {
  if (argc != 4) {
    std::cerr << "Usage: " << argv[0] << " \"expression\" start_time end_time" 
              << std::endl;
    return 1;
  }

  TsTime start(argv[2]), end(argv[3]);
  TsExpression expr(argv[1], start, end);
 
  bool run = expr.first();
  while (run) {
    const TsTime& t = expr.time();
    double v = expr.value();
    std::cout << t.toString() << ": " << v << std::endl;
    run = expr.next();
  }

  return 0;
}

