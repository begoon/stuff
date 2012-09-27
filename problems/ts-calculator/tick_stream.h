#ifndef _TS_TICKSTREAM
#define _TS_TICKSTREAM

#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <ios>
#include <map>

#include "executor.h"
#include "expression.h"

namespace tick_stream {

class TickStream {
 public:

  TickStream() : start_(0), stop_(0), time_(0) {}

  typedef struct {
    double time;
    double value;
  } Tick;

  typedef std::vector<Tick> Ticks;

  typedef struct {
    size_t position;
    Ticks ticks;
  } TicksInfo;

  typedef std::map<std::string, TicksInfo> Cache;

  void loadData(const expression::Symbols& symbols, 
                double start, double stop);

  expression::Context* first();
  expression::Context* next();

  double time() const { return time_; }
 private:
  Cache cache_;
  expression::Context context_;

  void loadSymbol(const std::string& name, double start, double stop);
  bool readTick(std::istream& is, Tick* tick);

  double start_;
  double stop_;

  double time_;
};

} // tick_stream

#endif

