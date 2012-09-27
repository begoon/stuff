#include "tick_stream.h"

namespace tick_stream {

using expression::Symbols;

void TickStream::loadData(const Symbols& symbols, double start, double stop) {
  start_ = start;
  stop_ = stop;
  cache_.clear();
  for (Symbols::const_iterator i = symbols.begin(); i != symbols.end(); ++i) {
    loadSymbol(*i, start, stop);
  }
}

void TickStream::loadSymbol(const std::string& name, 
                            double start, double stop) {
  std::pair<Cache::iterator, bool> i = 
    cache_.insert(std::make_pair(name, TicksInfo()));
  i.first->second.position = 0;
  Ticks& ticks = (i.first)->second.ticks;
  std::ifstream os((std::string("data/") + name).c_str(), 
                   std::ios::in | std::ios::binary);
  Tick tick;
  while (readTick(os, &tick)) {
    if (tick.time < start || tick.time > stop) continue;
    ticks.push_back(tick);
  }
}

bool TickStream::readTick(std::istream& is, Tick* tick) {
  is.read(reinterpret_cast<char*>(&tick->time), sizeof(tick->time));
  if (is.gcount() != sizeof(tick->time)) return false;
  is.read(reinterpret_cast<char*>(&tick->value), sizeof(tick->value));
  if (is.gcount() != sizeof(tick->value)) return false;
  return true;
}

expression::Context* TickStream::first() {
  context_.clear();
  bool has_data = false;
  time_ = start_;
  for (Cache::iterator i = cache_.begin(); i != cache_.end(); ++i) {
    TicksInfo& info = i->second;
    while (info.position < info.ticks.size() && 
           info.ticks[info.position].time < start_) {
      info.position += 1;
    }
    if (info.position < info.ticks.size()) {
      has_data = true;
      context_[i->first] = info.ticks[info.position].value;
      if (i == cache_.begin() || time_ < info.ticks[info.position].time)
        time_ = info.ticks[info.position].value;
    }
  }
  return has_data ? &context_ : 0;
}

expression::Context* TickStream::next() {
  bool has_data = false;

  double min_time = 0;
  std::string min_name;
  for (Cache::iterator i = cache_.begin(); i != cache_.end(); ++i) {
    TicksInfo& info = i->second;
    if (i == cache_.begin() || 
       (info.position < info.ticks.size() && 
        info.ticks[info.position].time < min_time)) {
      min_time = info.ticks[info.position].time;
      min_name = i->first;
    }
  }

  for (Cache::iterator i = cache_.begin(); i != cache_.end(); ++i) {
    if (i->first != min_name) continue;
    TicksInfo& info = i->second;
    info.position += 1;
    if (info.position < info.ticks.size() && 
        info.ticks[info.position].time < stop_) {
      has_data = true;
      context_[i->first] = info.ticks[info.position].value;
     time_ = info.ticks[info.position].value;
    }
  }

  return has_data ? &context_ : 0;
}

} // tick_stream

