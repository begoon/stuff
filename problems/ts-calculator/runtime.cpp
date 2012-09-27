#include "runtime.h"

namespace runtime {

double CallbackDispatcher::call(const std::string& name, 
                                const function::Arguments& args) {
  Callbacks::iterator i = callbacks_.find(name);
  if (i == callbacks_.end())
    throw UnknownFunction(name);
  assert(i->second->nb_args() == args.size());
  return i->second->call(args);
};

int CallbackDispatcher::nb_args(const std::string& name) {
  Callbacks::iterator i = callbacks_.find(name);
  if (i == callbacks_.end())
    throw UnknownFunction(name);
  return i->second->nb_args();
};
} // runtime
