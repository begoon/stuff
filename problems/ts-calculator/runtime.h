#ifndef _TS_RUNTIME
#define _TS_RUNTIME

#include <string>
#include <vector>
#include <map>
#include <exception>
#include <cassert>

#include "function.h"

namespace runtime {

class Dispatcher {
 public:
  virtual double call(const std::string& name, 
                      const function::Arguments& args) = 0;
  virtual int nb_args(const std::string& name) = 0;
};

class CallbackDispatcher: public Dispatcher {
 public:
  typedef std::map<std::string, function::Function*> Callbacks;

  CallbackDispatcher() {
    add(new function::BucketedAverage());
    add(new function::MovingAverage());
    add(new function::Plus());
    add(new function::Minus());
    add(new function::Times());
    add(new function::Divide());
    add(new function::Average());
  }

  virtual ~CallbackDispatcher() {
    for (Callbacks::iterator i = callbacks_.begin(); 
                             i != callbacks_.end(); ++i) {
      delete i->second;
    }
  }

  class UnknownFunction: public std::exception {
   public:
    UnknownFunction(const std::string& msg) : std::exception(msg.c_str()) {}
  };

  virtual double call(const std::string& name, 
                      const function::Arguments& args);
  virtual int nb_args(const std::string& name);

 private:
   void add(function::Function* function) {
    callbacks_[function->name()] = function;
  }

  Callbacks callbacks_;
};

} // runtime

#endif
