#ifndef _TS_FUNCTION
#define _TS_FUNCTION

#include <string>
#include <vector>
#include <queue>

namespace function {

typedef std::vector<double> Arguments;

class Function {
 public:
  virtual std::string name() const = 0;
  virtual int nb_args() const = 0;
  double call(const Arguments& args);
 protected:
  virtual double calc(const Arguments& args) = 0;
};

class BucketedAverage: public Function {
 public:
  virtual std::string name() const { return "bucketed_avg"; }
  virtual int nb_args() const { return 2; }
 private:
  virtual double calc(const Arguments& args);
};

class MovingAverage: public Function {
 public:
  MovingAverage() : sum_(0) {}
  virtual std::string name() const { return "moving_avg"; }
  virtual int nb_args() const { return 2; }
 private:
  std::queue<double> window_;
  double sum_;
  virtual double calc(const Arguments& args);
};

class Average: public Function {
 public:
  virtual std::string name() const { return "average"; }
  virtual int nb_args() const { return 2; }
 private:
  virtual double calc(const Arguments& args);
};

class Plus: public Function {
 public:
  virtual std::string name() const { return "+"; }
  virtual int nb_args() const { return 2; }
 private:
  virtual double calc(const Arguments& args);
};

class Minus: public Function {
 public:
  virtual std::string name() const { return "-"; }
  virtual int nb_args() const { return 2; }
 private:
  virtual double calc(const Arguments& args);
};

class Times: public Function {
 public:
  virtual std::string name() const { return "*"; }
  virtual int nb_args() const { return 2; }
  virtual double calc(const Arguments& args);
};

class Divide: public Function {
 public:
  virtual std::string name() const { return "/"; }
  virtual int nb_args() const { return 2; }
 private:
  virtual double calc(const Arguments& args);
};
} // function

#endif

