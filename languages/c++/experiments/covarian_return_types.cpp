class T {};
class D: public T {};

class A {
 public:
  virtual long& f() {}
};

class B: public A {
 public:
  virtual int& f() {}
};
