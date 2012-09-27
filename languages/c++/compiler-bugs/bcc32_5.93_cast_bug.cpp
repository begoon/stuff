class A {};
class C {};
A* a;
A* b = static_cast<C*>(a);
