void v() {}
void f(){ 
  return v();
}

int main() {
  f();
}
