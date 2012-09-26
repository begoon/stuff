template <typename T>
string itoa(T a) {
  stringstream fmt;
  fmt << a;
  return fmt.str();
}
