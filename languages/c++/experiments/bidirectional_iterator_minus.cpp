struct T {
  T& operator--() { return *this; }
};

// T& operator--(T& a) { return a; }

T end() { return T(); }

int main() {
  --end();
}

