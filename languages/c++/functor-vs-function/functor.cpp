#include "gtest/gtest.h"

#include <algorithm>

typedef double Type;

Type* array;
const int N = 100000000;

inline bool less(Type a1, Type a2) {
  return a1 < a2;
}

class Less {
 public:
  inline bool operator()(Type a1, Type a2) {
    return a1 < a2;
  }
};

TEST(Callback, BuiltIn) {
  std::sort(array, array + N);
}

TEST(Callback, Function) {
  std::sort(array, array + N, less);
}

TEST(Callback, Functor) {
  std::sort(array, array + N, Less());
}

int main(int argc, char* argv[]) {
  // Создаем отсортированный массив.
  array = new Type[N];
  Type* p = array;
  for (int i = 0; i < N; ++i) *p++ = i;

  testing::InitGoogleTest(&argc, argv);
  // Принудительно печатаем время работы тестов.
  testing::GTEST_FLAG(print_time) = true;
  return RUN_ALL_TESTS();
}
