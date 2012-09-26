#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <utility>
#include <random>
#include <numeric>
#include <ctime>
#include <cstdlib>

const int ITERATIONS_NUM = 10;
const int DATA_SIZE = 50000000;

typedef __int64 Type;

std::tr1::uniform_int<Type> uniform(
  std::numeric_limits<Type>::min(),
  std::numeric_limits<Type>::max());
std::mt19937_64 engine;

inline void endian_swap(Type& x) {
  x =
    (0x00000000000000FF & (x >> 56))
  | (0x000000000000FF00 & (x >> 40))
  | (0x0000000000FF0000 & (x >> 24))
  | (0x00000000FF000000 & (x >>  8))
  | (0x000000FF00000000 & (x <<  8))
  | (0x0000FF0000000000 & (x << 24))
  | (0x00FF000000000000 & (x << 40))
  | (0xFF00000000000000 & (x << 56));
}

void generate(std::vector<Type>& v) {
  std::for_each(v.begin(), v.end(), [](decltype(v[0]) &i) {
    i = uniform(engine); 
  });
}

void save_file(std::vector<Type> array, const std::string& name) {
  std::for_each(array.begin(), array.end(), [](decltype(array[0]) &i) {
    endian_swap(i);
  });
  std::ofstream os(name.c_str(), std::ios::binary|std::ios::out);
  const size_t bytes_to_write = array.size() * sizeof(array[0]);
  std::cout << "Saving " << array.size() << " bytes to " << name << std::endl;
  os.write((char *)&array[0], bytes_to_write);
}

void check_sorted(const std::vector<Type> v) {
  for (size_t i = 0; i < v.size() - 1; ++i) {
    if (v[i] > v[i + 1]) {
      std::cout << std::endl << "Unsorted" << std::endl;
      std::cout << std::endl << i << std::endl;
      std::cout << v[i] << " " << v[i + 1] << std::endl;
      std::exit(1);
    }
  }
}

int main(int argc, char argv[]) {
  std::vector<int> times;
  double times_sum = 0;
  for (int i = 0; i < ITERATIONS_NUM; ++i) {
    std::ostringstream fmt;
    fmt << "trash_for_sort_" << i << ".bin";
    const std::string name = fmt.str();

    std::vector<Type> unsorted(DATA_SIZE);
    generate(unsorted);
    save_file(unsorted, name);

    fmt.str("");
    fmt << "trash_for_sort_" << i << "_sorted.bin";
    const std::string name_sorted = fmt.str();

    std::cout << "Sorting..." << std::endl;
    std::sort(unsorted.begin(), unsorted.end());
    check_sorted(unsorted);
    save_file(unsorted, name_sorted);
  }
  return 0;
}
