#include <vector>                      
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <utility>
#include <numeric>
#include <future>
#include <random>
#include <ctime>
#include <cstdlib>

const int ITERATIONS_NUM = 10;
const int DATA_SIZE = 50000000;

typedef __int64 Type;

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

std::tr1::uniform_int<Type> uniform(
  std::numeric_limits<Type>::min(),
  std::numeric_limits<Type>::max());
std::mt19937_64 engine;

void generate(std::vector<Type>& v) {
  std::for_each(v.begin(), v.end(), [](decltype(v[0]) &i) {
    i = uniform(engine); 
  });
}

void check_sorted(const std::vector<Type> v, const std::string& msg) {
  for (size_t i = 0; i < v.size() - 1; ++i) {
    if (v[i] > v[i + 1]) {
      std::cout << std::endl << "Unsorted: " << msg << std::endl;
      std::cout << std::endl << i << std::endl;
      std::cout << v[i] << " " << v[i + 1] << std::endl;
      std::exit(1);
    }
  }
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

int main_generate(int argc, char* argv[]) {
  std::cout << "Generation" << std::endl;
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
    check_sorted(unsorted, name_sorted);
    save_file(unsorted, name_sorted);
  }
  return 0;
}

void load_file(std::vector<Type>& array, const std::string& name) {
  std::cout << "Loading " << name;
  array.resize(DATA_SIZE, 0);

  std::ifstream is(name.c_str(), std::ios::binary|std::ios::in);
  const size_t to_load = array.size() * sizeof(array[0]);
  is.read((char *)&array[0], to_load);
  if (is.gcount() != to_load) {
    std::cerr << ", Bad file " << name
      << ", loaded " << is.gcount() << " words but should be " << to_load
      << std::endl;
    std::exit(1);
  }
  std::for_each(array.begin(), array.end(), [](decltype(array[0]) & v){
    endian_swap(v);
  });
}

template <typename T>
int naive_quick_sort(typename T::iterator begin, typename T::iterator end) {
  auto const sz = end - begin;
  if (sz <= 1) return 0;

  auto pivot = begin + sz/2;
  auto const pivot_v = *pivot;

  std::swap(*pivot, *(end - 1));
  auto p = std::partition(begin, end, [&](const Type& a) { return a < pivot_v; } );
  std::swap(*p, *(end - 1));

  if (false && sz > 4096) {
    auto left = std::async(std::launch::async, [&]() {
      return naive_quick_sort<T>(begin, p);
    });
    naive_quick_sort<T>(p + 1, end);
  } else {
    naive_quick_sort<T>(begin, p);
    naive_quick_sort<T>(p + 1, end);
  }
  return 0;
}

void quick_sort(std::vector<Type>& arr) {
  naive_quick_sort<std::vector<Type>>(arr.begin(), arr.end());
}

#undef max

int main(int argc, char* argv[]) {
  if (argc == 2 && !std::strcmp(argv[1], "generate"))
    return main_generate(argc, argv);

  std::vector<int> times;
  double times_sum = 0;
  for (int i = 0; i < ITERATIONS_NUM; ++i) {
    std::ostringstream fmt;
    fmt << "trash_for_sort_" << i << ".bin";
    const std::string name = fmt.str();
    fmt.str("");
    fmt << "trash_for_sort_" << i << "_sorted.bin";
    const std::string name_sorted = fmt.str();

    std::vector<Type> unsorted;
    load_file(unsorted, name);

    std::vector<Type> verify;
    std::cout << ", ";
    load_file(verify, name_sorted);
    check_sorted(verify, "verify array");

    std::cout << ", Started";
    clock_t started = clock() / (CLOCKS_PER_SEC / 1000);

    quick_sort(unsorted);

    clock_t finished = clock() / (CLOCKS_PER_SEC / 1000);

    std::cout << ", Stopped, ";
    int duration = finished - started;
    std::cout << "Duration = " << duration;

    check_sorted(unsorted, "sorted array");

    const bool match = unsorted == verify;
    std::cout << (match ? ", OK" : ", DON'T MATCH");

    times.push_back(duration);
    times_sum += duration;

    std::cout << std::endl;
  }

  double average = times_sum / ITERATIONS_NUM;
  int max_element = *std::max_element(times.begin(), times.end());
  int min_element = *std::min_element(times.begin(), times.end());
  double average_fixed = (times_sum - max_element - min_element) / (ITERATIONS_NUM - 2);

  std::cout << "Average: " << average << "ms, " 
            << "Average without max/min: "
            << average_fixed << "ms." << std::endl;

  return 0;
}
