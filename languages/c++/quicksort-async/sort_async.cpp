#include <vector>                      
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <future>
#include <random>
#include <chrono>
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
  std::for_each(v.begin(), v.end(), [](Type& i) { i = uniform(engine); });
}

void check_sorted(const std::vector<Type>& v, const std::string& msg) {
  for (auto i = 0; i < v.size() - 1; ++i) {
    if (v[i] > v[i + 1]) {
      std::cout << "\nUnsorted: " << msg << "\n";
      std::cout << "\n" << i << "\n";
      std::cout << v[i] << " " << v[i + 1] << "\n";
      std::exit(1);
    }
  }
}

std::string data_file_name(const int i, const std::string& suffix) {
  std::ostringstream fmt;
  fmt << "trash_for_sort_" << i << suffix << ".bin";
  return fmt.str();
}

void save_file(std::vector<Type> array, const std::string& name) {
  std::for_each(array.begin(), array.end(), [](Type& i) { endian_swap(i); });
  std::ofstream os(name.c_str(), std::ios::binary|std::ios::out);
  auto const bytes_to_write = array.size() * sizeof(array[0]);
  std::cout << "Saving " << array.size() << " bytes to " << name << "\n";
  os.write((char *)&array[0], bytes_to_write);
}

int main_generate(int argc, char* argv[]) {
  std::cout << "Generation\n";
  for (auto i = 0; i < ITERATIONS_NUM; ++i) {
    std::vector<Type> unsorted(DATA_SIZE);
    generate(unsorted);
    save_file(unsorted, data_file_name(i, ""));
    std::cout << "Sorting...\n";
    std::sort(unsorted.begin(), unsorted.end());
    check_sorted(unsorted, "check sorted array");
    save_file(unsorted, data_file_name(i, "_sorted"));
  }
  return 0;
}

void load_file(std::vector<Type>& array, const std::string& name) {
  std::cout << "Loading " << name;
  array.resize(DATA_SIZE, 0);

  std::ifstream is(name.c_str(), std::ios::binary|std::ios::in);
  auto const to_load = array.size() * sizeof(array[0]);
  is.read((char *)&array[0], to_load);
  if (is.gcount() != to_load) {
    std::cerr << ", Bad file " << name
      << ", loaded " << is.gcount() << " words but should be " << to_load << "\n";
    std::exit(1);
  }
  std::for_each(array.begin(), array.end(), [](Type& v){ endian_swap(v); });
}

int naive_quick_sort(std::vector<Type>::iterator begin, std::vector<Type>::iterator end) {
  auto const sz = end - begin;
  if (sz <= 1) return 0;

  auto pivot = begin + sz/2;
  auto const pivot_v = *pivot;

  std::swap(*pivot, *(end - 1));
  auto p = std::partition(begin, end, [&](const Type& a) { return a < pivot_v; } );
  std::swap(*p, *(end - 1));

  if (sz > 4096) {
    auto left = std::async(std::launch::async, [&]() {
      return naive_quick_sort(begin, p);
    });
    naive_quick_sort(p + 1, end);
  } else {
    naive_quick_sort(begin, p);
    naive_quick_sort(p + 1, end);
  }
  return 0;
}

void quick_sort(std::vector<Type>& arr) {
  naive_quick_sort(arr.begin(), arr.end());
}

int main(int argc, char* argv[]) {
  if (argc == 2 && !std::strcmp(argv[1], "generate"))
    return main_generate(argc, argv);

  std::vector<double> times;
  auto times_sum = 0.0;
  for (auto i = 0; i < ITERATIONS_NUM; ++i) {
    std::vector<Type> unsorted;
    load_file(unsorted, data_file_name(i, ""));

    std::vector<Type> verify;
    std::cout << ", ";
    load_file(verify, data_file_name(i, "_sorted"));
    check_sorted(verify, "verify array");

    std::cout << ", Started";
    auto start = std::chrono::high_resolution_clock::now();

    quick_sort(unsorted);

    auto stop = std::chrono::high_resolution_clock::now();
    std::cout << ", Stopped, ";
    auto duration = std::chrono::duration<double>(stop - start).count();
    std::cout << duration;

    check_sorted(unsorted, "sorted array");

    const auto match = unsorted == verify;
    std::cout << (match ? ", OK" : ", DON'T MATCH");

    times.push_back(duration);
    times_sum += duration;

    std::cout << "\n";
  }

  auto const average = times_sum / ITERATIONS_NUM;
  auto const max_element = *std::max_element(times.begin(), times.end());
  auto const min_element = *std::min_element(times.begin(), times.end());
  auto const average_fixed = (times_sum - max_element - min_element) /
                             (ITERATIONS_NUM - 2);

  std::cout << "Average: " << average << "s, " 
            << "Average without max/min: "
            << average_fixed << "s." << std::endl;
}
