#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <utility>
#include <numeric>
#include <thread>
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

#undef max

int main(int argc, char argv[]) {
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
    std::vector<Type> etalon(unsorted);

    std::vector<Type> verify;
    std::cout << ", ";
    load_file(verify, name_sorted);
    check_sorted(verify, "verify array");

    std::vector<Type> merged(unsorted.size(), 1);

    std::cout << ", Started";
    clock_t started = clock() / (CLOCKS_PER_SEC / 1000);

    const size_t chunks = 4;
    const size_t chunk_sz = unsorted.size() / chunks;
    std::vector<std::thread> workers;
    for (size_t i = 0, offset = 0; i < chunks; ++i, offset += chunk_sz) {
      workers.emplace_back(
        std::thread(std::bind(
          [&unsorted](size_t from, size_t to) {
            std::sort(unsorted.begin() + from, unsorted.begin() + to);
          },
          offset, offset + chunk_sz)));
    }

    std::for_each(workers.begin(), workers.end(), [](std::thread& w){ w.join(); });

    std::vector<int> chunk_sizes(chunks, chunk_sz);
    std::vector<std::vector<Type>::iterator> offset(chunks);
    for (size_t i = 0; i < offset.size(); ++i)
      offset[i] = unsorted.begin() + i * chunk_sz;

    for (std::vector<Type>::iterator r = merged.begin(); r != merged.end(); ++r) {
      size_t min_i;
      Type min = std::numeric_limits<Type>::max();
      std::vector<std::vector<Type>::iterator>::const_iterator j = offset.begin();
      for (size_t i = 0; i < offset.size(); ++i, ++j) {
        if (chunk_sizes[i] < 1 || **j >= min) continue;
        min_i = i;
        min = **j;
      }
      *r = min;
      offset[min_i] += 1;
      chunk_sizes[min_i] -= 1;
    }

    clock_t finished = clock() / (CLOCKS_PER_SEC / 1000);

    unsorted.swap(merged);

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
