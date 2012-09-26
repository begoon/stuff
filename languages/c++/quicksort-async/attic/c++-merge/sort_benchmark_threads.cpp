#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <iomanip>
#include <utility>
#include <numeric>
#include <ctime>
#include <cstdlib>

const int ITERATIONS_NUM = 10;
const int DATA_SIZE = 50000000;

#ifdef WIN32
#include <windows.h>
typedef HANDLE ThreadType;
#else
#include <pthread.h>
#include <signal.h>
typedef pthread_t ThreadType;
#endif

class Thread {
 public:
  Thread();
  ~Thread();

  void Start();
  virtual void Execute() = 0;

  void Join();
 private:
  ThreadType handle_;
};

Thread::Thread() : handle_(0) {}

static void ThreadCallback(Thread* who) {
#ifndef WIN32
  int old_thread_type;
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old_thread_type);
#endif
  who->Execute();
}

#ifdef WIN32

Thread::~Thread() { CloseHandle(handle_); }

void Thread::Start() {
  handle_ = CreateThread(
    0, 0,
    reinterpret_cast<LPTHREAD_START_ROUTINE>(ThreadCallback), this,
    0, 0);
}

void Thread::Join() { WaitForSingleObject(handle_,  INFINITE); }

#else

Thread::~Thread() {}

extern "C"
typedef void *(*pthread_callback)(void *);  // NOLINT, unnamed parameter

void Thread::Start() {
  pthread_create(&handle_, 0,
                 reinterpret_cast<pthread_callback>(ThreadCallback),
                 this);
}

void Thread::Join() { pthread_join(handle_, 0); }

#endif

typedef __int64 Type;
typedef unsigned __int64 UnsignedType;

inline void endian_swap(const UnsignedType& x, Type& to) {
  UnsignedType v =
    (0x00000000000000FF & (x >> 56))
  | (0x000000000000FF00 & (x >> 40))
  | (0x0000000000FF0000 & (x >> 24))
  | (0x00000000FF000000 & (x >>  8))
  | (0x000000FF00000000 & (x <<  8))
  | (0x0000FF0000000000 & (x << 24))
  | (0x00FF000000000000 & (x << 40))
  | (0xFF00000000000000 & (x << 56));
  to = static_cast<Type>(v);
}

template <typename T>
std::ostream& hex(std::ostream& os, const T& value) {
  os << std::hex << std::uppercase << std::setw(16) << std::setfill('0');
  os << value;
  return os;
}

template <typename T>
std::ostream& dec(std::ostream& os, const T& value) {
  os << std::dec << std::uppercase << std::setfill('0');
  os << value;
  return os;
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
  for (size_t i = 0; i < array.size(); ++i) {
    const UnsignedType v = array[i];
    endian_swap(v, array[i]);
  }
}

void save_file(const std::vector<Type>& array, const std::string& name) {
  std::vector<Type> v(array);
  for (size_t i = 0; i < v.size(); ++i) {
    const UnsignedType t = v[i];
    endian_swap(t, v[i]);
  }
  std::ofstream os(name.c_str(), std::ios::binary|std::ios::out);
  const size_t to_write = v.size() * sizeof(v[0]);
  std::cout << "Saving " << std::dec << v.size() << " bytes to " << name << std::endl;
  os.write((char *)&v[0], to_write);
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

class QuickSortThread: public Thread {
  public:
    QuickSortThread(std::vector<Type>& v, size_t from, size_t to) :
      v_(v), from_(from), to_(to) {}
    void Execute() {
      std::sort(v_.begin() + from_, v_.begin() + to_);
    }
  private:
   std::vector<Type>& v_;
   size_t from_, to_;
};

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
    check_sorted(verify);

    std::vector<Type> merged(unsorted.size(), 1);

    const size_t chunks = 4;
    const size_t chunk_sz = unsorted.size() / chunks;
    std::vector<QuickSortThread*> workers;
    for (size_t i = 0, offset = 0; i < chunks; ++i, offset += chunk_sz) {
      QuickSortThread* worker = new QuickSortThread(unsorted, offset, offset + chunk_sz);
      workers.push_back(worker);
    }

    std::cout << ", Started";
    clock_t started = clock() / (CLOCKS_PER_SEC / 1000);

    for (std::vector<QuickSortThread*>::iterator i = workers.begin();
         i != workers.end(); ++i)
      (*i)->Start();

    for (std::vector<QuickSortThread*>::iterator i = workers.begin();
         i != workers.end(); ++i)
      (*i)->Join();

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

    check_sorted(unsorted);

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
