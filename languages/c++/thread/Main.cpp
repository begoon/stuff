#ifdef WIN32
#include <windows.h>
#define msleep(x) Sleep(x)
#else
#include <unistd.h>
#define msleep(x) usleep((x) * 1000)
#endif

#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <typeinfo>
#include <exception>
#include <cstdlib>
#include <ctime>
#include <vector>

#include "Thread.h"
#include "Mutex.h"

monitor::Mutex log;
int count;

class One : public monitor::Thread {
public:
	One(int id, int limit, int interval) :
		__id(id),
		__limit(limit),
		__interval(interval)
	{}

	virtual void Execute() {
		for (int i = 0; i < __limit; i++) {
			log.Lock();
			std::cout 
				<< " id: " << std::setw(5) << __id 
				<< " interval: " << std::setw(4) << __interval
				<< " limit: " << std::setw(4) << __limit
				<< " i: "  << std::setw(4) << i 
				<< " count: "  << std::setw(4) << count
				<< std::endl;
			log.Unlock();
			msleep(__interval);
		}
		delete this;
	}

	int id() { return __id; }
private:
	int __id;
	int __limit;
	int __interval;
};

int main(int argc, char* argv[])
{
	std::srand(std::time(0));

	std::vector< One* > threads;

	count = 3000;
	for (int i = 0; i < count; i++) {
		int id = i;
		int interval = std::rand() % 10;
		int limit = std::rand() % 10;
		threads.push_back(new One(id, limit, interval));
		threads.back()->Start();
	}

	for (std::vector<One*>::iterator i = threads.begin(); i != threads.end(); i++) {
		(*i)->Join();
		log.Lock();
		std::cout << "id : " << (*i)->id() << " done!" << std::endl;
		count -= 1;
		log.Unlock();
	}

	std::cout << "Count: " << count << std::endl;

	return 0;
}
