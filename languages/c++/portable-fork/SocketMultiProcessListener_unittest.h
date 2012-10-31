#ifndef _MONITOR_SOCKET_MULTIPROCESS_LISTENER_THREAD_H
#define _MONITOR_SOCKET_MULTIPROCESS_LISTENER_THREAD_H

#ifdef WIN32
#include <windows.h>
#define msleep(x) Sleep(x)
#else
#include <unistd.h>
#define msleep(x) usleep((x)*1000)
#endif

#include <string>

#include "SocketMultiProcessListener.h"

class SimpleClient: public SocketMultiProcessListener {
public:
	SimpleClient() {
		__echo = "CLIENT";
	}

	virtual std::string name() const { return "Simple"; }

	virtual int execute(monitor::Socket* peer) {
		peer->write(__echo.c_str(), __echo.length());
		return 0;
	}

	const std::string data() { return __echo; }

private:
	std::string __echo;
};

class SequenceClient: public SocketMultiProcessListener {
public:
	SequenceClient() {}

	virtual std::string name() const { return "Sequence"; }

	virtual int execute(monitor::Socket* peer) {
		for (int i = 0; i < 256; i++) {
			char ch = i & 0xff;
			peer->write(&ch, 1);
		}
		return 0;
	}

	const std::string data() { return __echo; }

private:
	std::string __echo;
};

class EchoClient: public SocketMultiProcessListener {
public:
	EchoClient() {}

	virtual std::string name() const { return "Echo"; }

	virtual int execute(monitor::Socket* peer) {
		while (true) {
			char ch;
			if (peer->read(&ch, 1) != 1) break;
			peer->write(&ch, 1);
		}
		return 0;
	}
};

#endif
