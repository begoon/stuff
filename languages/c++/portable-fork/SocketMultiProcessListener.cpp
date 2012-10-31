#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/wait.h>
#endif

#include <string>
#include <sstream>

#include "ListenSocket.h"
#include "SystemMessage.h"
#include "Thread.h"
#include "Socket.h"

#include "SocketMultiProcessListener.h"

class Child: public monitor::Thread {
public:
	Child(
		monitor::Socket* peer, monitor::ListenSocket* listener,
		SocketMultiProcessListener* parent
	) :
		__peer(peer), __listener(listener),
		__parent(parent),
		__error_code(0)
	{}

	int error_code() { return __error_code; }
	const std::string& error_message() { return __error_message; }
	
	int error();
	
	virtual void Execute();

private:
	monitor::Socket* __peer;
	monitor::ListenSocket* __listener;
	SocketMultiProcessListener* __parent;
	
	int __error_code;
	std::string __error_message;
};

SocketMultiProcessListener::SocketMultiProcessListener() :
	__ready(false)
{}

std::string SocketMultiProcessListener::id() const {
	return std::string("--child__") + name();
}

void SocketMultiProcessListener::child(int argc, char** argv) {
	for (int i = 0; i < argc - 1; i++)
		if (std::string(argv[i]) == id())
			action(std::atoi(argv[i+1]));
}

void SocketMultiProcessListener::listen(int port, const char* host) {

	monitor::ListenSocket* listener = new monitor::ListenSocket();

	listener->listen(port, host);

	__ready = true;

	while (true) {
		monitor::Socket* peer = listener->accept();
		if (!peer) break;
		(new Child(peer, listener, this))->Start();
	}	
}

void SocketMultiProcessListener::action(int sock) {
	monitor::Socket* peer = new monitor::Socket(sock);
	action(peer);
}

void SocketMultiProcessListener::action(monitor::Socket* peer) {
	int error = execute(peer);
	peer->shutdown();
	peer->disconnect();
	delete peer;
	std::exit(error);
}

void Child::Execute() {
#ifdef WIN32
	STARTUPINFOW siStartupInfo;
	PROCESS_INFORMATION piProcessInfo;
	std::memset(&siStartupInfo, 0, sizeof(siStartupInfo));
	std::memset(&piProcessInfo, 0, sizeof(piProcessInfo));
	siStartupInfo.cb = sizeof(siStartupInfo);

	std::string cmdline = GetCommandLine();
	std::string exe = cmdline.substr(0, cmdline.find(" "));

	std::stringstream fmt;
	fmt << exe << " " << __parent->id() << " " << __peer->handle();

	if (CreateProcess(
		exe.c_str(), (LPSTR)fmt.str().c_str(), 0, 0, true, 
		CREATE_DEFAULT_ERROR_MODE, 0, 0,
		(LPSTARTUPINFO)&siStartupInfo, &piProcessInfo
	) == false) {
		error();
		return;
	}

	WaitForSingleObject(piProcessInfo.hProcess, INFINITE);
	
	CloseHandle(piProcessInfo.hThread);
	CloseHandle(piProcessInfo.hProcess);
#else
	int pid = fork();

	if (pid < 0) {
		error();
		return;
	}

	if (pid == 0) {
		// the child closes the listener, does job and exists
		delete __listener;
		__parent->action(__peer);
	}

	// the parent waits until the child terminates
	waitpid(pid, 0, 0);
#endif
	__peer->shutdown();
	__peer->disconnect();

	delete __peer;

	delete this;
}

int Child::error() {
	__error_code = monitor::SystemMessage::code();
	__error_message = monitor::SystemMessage::message(__error_code);
	return __error_code;
}
