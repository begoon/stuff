#include <gtest/gtest.h>

#include <fstream>
#include <sstream>
#include <iostream>
#include <string>
#include <algorithm>
#include <stdexpept>

#include "Socket.h"
#include "Http.h"

class MockWriteSocket: public monitor::Socket {
public:
	virtual void write(const char* buf, size_t sz) {
		__buf.append(buf, sz);
	}
	virtual void write(const std::string& str) {
		write(str.data(), str.length());
	}
	const std::string& buf() const { return __buf; }
private:
	std::string __buf;
};

namespace monitor {
namespace http {

class Page {
public:
	virtual void process(Socket& peer, const std::string& args = "") = 0;
	virtual const std::string& name() = 0;
	virtual ~Page() = 0;
};

class StaticPage {
public:
	StaticPage(const std::string& name, const std::string& content) :
		__name(name),
		__content(content)
	{}
	virtual void process(Socket& peer, const std::string& args = "") {
		peer.write(__content);
	}
	const std::string& content() const { return __content; }
	const std::string& name() const { return __name; }
private:
	std::string __name;
	std::string __content;
};

class Directory {
public:
	class BadURL: public std::logic_error {
		BadURL(const std::string& origin, const std::string& url) :
			std::logic_error(origin + ", " + "Malformed URL: " + url)
		{}
	};

	Directory(const std::string& url) {
	}
};

class PageDirectory {
public:
	PageDirectory() {
	}

	virtual ~PageDirectory() {
		std::for_each(__pages.begin(), __pages.end(), deleter());
	}
private:
	class deleter {
	public:
		template <typename T>
		void operator()(const T* p) const {	delete p; }
	};

	std::vector<Page *> __pages;
};

} // monitor
} // http

TEST(HTTP, PageSimple) {
	monitor::http::StaticPage page("", "I'm a simple page");
	MockWriteSocket peer;
	
	page.process(peer);

	EXPECT_EQ(page.content(), peer.buf()); 
}
