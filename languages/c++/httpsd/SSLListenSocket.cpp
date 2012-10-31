#include "SSLListenSocket.h"
#include "SSLSocket.h"
#include "SSLError.h"

#include <iostream>

namespace monitor {

SSLListenSocket::SSLListenSocket(SSLContext* ctx) :
	__ctx(ctx)
{}

SSLListenSocket::~SSLListenSocket() 
{}

Socket* SSLListenSocket::accept() {	
	//std::cerr << "Before do_SSL_accept" << std::endl;
	int sock = do_accept();
	//std::cerr << "After do_SSL_accept" << std::endl;
	SSLSocket* peer = new SSLSocket(__ctx, sock, true);

	//std::cerr << "Before SSL_accept" << std::endl;
	int err = SSL_accept(peer->ssl());
	//std::cerr << "After SSL_accept" << std::endl;
	if (err != 1) {
		ListenSocket::do_disconnect(sock);
		throw SSLError(SSL_get_error(peer->ssl(), err), "SSLListenSocket::accept::SSL_accept()");
	}

	return peer;
}

} // monitor
