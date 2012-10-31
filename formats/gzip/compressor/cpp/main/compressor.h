#ifndef __COMPRESSOR_H
#define __COMPRESSOR_H

#include <string>
#include <exception>

#include "zlib.h"

class Compressor
{
public:
	virtual std::string compress(const std::string& data) = 0;
	virtual std::string uncompress(const std::string& data) = 0;
};

class GZipCompressor
{
public:
	virtual std::string compress(const std::string& data);
	virtual std::string uncompress(const std::string& data);

	class Error: public std::exception 
	{
	public:
		virtual const char* what() const throw();
	protected:
		virtual std::string msg() const = 0;
	private:
		static std::string __msg;
	};

	class BadHeader: public Error
	{
	public:
		virtual std::string msg() const;
	};

	class ZLibError: public Error
	{
	public:
		ZLibError(int status) : __status(status) {}
		virtual std::string msg() const;
	private:
		int __status;
	};

	class CrcError: public Error
	{
	public:
		CrcError(uLong real_crc, uLong actual_crc) : 
		  __real_crc(real_crc), __actual_crc(actual_crc)
		{}
		virtual std::string msg() const;
	private:
		uLong __real_crc;
		uLong __actual_crc;
	};

private:	
#ifdef WIN32
	static const int ZLIB_MODIFIER = 100;
#else
	static const int ZLIB_MODIFIER = 1000;
#endif

	static const unsigned char GZ_MAGIC_0 = 0x1f;
	static const unsigned char GZ_MAGIC_1 = 0x8b;

	static const unsigned char OS_CODE = 0x03;

	static const int GZIP_HEADER_LENGTH = 10;
	static const int GZIP_FOOTER_LENGTH = 8;
};

#endif
