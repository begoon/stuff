#include "compressor.h"

#include <sstream>
#include <iomanip>

std::string GZipCompressor::Error::__msg;

const char* GZipCompressor::Error::what() const throw()
{
	__msg = msg();
	return __msg.c_str();
}

std::string GZipCompressor::BadHeader::msg() const
{
	return "Bad GZIP header";
}

std::string GZipCompressor::ZLibError::msg() const
{
	std::stringstream formatter;
	formatter << "ZLIB error, code=" << __status;
	return formatter.str();
}

std::string GZipCompressor::CrcError::msg() const
{
	std::stringstream formatter;
	formatter 
		<< "CRC error, must be " 
		<< std::hex << std::setw(8) << std::setfill('0') << std::uppercase << __real_crc 
		<< " but actual value is " 
		<< std::hex << std::setw(8) << std::setfill('0') << std::uppercase << __actual_crc;
	return formatter.str();
}

std::string GZipCompressor::compress(const std::string& data)
{
	z_stream stream;

	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.next_in = (Bytef *)data.data();
	stream.avail_in = (uInt)data.length();

	stream.avail_out = stream.avail_in + (stream.avail_in / ZLIB_MODIFIER) + 15 + 1; /* room for \0 */

	char* buf = new char[stream.avail_out + GZIP_HEADER_LENGTH + GZIP_FOOTER_LENGTH];

	/* add gzip file header */
	buf[0] = GZ_MAGIC_0;
	buf[1] = GZ_MAGIC_1;
	buf[2] = Z_DEFLATED;
	buf[3] = buf[4] = buf[5] = buf[6] = buf[7] = buf[8] = 0; /* time set to 0 */
	buf[9] = OS_CODE;

	stream.next_out = (Bytef *)(buf + GZIP_HEADER_LENGTH);

	std::string result;

	int status;

	/* windowBits is passed < 0 to suppress zlib header & trailer */
	if ((status = deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY)) == Z_OK) {

		status = deflate(&stream, Z_FINISH);
		if (status != Z_STREAM_END) {
			deflateEnd(&stream);
			if (status == Z_OK) {
				status = Z_BUF_ERROR;
			}
		} else {
			status = deflateEnd(&stream);
		}

		if (status == Z_OK) 
		{
			char* trailer = buf + stream.total_out + GZIP_HEADER_LENGTH;

			uLong crc = crc32(0L, Z_NULL, 0);
			crc = crc32(crc, (const Bytef *) data.data(), (uInt)data.length());

			trailer[0] = (char) crc & 0xFF;
			trailer[1] = (char) (crc >> 8) & 0xFF;
			trailer[2] = (char) (crc >> 16) & 0xFF;
			trailer[3] = (char) (crc >> 24) & 0xFF;
			trailer[4] = (char) stream.total_in & 0xFF;
			trailer[5] = (char) (stream.total_in >> 8) & 0xFF;
			trailer[6] = (char) (stream.total_in >> 16) & 0xFF;
			trailer[7] = (char) (stream.total_in >> 24) & 0xFF;

			result.assign(buf, buf + stream.total_out + GZIP_HEADER_LENGTH + GZIP_FOOTER_LENGTH);
		}
	}

	delete buf;

	if (status != Z_OK)
		throw ZLibError(status);

	return result;
}

std::string GZipCompressor::uncompress(const std::string& data)
{
	z_stream stream;

	const unsigned char* raw = (const unsigned char *)data.data();

	if (raw[0] != GZ_MAGIC_0 || raw[1] != GZ_MAGIC_1 || raw[2] != Z_DEFLATED)
	{
		throw BadHeader();
	}

	stream.zalloc = Z_NULL;
	stream.zfree = Z_NULL;
	stream.opaque = Z_NULL;

	stream.next_in = (Bytef *)raw + GZIP_HEADER_LENGTH;
	stream.avail_in = (uInt)data.length() - GZIP_HEADER_LENGTH - GZIP_FOOTER_LENGTH;

	stream.avail_out = (raw[data.length() - 1] << 24) | (raw[data.length() - 2] << 16) | (raw[data.length() - 3] << 8) | raw[data.length() - 4];

	char* buf = new char[stream.avail_out + 1];

	stream.next_out = (Bytef *)buf;

	std::string result;

	int status;

	/* windowBits is passed < 0 to suppress zlib header & trailer */
	if ((status = inflateInit2(&stream, -MAX_WBITS)) == Z_OK) 
	{
		status = inflate(&stream, Z_FINISH);
		if (status != Z_STREAM_END) {
			inflateEnd(&stream);
			if (status == Z_OK) {
				status = Z_BUF_ERROR;
			}
		} else {
			status = inflateEnd(&stream);
		}

		if (status == Z_OK)
		{
			uLong actual_crc = crc32(0L, Z_NULL, 0);
			actual_crc = crc32(actual_crc, (const Bytef *)buf, stream.total_out);

			uLong crc = (raw[data.length() - 5] << 24) | (raw[data.length() - 6] << 16) | (raw[data.length() - 7] << 8) | raw[data.length() - 8];

			if (crc != actual_crc)
			{
				delete buf;
				throw CrcError(crc, actual_crc);
			}

			result.assign(buf, buf + stream.total_out);
		}
	}

	delete buf;

	if (status != Z_OK)
		throw ZLibError(status);

	return result;
}
