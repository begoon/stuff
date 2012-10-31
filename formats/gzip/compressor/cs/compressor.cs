using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace com.jbase.framework.io
{
	interface Compressor
	{
		byte[] compress(byte[] data);
		byte[] uncompress(byte[] data);
	}

	class GZipCompressor: Compressor
	{
		public byte[] compress(byte[] data)
		{
			MemoryStream packed = new MemoryStream();

			System.IO.Stream packer = new System.IO.Compression.GZipStream(
				packed, 
				System.IO.Compression.CompressionMode.Compress
			);

			packer.Write(data, 0, data.Length);
			packer.Close();

			return packed.ToArray();
		}

		public byte[] uncompress(byte[] data)
		{
			MemoryStream packed = new MemoryStream(data);

			System.IO.Stream unpacker = new System.IO.Compression.GZipStream(
				packed, 
				System.IO.Compression.CompressionMode.Decompress
			);

			MemoryStream unpacked = new MemoryStream();

			byte[] chunk = new byte[10240];

			int n = unpacker.Read(chunk, 0, chunk.Length);
			while (n > 0)
			{
				unpacked.Write(chunk, 0, n);
				n = unpacker.Read(chunk, 0, chunk.Length);
			}

			return unpacked.ToArray();
		}
	}
}
