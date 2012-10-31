using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ZlibTest
{
	class Deflate
	{
		static void Main(string[] args)
		{
			System.IO.Stream packer = new System.IO.Compression.GZipStream(
				System.Console.OpenStandardOutput(), 
				System.IO.Compression.CompressionMode.Compress
			);

			MemoryStream buf = new MemoryStream();

			System.IO.Stream stdin = System.Console.OpenStandardInput();
			byte[] chunk = new byte[10240];

			int n = stdin.Read(chunk, 0, chunk.Length);
			while (n > 0)
			{
				buf.Write(chunk, 0, n);
				n = stdin.Read(chunk, 0, chunk.Length);
			}

			packer.Write(buf.GetBuffer(), 0, (int)buf.Length);
			packer.Close();
		}
	}
}
