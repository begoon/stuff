using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace compressortest
{
	class Inflate
	{
		static void Main(string[] args)
		{
			MemoryStream raw = new MemoryStream();

			Stream stdin = System.Console.OpenStandardInput();
			byte[] chunk = new byte[10240];

			int n = stdin.Read(chunk, 0, chunk.Length);
			while (n > 0)
			{
				raw.Write(chunk, 0, n);
				n = stdin.Read(chunk, 0, chunk.Length);
			}

			byte[] unpacked = new com.jbase.framework.io.GZipCompressor().uncompress(raw.ToArray());

			System.Console.OpenStandardOutput().Write(unpacked, 0, unpacked.Length);
		}
	}
}
