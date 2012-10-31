using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace compressortest
{
	class Deflate
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

			byte[] packed = new com.jbase.framework.io.GZipCompressor().compress(raw.ToArray());

			System.Console.OpenStandardOutput().Write(packed, 0, packed.Length);
		}
	}
}
