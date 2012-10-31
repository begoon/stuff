using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ZlibTest
{
	class Inflate
	{
		static void Main(string[] args)
		{
			System.IO.Stream unpacker = new System.IO.Compression.GZipStream(
				System.Console.OpenStandardInput(),
				System.IO.Compression.CompressionMode.Decompress
			);

			System.IO.Stream stdout = System.Console.OpenStandardOutput();

			byte[] chunk = new byte[10240];

			try 
			{
				int n = unpacker.Read(chunk, 0, chunk.Length);
				while (n > 0)
				{
					stdout.Write(chunk, 0, n);
					n = unpacker.Read(chunk, 0, chunk.Length);
				}
			} catch (System.IO.InvalidDataException e) 
			{
			}
		}
	}
}
