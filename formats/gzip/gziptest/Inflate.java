import java.io.*;
import java.text.*;
import java.util.zip.*;

class Inflate
{
	static public void main(String argv[])
	{
		try 
		{
			InputStream unpacker = new GZIPInputStream(System.in);
			
			byte[] chunk = new byte[10240];
			int n = unpacker.read(chunk, 0, chunk.length);
			while (n > 0)
			{
				System.out.write(chunk, 0, n);
				n = unpacker.read(chunk, 0, chunk.length);
			}
		} 
		catch (java.io.IOException e)
		{
		}
	}
}