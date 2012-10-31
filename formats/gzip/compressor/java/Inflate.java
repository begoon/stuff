import java.io.*;
import java.text.*;
import java.util.zip.*;

class Inflate
{
	static public void main(String argv[])
	{
		try 
		{
			ByteArrayOutputStream packed = new ByteArrayOutputStream();
			
			// Read data from standard input into memory
			byte[] chunk = new byte[102400];
			int n = System.in.read(chunk, 0, chunk.length);
			while (n > 0)
			{
				packed.write(chunk, 0, n);
				n = System.in.read(chunk, 0, chunk.length);
			}
			
			byte[] unpacked = new com.jbase.framework.io.GZipCompressor().uncompress(packed.toByteArray());

			System.out.write(unpacked, 0, unpacked.length);
		} 
		catch (java.io.IOException e)
		{
		}
	}
}
