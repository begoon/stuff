import java.io.*;
import java.text.*;
import java.util.zip.*;

class Deflate
{
	static public void main(String argv[])
	{
		try 
		{
			ByteArrayOutputStream unpacked = new ByteArrayOutputStream();
			
			// Read data from standard input into memory
			byte[] chunk = new byte[102400];
			int n = System.in.read(chunk, 0, chunk.length);
			while (n > 0)
			{
				unpacked.write(chunk, 0, n);
				n = System.in.read(chunk, 0, chunk.length);
			}
			
			byte[] packed = new com.jbase.framework.io.GZipCompressor().compress(unpacked.toByteArray());

			System.out.write(packed, 0, packed.length);
		} 
		catch (java.io.IOException e)
		{
		}
	}
}
