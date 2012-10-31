import java.io.*;
import java.text.*;
import java.util.zip.*;

class Deflate
{
	static public void main(String argv[])
	{
		try 
		{
			ByteArrayOutputStream unpacked_data = new ByteArrayOutputStream();
			
			// Read data from standard input into memory
			byte[] chunk = new byte[102400];
			int n = System.in.read(chunk, 0, chunk.length);
			while (n > 0)
			{
				unpacked_data.write(chunk, 0, n);
				n = System.in.read(chunk, 0, chunk.length);
			}
			
			GZIPOutputStream packer = new GZIPOutputStream(System.out);
			
			byte[] data = unpacked_data.toByteArray();
			packer.write(data, 0, data.length);
			packer.finish();
		} 
		catch (java.io.IOException e)
		{
		}
	}
}