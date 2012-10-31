package com.jbase.framework.io;

import java.io.*;
import java.text.*;
import java.util.zip.*;

public class GZipCompressor implements com.jbase.framework.io.Compressor
{
	public byte[] compress(byte[] data) throws java.io.IOException
	{
		ByteArrayOutputStream packed = new ByteArrayOutputStream();
		
		GZIPOutputStream packer = new GZIPOutputStream(packed);
		
		packer.write(data, 0, data.length);
		packer.finish();
		
		return packed.toByteArray();
	} 
	
	public byte[] uncompress(byte[] data) throws java.io.IOException	
	{
		ByteArrayOutputStream unpacked = new ByteArrayOutputStream();
		
		InputStream unpacker = new GZIPInputStream(new ByteArrayInputStream(data));
		
		byte[] chunk = new byte[10240];
		int n = unpacker.read(chunk, 0, chunk.length);
		while (n > 0)
		{
			unpacked.write(chunk, 0, n);
			n = unpacker.read(chunk, 0, chunk.length);
		}
		
		return unpacked.toByteArray();
	}
}
