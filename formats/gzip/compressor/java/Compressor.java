package com.jbase.framework.io;

import java.io.*;
import java.text.*;
import java.util.zip.*;

interface Compressor
{
	byte[] compress(byte[] data) throws java.io.IOException;
	byte[] uncompress(byte[] data) throws java.io.IOException;
}
