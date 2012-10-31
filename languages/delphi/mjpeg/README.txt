MJPEG Decoder Class
Version 1.0.0
Copyright 2006, Steve Blinch
http://code.blitzaffe.com
============================================================================

DESCRIPTION

Provides a native Delphi implementation of an MJPEG movie decoder.  MJPEG
movies are commonly used by web cams and/or web cam software to display
image streams.

This class uses the Indy component library to make an HTTP connection to an
MJPEG movie source on the network, then displays the movie in a standard
TImage component.  All movie retrieval and processing tasks are performed
asynchronously in a separate thread, so they should not interfere with the
rest of your application, or cause your application to block.

This class has been tested with MJPEG streams created by the Motion software
motion detector at: http://www.lavrsen.dk/twiki/bin/view/Motion/WebHome



PREREQUISITES

Requires the Indy socket component library.  Has only been tested with
Delphi 7 but may work with other Delphi versions as well.



USAGE

1. Add the MJPEGDecoderUnit unit to your "uses" clause.  
2. Create an instance of TMJPEGDecoder.
3. Set the OnFrame property of your TMJPEGDecoder instance to a callback
   which will receive each frame received from the MJPEG movie.
4. Optionally set the OnError and OnMessage properties for debugging
   purposes.
5. Setup your "OnFrame" callback to assign each frame to a TImage.
6. Call the Connect method of your TMJPEGDecoder instance to connect to
   your movie source.  Pass the hostname, port, and URI of the movie as
   arguments.
7. Call the Disconnect method to disconnect at any time.   



EXAMPLE

Refer to the MJPEGTest project for an example.



LICENSE

This script is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This script is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along
with this script; if not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
