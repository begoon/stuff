{
MJPEG Decoder Class
Copyright 2006, Steve Blinch
http://code.blitzaffe.com

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
}

unit mainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JPEG, MJPEGDecoderUnit;

type
  TForm1 = class(TForm)
    Image: TImage;
    Button1: TButton;
    Notices: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    MJPEGDecoder: TMJPEGDecoder;
    { Private declarations }
    procedure HandleFrame(Sender: TObject; Frame: TJPEGImage);
    procedure HandleError(Sender: TObject; Error: String);
    procedure HandleMessage(Sender: TObject; Msg: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (MJPEGDecoder.Connected) then
    begin
      MJPEGDecoder.Disconnect;
    end
    else
    begin
      MJPEGDecoder.OnFrame:=HandleFrame;
      MJPEGDecoder.OnError:=HandleError;
      MJPEGDecoder.OnMessage:=HandleMessage;

      // put your own IP address, port number, and stream URI here
      MJPEGDecoder.Connect('192.168.0.10',8081,'/');
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MJPEGDecoder:=TMJPEGDecoder.Create(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MJPEGDecoder.Free;
end;

// called every time a JPEG frame is received from the server; you can do
// whatever you like with the frame
procedure TForm1.HandleFrame(Sender: TObject; Frame: TJPEGImage);
begin
  // display the JPEG image in our TImage
  Image.Picture.Bitmap.Assign(Frame);

  // you MUST free Frame before returning or your application will
  // leak memory like a sieve
  Frame.Free;
end;

// called when an error occurs; Error contains the error message
procedure TForm1.HandleError(Sender: TObject; Error: String);
begin
  Notices.Lines.Add('**** '+Error)
end;

// called when a debug notice occurs; Msg contains the debug message
procedure TForm1.HandleMessage(Sender: TObject; Msg: String);
begin
  Notices.Lines.Add(Msg)
end;

end.
