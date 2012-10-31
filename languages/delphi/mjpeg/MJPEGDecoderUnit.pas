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

unit MJPEGDecoderUnit;

interface

uses Classes, ExtCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, MJPEGDecoderThread, Messages, JPEG;

type
  TMJPEGFrameEvent = procedure(Sender: TObject; Frame: TJPEGImage) of object;
  TMJPEGMessageEvent = procedure(Sender: TObject; Msg: String) of object;
  
  TMJPEGDecoder = class(TObject)
    private
      FOnFrame: TMJPEGFrameEvent;
      FOnError: TMJPEGMessageEvent;
      FOnMessage: TMJPEGMessageEvent;
      FOnConnected: TNotifyEvent;
      FOnDisconnected: TNotifyEvent;

      FConnected: Boolean;

      FOwner: TComponent;

      ClientURI: String;
      Client: TIdTCPClient;

      FHeaders: TStringList;

      FWindowHandle: THandle;

      Thread: TMJPEGDecoderThread;
      ThreadActive: Boolean;

      procedure HandleConnected(Sender: TObject);
      procedure HandleThreadTerminate(Sender: TObject);

      function IsMixedReplace(CType: String): Boolean;
      procedure InternalWinProc(var Msg: TMessage);

      procedure Error(Msg: String);
      procedure Info(Msg: String);

    public
      constructor Create(AOwner: TComponent);
      destructor Destroy; override;

      // connect to Hostname on port Port, and fetch the MJPEG stream from URI
      procedure Connect(Hostname: String; Port: Integer; URI: String);

      // disconnect (if connected)
      procedure Disconnect;
    published
      // called when a frame is received; you MUST dispose of the frame yourself!
      property OnFrame: TMJPEGFrameEvent read FOnFrame write FOnFrame;

      // called when an error occurs
      property OnError: TMJPEGMessageEvent read FOnError write FOnError;

      // called when a debug message is generated
      property OnMessage: TMJPEGMessageEvent read FOnMessage write FOnMessage;

      // called when MJPEGDecoder connects to an MJPEG source
      property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;

      // called when MJPEGDecoder disconnects
      property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;

      // contains the headers received from the HTTP server
      property Headers: TStringList read FHeaders;

      // returns TRUE if connected, otherwise FALSE
      property Connected: Boolean read FConnected;
    end;

implementation

uses SysUtils;

procedure TMJPEGDecoder.InternalWinProc(var Msg: TMessage);
var
  JPEGImage: TJPEGImage;
  JPEGStream: TMemoryStream;
begin
  if (Msg.Msg = WM_USER) then
    begin
      case Msg.WParam of
          MSG_MJPEG_ERROR:
            begin
              Error('Decoder thread error: '+IntToStr(Msg.LParam));
              try
                Client.Disconnect;
              except;
              end;
              FConnected:=False;
            end;
          MSG_MJPEG_MESSAGE:
            begin
              with TMJPEGDecoderMsg(Msg.LParam) do
                begin
                  Info(msg);
                  Free;
                end;
            end;
          MSG_MJPEG_FRAME:
            begin
              JPEGStream:=TMemoryStream(Msg.LParam);
              JPEGStream.Position:=0;

              if Assigned(FOnFrame) then
                begin
                  JPEGImage:=TJPEGImage.Create;
                  JPEGImage.LoadFromStream(JPEGStream);

                  FOnFrame(Self,JPEGImage);
                end;

              JPEGStream.Free;
            end;
        end;
    end;
end;


constructor TMJPEGDecoder.Create(AOwner: TComponent);
begin
  inherited Create;
  
  FOwner:=AOwner;

  FOnFrame:=nil;
  FOnConnected:=nil;
  FOnDisconnected:=nil;

  Thread:=nil;
  ThreadActive:=False;

  FWindowHandle := AllocateHWnd(InternalWinProc);

  FHeaders:=TStringList.Create;
  FHeaders.NameValueSeparator:=':';
  Client:=TIdTCPClient.Create(FOwner);
end;

destructor TMJPEGDecoder.Destroy;
begin
  if Assigned(Thread) and ThreadActive then Thread.Terminate;
  
  FHeaders.Free;
  Client.Free;
  inherited;
end;

procedure TMJPEGDecoder.HandleThreadTerminate(Sender: TObject);
begin
  ThreadActive:=False;
end;


procedure TMJPEGDecoder.Connect(Hostname: String; Port: Integer; URI: String);
begin
  if FConnected then exit;

  Client.Host:=Hostname;
  Client.Port:=Port;
  ClientURI:=URI;
  FHeaders.Clear;
  
  Client.OnConnected:=HandleConnected;
  try
    Client.Connect(10);
  except
    if not Client.Connected then Error('Could not connect to server');
  end;

end;

procedure TMJPEGDecoder.Disconnect;
begin
  if not FConnected then exit;

  Thread.Terminate;
  Client.Disconnect;
  if (FConnected) and Assigned(FOnDisconnected) then FOnDisconnected(Self);
  FConnected:=False;
end;

procedure TMJPEGDecoder.HandleConnected(Sender: TObject);
var
  S: String;
  CType: String;

begin
  Info('Connected, sending request');
  Client.WriteLn('GET '+ClientURI+' HTTP/1.1');
  Client.WriteLn;
  while (true) do
    begin
      S:=Client.ReadLn();
      if (S='') then break;

      FHeaders.Add(S)
    end;
  CType:=FHeaders.Values['Content-Type'];
  if (not IsMixedReplace(CType)) then
    begin
      Error('Invalid content type received from server: '+CType);
      Error(FHeaders.Text);
      Disconnect;
      exit;
    end;

  Info('Request sent, spawning decoder thread');
  Thread:=TMJPEGDecoderThread.Create(Client,FWindowHandle);
  Thread.OnTerminate:=HandleThreadTerminate;
  ThreadActive:=True;
  Thread.Resume;
  FConnected:=True;
end;

function TMJPEGDecoder.IsMixedReplace(CType: String): Boolean;
var p: Integer;
begin
  CType:=Trim(Ctype);
  p:=Pos(';',CType);
  if (p>0) then SetLength(CType,p-1);

  Result:=(CType='multipart/x-mixed-replace');
end;

procedure TMJPEGDecoder.Error(Msg: String);
begin
  if Assigned(FOnError) then FOnError(Self,Msg);
end;

procedure TMJPEGDecoder.Info(Msg: String);
begin
  if Assigned(FOnMessage) then FOnMessage(Self,Msg);
end;

end.

