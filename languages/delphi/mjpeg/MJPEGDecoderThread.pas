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

unit MJPEGDecoderThread;

interface

uses Classes,IdTCPClient;

const
  MSG_MJPEG_FRAME   = 1;
  MSG_MJPEG_ERROR   = 2;
  MSG_MJPEG_MESSAGE = 3;

  ERROR_MJPEG_BADFRAME    = 1;
  ERROR_MJPEG_EXCEPT      = 2;
  ERROR_MJPEG_DISCONNECT  = 3;

Type
  TMJPEGDecoderMsg = class(TObject)
      public
        Msg: String;
    end;

  TMJPEGDecoderThread = class(TThread)
    private
      FClient: TIdTCPClient;
      FActive: Boolean;
      FNotifyWindow: THandle;

      procedure Info(S: String);
      function CheckConnected: Boolean;
    public
      constructor Create(TCPClient: TIdTCPClient; NotifyWindow: THandle);
      procedure Execute; override;
    published
      property Active: Boolean read FActive;
    end;

implementation

uses SysUtils,Windows,Messages;

constructor TMJPEGDecoderThread.Create(TCPClient: TIdTCPClient; NotifyWindow: THandle);
begin
  FActive:=False;
  FClient:=TCPClient;
  FNotifyWindow:=NotifyWindow;
  
  inherited Create(True);
  FreeOnTerminate:=True;
end;

function TMJPEGDecoderThread.CheckConnected: Boolean;
begin
  if not FClient.Connected then
    begin
      PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_ERROR,ERROR_MJPEG_DISCONNECT);
      Terminate;
    end;
  Result:=FClient.Connected;
end;

procedure TMJPEGDecoderThread.Execute;
var
  S: String;
  CLength: Integer;
  JPEG: TMemoryStream;
begin
  FActive:=True;
  try
    try

      CLength:=0;
      Info('Decoder thread starting execution loop');
      while (not Terminated) do
        begin
          if not CheckConnected then break;

          // grab a header
          Info('Waiting for data...');
          try
            S:=Trim(LowerCase(FClient.ReadLn));
          except
            if (not CheckConnected) then break;
            PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_ERROR,ERROR_MJPEG_EXCEPT);
            Terminate;
            break;
          end;
          Info('Data: '+S);

          if not CheckConnected then break;

          // if it's a content-length line, record the content length
          if (Copy(S,1,15)='content-length:') then
            begin
              Delete(S,1,15);
              S:=Trim(S);
              CLength:=StrToIntDef(S,0);
              Info('Next frame content length: '+IntToStr(CLength));
            end;

          // if it's a blank line and we have a content-length, then we're good to receive the stream
          if (Length(S)=0) and (CLength>0) then
            begin
              Info('Receiving frame');
              JPEG:=TMemoryStream.Create;
              JPEG.SetSize(CLength);
              try
                FClient.ReadStream(JPEG,CLength);
                PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_FRAME,Integer(JPEG));
              except
                PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_ERROR,ERROR_MJPEG_BADFRAME);
                Terminate;
                break;
              end;
              Info('Frame received');
              CLength:=0;
            end;

        end;
      Info('Decoder thread execution loop complete');
    except
      on E: Exception do
        begin
          PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_ERROR,ERROR_MJPEG_EXCEPT);
        end;
    end;
  finally
    FActive:=False;
  end;

end;

procedure TMJPEGDecoderThread.Info(S: String);
var Msg: TMJPEGDecoderMsg;
begin
  exit;
  Msg:=TMJPEGDecoderMsg.Create;
  Msg.Msg:=S;
  PostMessage(FNotifyWindow,WM_USER,MSG_MJPEG_MESSAGE,Integer(Msg));
end;

end.
 