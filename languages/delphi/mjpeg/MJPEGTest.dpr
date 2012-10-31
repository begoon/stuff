program MJPEGTest;

uses
  Forms,
  mainUnit in 'mainUnit.pas' {Form1},
  MJPEGDecoderUnit in 'MJPEGDecoderUnit.pas',
  MJPEGDecoderThread in 'MJPEGDecoderThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
