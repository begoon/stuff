//---------------------------------------------------------------------------

#include <vcl.h>
#include <IdTCPClient.hpp>
#include <ctime>
#pragma hdrstop

#include "main.h"
#include "xportgpio.h"
#include "formload.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CPortCtl"
#pragma link "SHDocVw_OCX"
#pragma resource "*.dfm"
TMainForm *MainForm;

// XPortGPIO xportgpio( "localhost", 0x77F0, 50000 );
XPortGPIO xportgpio( "localhost" );

//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
{
   xportgpio.set_host( XPortIPLabeledEdit->Text );   
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led1GetButtonClick(TObject *Sender)
{
   int v = xportgpio.GetCurrentStates();
   ComLed1->State = v & 1 ? lsOn : lsOff;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led2GetButtonClick(TObject *Sender)
{
   int v = xportgpio.GetCurrentStates();
   ComLed2->State = v & 2 ? lsOn : lsOff;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led3GetButtonClick(TObject *Sender)
{
   int v = xportgpio.GetCurrentStates();
   ComLed3->State = v & 4 ? lsOn : lsOff;
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led1OutSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 1, 1 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led1InSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 1, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led2OutSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 2, 1 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led2InSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 2, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led3OutSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 4, 1 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led3InSpeedButtonClick(TObject *Sender)
{
   xportgpio.SetDirections( 4, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led11ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 1, 1 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led10ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 1, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led21ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 2, 2 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led20ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 2, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led31ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 4, 4 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::Led30ButtonClick(TObject *Sender)
{
   xportgpio.SetStates( 4, 0 );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::XPortIPLabeledEditChange(TObject *Sender)
{
   xportgpio.set_host( XPortIPLabeledEdit->Text );   
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCreate(TObject *Sender)
{
   FormLoader::restore(this);
   CppWebBrowser->Navigate( (BSTR)WideString(CameraURLLabeledEdit->Text) );
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormDestroy(TObject *Sender)
{
   FormLoader::store(this);
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::GoButtonClick(TObject *Sender)
{
   CppWebBrowser->Navigate( (BSTR)WideString(CameraURLLabeledEdit->Text) );
}
//---------------------------------------------------------------------------
