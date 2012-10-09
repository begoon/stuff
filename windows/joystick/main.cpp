//---------------------------------------------------------------------------

#include <vcl.h>
#include <mmsystem.h>
#pragma hdrstop

#include "main.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
{
   int n = joyGetNumDevs();
   if( n == 0 ) {
      Application->MessageBox( "Джойстиков в системе не обнаружено", "Ошибка", MB_OK );
      Application->Terminate();
   }

   for( int i = 0; i < n; i++ ) {
      JOYINFO jiInfo;
      if( joyGetPos( i, &jiInfo) == JOYERR_NOERROR /* != JOYERR_UNPLUGGED */ ) {
         LogMemo->Lines->Add( AnsiString("Джойстик ") + i );
         JoyIndexEdit->Text = i;
      }
   }

   if( LogMemo->Lines->Count == 0 ) {
      Application->MessageBox( "Джойстиков в системе не обнаружено", "Ошибка", MB_OK );
      Application->Terminate();
   }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TimerTimer(TObject *Sender)
{
   JOYINFO jiInfo;
   int joy = JoyIndexEdit->Text.ToIntDef( 0 );
   if( joyGetPos( joy, &jiInfo ) == JOYERR_NOERROR ) {
      SpeedButton1->Down = jiInfo.wButtons & JOY_BUTTON1;
      SpeedButton2->Down = jiInfo.wButtons & JOY_BUTTON2;
      SpeedButton3->Down = jiInfo.wButtons & JOY_BUTTON3;
      SpeedButton4->Down = jiInfo.wButtons & JOY_BUTTON4;
      XLabeledEdit->Text = jiInfo.wXpos;
      YLabeledEdit->Text = jiInfo.wYpos;
      ZLabeledEdit->Text = jiInfo.wZpos;
   }
}
//---------------------------------------------------------------------------

