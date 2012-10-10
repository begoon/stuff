//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TLabeledEdit *XLabeledEdit;
   TLabeledEdit *YLabeledEdit;
   TLabeledEdit *ZLabeledEdit;
   TSpeedButton *SpeedButton1;
   TSpeedButton *SpeedButton2;
   TSpeedButton *SpeedButton3;
   TSpeedButton *SpeedButton4;
   TTimer *Timer;
   TMemo *LogMemo;
   TEdit *JoyIndexEdit;
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall TimerTimer(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
