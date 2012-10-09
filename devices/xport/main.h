//---------------------------------------------------------------------------

#ifndef mainH
#define mainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <IdBaseComponent.hpp>
#include <IdComponent.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>

#include "xportgpio.h"
#include "CPortCtl.hpp"
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include "SHDocVw_OCX.h"
#include <OleCtrls.hpp>

//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TComLed *ComLed1;
   TComLed *ComLed2;
   TComLed *ComLed3;
   TSpeedButton *Led1OutSpeedButton;
   TSpeedButton *Led1InSpeedButton;
   TSpeedButton *Led2OutSpeedButton;
   TSpeedButton *Led2InSpeedButton;
   TSpeedButton *Led3OutSpeedButton;
   TSpeedButton *Led3InSpeedButton;
   TButton *Led1GetButton;
   TButton *Led2GetButton;
   TButton *Led3GetButton;
   TButton *Led11Button;
   TButton *Led10Button;
   TButton *Led21Button;
   TButton *Led20Button;
   TButton *Led31Button;
   TButton *Led30Button;
   TLabeledEdit *XPortIPLabeledEdit;
   TCppWebBrowser *CppWebBrowser;
   TLabeledEdit *CameraURLLabeledEdit;
   TButton *GoButton;
   void __fastcall Led1GetButtonClick(TObject *Sender);
   void __fastcall Led2GetButtonClick(TObject *Sender);
   void __fastcall Led3GetButtonClick(TObject *Sender);
   void __fastcall Led1OutSpeedButtonClick(TObject *Sender);
   void __fastcall Led1InSpeedButtonClick(TObject *Sender);
   void __fastcall Led2OutSpeedButtonClick(TObject *Sender);
   void __fastcall Led2InSpeedButtonClick(TObject *Sender);
   void __fastcall Led3OutSpeedButtonClick(TObject *Sender);
   void __fastcall Led3InSpeedButtonClick(TObject *Sender);
   void __fastcall Led11ButtonClick(TObject *Sender);
   void __fastcall Led10ButtonClick(TObject *Sender);
   void __fastcall Led21ButtonClick(TObject *Sender);
   void __fastcall Led20ButtonClick(TObject *Sender);
   void __fastcall Led31ButtonClick(TObject *Sender);
   void __fastcall Led30ButtonClick(TObject *Sender);
   void __fastcall XPortIPLabeledEditChange(TObject *Sender);
   void __fastcall FormCreate(TObject *Sender);
   void __fastcall FormDestroy(TObject *Sender);
   void __fastcall GoButtonClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
