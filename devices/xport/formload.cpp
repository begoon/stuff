#include <typeinfo>

#include <vcl.h>
#include <Buttons.hpp>
#include <IniFiles.hpp>

#include <sstream>

#include "formload.h"

void FormLoader::store( TForm* form, AnsiString file )
{
   if( !file.Length() )
      file = ChangeFileExt( Application->ExeName, ".ini" );

   TIniFile* ini = new TIniFile( file );

   try {

      AnsiString section = form->Name;

      ini->WriteString( section, "Left", form->Left );
      ini->WriteString( section, "Top", form->Top );
      ini->WriteString( section, "Width", form->Width );
      ini->WriteString( section, "Height", form->Height );

      for( int i = 0; i < form->ComponentCount; i++ ) {
         TComponent* c = form->Components[i];

         if( c->Tag == -1 ) continue;

         AnsiString type = typeid( *c ).name();
         AnsiString name = c->Name;
         AnsiString value;

         if( type == "TLabeledEdit" ) value = dynamic_cast< TLabeledEdit * >(c)->Text;
         if( type == "TEdit" ) value = dynamic_cast< TEdit * >(c)->Text;
         if( type == "TOpenDialog" ) value = dynamic_cast< TOpenDialog * >(c)->FileName;
         if( type == "TCheckBox" ) value = (int)dynamic_cast< TCheckBox * >(c)->Checked;
         if( type == "TRadioButton" ) value = (int)dynamic_cast< TRadioButton * >(c)->Checked;
         if( type == "TSpeedButton" ) value = (int)dynamic_cast< TSpeedButton * >(c)->Down;
         if( type == "TMenuItem" ) value = (int)dynamic_cast< TMenuItem * >(c)->Checked;

         if( type == "TMemo" ) {
            TMemo* memo = dynamic_cast< TMemo * >(c);
            memo->Lines->QuoteChar = '|';
            value = memo->Lines->DelimitedText;
         }

         if( !value.Length() ) continue;

         name = name.Delete( name.Length() - type.Length() + 2, type.Length() - 1 );

         ini->WriteString( section, name, value );
      }
   } __finally {
      delete ini;
   }
}

void FormLoader::restore( TForm* form, AnsiString file )
{
   if( !file.Length() )
      file = ChangeFileExt( Application->ExeName, ".ini" );

   TIniFile* ini = new TIniFile( file );

   try {

      AnsiString section = form->Name;

      AnsiString window = ini->ReadString( section, "Window", "" );

      form->Left = ini->ReadString( section, "Left", form->Left ).ToInt();
      form->Top = ini->ReadString( section, "Top", form->Top ).ToInt();
      form->Width = ini->ReadString( section, "Width", form->Width ).ToInt();
      form->Height = ini->ReadString( section, "Height", form->Height ).ToInt();

      for( int i = 0; i < form->ComponentCount; i++ ) {
         TComponent* c = form->Components[i];

         if( c->Tag == -1 ) continue;

         AnsiString type = typeid( *c ).name();
         AnsiString name = c->Name;
         AnsiString default_value;

         bool use = false;

         if( type == "TLabeledEdit" ) {
            default_value = dynamic_cast< TLabeledEdit * >(c)->Text;
            use = true;
         }
         if( type == "TEdit" ) {
            default_value = dynamic_cast< TEdit * >(c)->Text;
            use = true;
         }
         if( type == "TOpenDialog" ) {
            default_value = dynamic_cast< TOpenDialog * >(c)->FileName;
            use = true;
         }
         if( type == "TCheckBox" ) {
            default_value = (int)dynamic_cast< TCheckBox * >(c)->Checked;
            use = true;
         }
         if( type == "TRadioButton" ) {
            default_value = (int)dynamic_cast< TRadioButton * >(c)->Checked;
            use = true;
         }
         if( type == "TSpeedButton" ) {
            default_value = (int)dynamic_cast< TSpeedButton * >(c)->Down;
            use = true;
         }
         if( type == "TMenuItem" ) {
            default_value = (int)dynamic_cast< TMenuItem * >(c)->Checked;
            use = true;
         }
         if( type == "TMemo" ) {
            TMemo* memo = dynamic_cast< TMemo * >(c);
            memo->Lines->QuoteChar = '|';
            default_value = memo->Lines->DelimitedText;
            use = true;
         }

         if( !use ) continue;

         name = name.Delete( name.Length() - type.Length() + 2, type.Length() - 1 );

         AnsiString value = ini->ReadString( section, name, default_value );

         if( type == "TLabeledEdit" ) dynamic_cast< TLabeledEdit * >(c)->Text = value;
         if( type == "TEdit" ) dynamic_cast< TEdit * >(c)->Text = value;
         if( type == "TOpenDialog" ) dynamic_cast< TOpenDialog * >(c)->FileName = value;
         if( type == "TCheckBox" ) dynamic_cast< TCheckBox * >(c)->Checked = (bool)value.ToInt();
         if( type == "TRadioButton" ) (bool)dynamic_cast< TRadioButton * >(c)->Checked = (bool)value.ToInt();
         if( type == "TSpeedButton" ) (bool)dynamic_cast< TSpeedButton * >(c)->Down = (bool)value.ToInt();
         if( type == "TMenuItem" ) (bool)dynamic_cast< TMenuItem * >(c)->Checked = (bool)value.ToInt();

         if( type == "TMemo" ) {
            TMemo* memo = dynamic_cast< TMemo * >(c);
            memo->Lines->QuoteChar = '|';
            memo->Lines->DelimitedText = value;
         }
      }
   } __finally {
      delete ini;
   }
}
