#ifndef _CSE_H_
#define _CSE_H_


#include <eh.h>              // For _set_se_translator

#define __except_cse __except(1)

class CSE {
public:
   // Call this function for each thread.
   static void MapSEtoCE() { _set_se_translator(TranslateSEtoCE); }
   operator DWORD() { return(m_er.ExceptionCode); }

   //CSE();
private:
   CSE(PEXCEPTION_POINTERS pep) {
      m_er      = *pep->ExceptionRecord;
      m_context = *pep->ContextRecord;
   }
   static void _cdecl TranslateSEtoCE(UINT dwEC, 
      PEXCEPTION_POINTERS pep) {
      throw CSE(pep);
   }
private:
   EXCEPTION_RECORD m_er;      // CPU independent exception information
   CONTEXT          m_context; // CPU dependent exception information
};

#endif