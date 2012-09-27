#ifndef _TS_TOKEN
#define _TS_TOKEN

#include "parser_grammar.h"

namespace grammar {

static const int kMaxTokenLength = 64;

struct Token {
  int token;
  char value[kMaxTokenLength + 1];
};
} // grammar

#endif
