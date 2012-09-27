#ifndef _TS_PARSER
#define _TS_PARSER

#include <string>
#include <vector>
#include <iostream>
#include <exception>

#include "tokenizer.h"

namespace grammar {

typedef std::vector<std::string> Program;

class Parser {
 public:
  Parser(std::istream& is, Tokenizer& tokenizer) : tokenizer_(tokenizer) {}
  void parse(Program* program);

  class ParsingInterrupted: public std::exception {
   public:
    ParsingInterrupted() {}
  };
  
  class SyntaxError: public std::exception {
   public:
    SyntaxError(const std::string& msg) : std::exception(msg.c_str()) {}
  };

 private:
  Tokenizer& tokenizer_;
};

} // grammar

#endif
