#ifndef _TS_TOKENIZER_H
#define _TS_TOKENIZER_H

#include <iostream>
#include <iomanip>
#include <sstream>
#include <stdexcept>

#include "token.h"
#include "FlexLexer.h"

namespace {
template <typename T> T min(T a, T b) { return a < b ? a : b; }
}

namespace grammar {

class Tokenizer: public yyFlexLexer {
 public:
  Tokenizer(std::istream& is) : 
    yyFlexLexer(&is, &err_), col_(1), offset_(1) {}

  Token next() {
    if (yylex() != 0) {
      return token_;
    }
    err_ << std::flush;
    if (!err_.str().empty())
      throw unknown(err_.str(), col_, lineno(), offset_);
    throw eof();
  }

  size_t col() const { return col_; }
  size_t offset() const { return offset_; }
  
  class eof: public std::exception {
   public:
    explicit eof() {}
  };

  class unknown: public std::exception {
   public:
    explicit unknown(const std::string& token, size_t col, size_t line, 
                     size_t offset) : msg_(""), token_(token), col_(col), 
                                      line_(line), offset_(offset) {
      std::stringstream fmt;
      fmt << "unknown token '" << token_ << "' at the position " << col_
          << ", line " << line_ << ", offset " << offset_;
      msg_ = fmt.str();
    }

    virtual const char* what() const throw() { return msg_.c_str(); }

   private:
    std::string msg_;
    std::string token_;
    size_t col_;
    size_t line_;
    size_t offset_;
  };

 private:
  virtual int create_token(int token) {
    token_.token = token;
    size_t sz = min(static_cast<int>(std::strlen(yytext)), kMaxTokenLength);
    std::memcpy(token_.value, yytext, sz);
    token_.value[sz] = 0;
    skip_token();
    return token_.token;
  }

  virtual void skip_token() {
    col_ += yyleng;
    offset_ += yyleng;
  }

  virtual void next_line() {
    col_ = 0;
  }

  size_t col_;
  size_t offset_;
  Token token_;
  std::stringstream err_;

  Tokenizer(const Tokenizer&);
  void operator=(const Tokenizer&);
};

} // namespace

#endif

