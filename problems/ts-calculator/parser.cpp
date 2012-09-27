#include "parser.h"

#include <sstream>
#include <vector>
#include <string>

void *ParseAlloc(void *(*mallocProc)(size_t));
void Parse(void *yyp, int yymajor, grammar::Token yyminor, 
           grammar::Program* program);
void ParseFree(void *p, void (*freeProc)(void*));

namespace grammar {

void Parser::parse(Program* program) {
  program->clear();
  void *pParser = ParseAlloc(malloc);
  Token token;
  for (bool eof = false; !eof;) {
    try {
      token = tokenizer_.next();
      Parse(pParser, token.token, token, program);
    } catch (const Tokenizer::eof&) {
      eof = true;
    } catch (const Tokenizer::unknown& e) {
      ParseFree(pParser, free);
      throw SyntaxError(e.what());
    } catch (const Parser::ParsingInterrupted&) {
      std::stringstream fmt;
      fmt << "syntax error at the position " << tokenizer_.col() 
          << " line " << tokenizer_.lineno() 
          << " (offset " << tokenizer_.offset() << ")";
      ParseFree(pParser, free);
      throw SyntaxError(fmt.str());
    }
  }
  Parse(pParser, 0, token, program);
  ParseFree(pParser, free);
}

} // grammar
