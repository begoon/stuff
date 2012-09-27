%include {
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <iomanip>
#include <algorithm>
#include <cassert>

#include "token.h"
#include "parser.h"

} /* %include */

%left PLUS MINUS.
%left TIMES DIVIDE.

%token_type {grammar::Token}

%default_type {grammar::Token}

%extra_argument {grammar::Program* program}

%parse_accept {
}

%syntax_error {
  throw grammar::Parser::ParsingInterrupted();
}

main ::= program.
program ::= expression.

expression(A) ::= function(B). { 
  A = B; 
}

function ::= function_name(A) LPARENT arg_list RPARENT. {
  program->push_back(std::string("@") + A.value);
}

function_name(A) ::= SYMBOL(B). {
  A = B;
}

arg_list ::= expression. 

arg_list ::= arg_list COMMA expression.

expression ::= LPARENT expression RPARENT.

expression ::= CONSTANT(A). {
  program->push_back(A.value);
}

expression ::= SYMBOL(A). {
  program->push_back(std::string("#") + A.value);
}

expression ::= expression PLUS expression. {
  program->push_back("@+");
}

expression ::= expression MINUS expression. {
  program->push_back("@-");
}

expression ::= expression TIMES expression. {
  program->push_back("@*");
}

expression ::= expression DIVIDE expression. {
  program->push_back("@/");
}
