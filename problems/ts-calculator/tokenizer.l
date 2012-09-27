%{
#include "token.h"
%}

%option yylineno
%option nounistd
%option default warn
%option noyywrap
%option c++

letter     [a-zA-Z]
digit      [0-9]

%%

{digit}+("."{digit}+)? {
  return create_token(CONSTANT);
}

"_"*{letter}({letter}|{digit}|"_")* { 
  return create_token(SYMBOL); 
}

"+" { 
  return create_token(PLUS);
}

"-" { 
  return create_token(MINUS);
}

"*" { 
  return create_token(TIMES);
}

"/" { 
  return create_token(DIVIDE);
}

"," { 
  return create_token(COMMA);
}

"(" {
  return create_token(LPARENT);
}

")" {
  return create_token(RPARENT);
}
  
[ \t]  { 
  skip_token();
}

\n { 
  next_line();
  skip_token();
}

%%

// Additional C/C++ code could added here.

