// tokeniser.h : shared definition for tokeniser.l and compilateur.cpp
#ifndef TOKENISER_H
#define TOKENISER_H

enum TOKEN
{
  FEOF,
  UNKNOWN,
  NUMBER,
  ID,
  STRINGCONST,
  RBRACKET,
  LBRACKET,
  RPARENT,
  LPARENT,
  COMMA,
  SEMICOLON,
  DOT,
  ADDOP,
  MULOP,
  RELOP,
  NOT,
  ASSIGN,
  KEYWORD,
  TYPE,
  COLON,
  CHARCONST,
  BOOLCONST
};

#endif // TOKENISER_H