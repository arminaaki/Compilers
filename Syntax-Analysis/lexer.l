%option yylineno
%pointer

%{
    #include <cstdlib>
    #include <cerrno>
    #include <climits>
    #include <limits>
    #include "parser.hpp"
    
	void yyerror(const char *);
%}
%x       CSYLE_COMMENT
ID       [A-Za-z][A-Za-z0-9]*
num      0|[1-9][0-9]*


/* WRITEME: Write any definitions here. You can find information on
            definitions here:
            http://flex.sourceforge.net/manual/Definitions-Section.html */

/* WRITEME: If you are going to use start conditions, write them here.
            You can find more information on start conditions here:
            http://flex.sourceforge.net/manual/Start-Conditions.html
            You will probably want to use exclusive conditions rather
            than inclusive conditions (use the %x instead of the %s). */

%%

<INITIAL>"/*"                      BEGIN(CSYLE_COMMENT);

<CSYLE_COMMENT>[\n\t\r ]				{}
<CSYLE_COMMENT><<EOF>>             { yyerror("dangling comment"); }  
<CSYLE_COMMENT>"*/"                BEGIN(INITIAL);  
<CSYLE_COMMENT>.				{}


[\n\t\r ]+          ;

"->"                 { return  T_ARROW;   }
 /* evaluation operators */
"+"                  { return  T_PLUS;       }
"-"                  { return  T_MINUS;      }
"*"                  { return  T_MULT;      }
"/"                  { return  T_DIVIDE;      }
"and"                { return  T_AND;      }
"or"                 { return  T_OR;      }
"not"                { return  T_NOT;     }

 /*  blocks and groups */
"("                  { return  T_OPAREN;     }
")"                  { return  T_CPAREN;     }
"{"                  { return  T_OBRACET;     }
"}"                  { return  T_CBRACET;     }
";"                  { return  T_SEMICOLON;  }
","                  { return  T_COMMA;      }
"."                  { return  T_DOT;        }
"="                  { return  T_ASSIGNMENT ;        }

 /* comparison */
"<="                 { return  T_LESSEQ;   }
"<"                  { return  T_LESS;        }
"equals"            { return  T_EQUALS;   }

 /*  IF statement */
"if"                 { return  T_IFSTMNT;      }
"else"               { return  T_ELSESTMNT;     }

 /* WHILE statement */
"while"              { return  T_WHILESTMNT;      }

 /* repeat-until statement */
"repeat"             { return  T_REPEATSTMNT;      }
"until"              { return  T_UNTILSTMNT;      }

 /* print statement */
 "print"              { return  T_PRINTSTMNT;      }


 /* DATA TYPES */
"boolean"            { return  T_BOOLEAN;    }
"integer"            { return  T_INTEGER;    }

 /* Return types */
"none"               { return  T_NONE;}

 /* Class OPS */
"new"                { return  T_NEW; }
"class"              { return  T_CLASS;}
"extends"            { return  T_EXTENDS;}

 /*misc */
<<EOF>>              { yyterminate();}


 /* function Tokens */
":"                  { return  T_COLON;  }  
"return"             { return  T_RETURN; }
"true"               { return  T_TRUE;}
"false"              { return  T_FALSE;}

{num}                  { return  T_LITERAL;}
{ID}                 { return T_IDENTIFIER;}
 /* value detector */





.                 { yyerror("invalid character"); }

%%

int yywrap(void) {
  return 1;
}
