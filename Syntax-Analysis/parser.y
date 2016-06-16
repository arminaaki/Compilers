%{
    #include <cstdlib>
    #include <cstdio>
    #define YYDEBUG 1

    int yylex(void);
    void yyerror(const char *);
%}

%error-verbose

 /* WRITEME: List all your tokens here */
%token T_ARROW
%token T_PLUS   
%token T_MINUS
%token T_MULT
%token T_DIVIDE
%token T_AND 
%token T_OR      
%token T_NOT               
%token T_OPAREN  
%token T_CPAREN  
%token T_OBRACET 
%token T_CBRACET 
%token T_SEMICOLON
%token T_COMMA  
%token T_DOT     
%token T_ASSIGNMENT             
%token T_LESSEQ  
%token T_LESS   
%token T_EQUALS             
%token T_IFSTMNT 
%token T_ELSESTMNT        
%token T_WHILESTMNT    
%token T_REPEATSTMNT
%token T_UNTILSTMNT           
%token T_PRINTSTMNT          
%token T_LITERAL
%token T_TRUE
%token T_FALSE      
%token T_BOOLEAN 
%token T_INTEGER 
%token T_NONE
%token T_NEW
%token T_EXTENDS
%token T_EOF
%token T_COLON 
%token T_RETURN
%token T_CLASS
%token T_IDENTIFIER

%left T_OR 
%left T_AND
%left T_LESSEQ T_LESS T_EQUALS
%left T_PLUS T_MINUS
%left T_MULT T_DIVIDE
%right NOT UNARYMINUS
 /* WRITEME: Specify precedence here*/ 

%%

/* WRITEME: This rule is a placeholder, since Bison requires
            at least one rule to run successfully. Replace
            this with your appropriate start rules. */
START : START CLASS
      | CLASS
      ;            

CLASS : T_IDENTIFIER INHERITS T_OBRACET CLASS_BLOCK T_CBRACET
      ;

INHERITS : T_EXTENDS T_IDENTIFIER
         |
         ;
CLASS_BLOCK : MEMBERS METHODS
            | MEMBERS
            | METHODS
            |
            ;
MEMBERS : MEMBERS MEMBER T_SEMICOLON
        | MEMBER T_SEMICOLON
MEMBER :  MEMBER T_COMMA T_IDENTIFIER
       |  MEMBER_TYPES T_IDENTIFIER 
       ;
MEMBER_TYPES : T_BOOLEAN
             | T_INTEGER
             | T_IDENTIFIER
             ;
METHODS : METHODS METHOD
        | METHOD 
RETURN_MEMBER_TYPES : T_BOOLEAN
                    | T_INTEGER
                    | T_IDENTIFIER
                    | T_NONE
                    ;
METHOD : T_IDENTIFIER T_OPAREN PARAMETERS T_CPAREN T_ARROW RETURN_MEMBER_TYPES T_OBRACET BODY T_CBRACET
        | T_IDENTIFIER T_OPAREN T_CPAREN T_ARROW RETURN_MEMBER_TYPES T_OBRACET BODY T_CBRACET
        ;
PARAMETERS :  PARAMETERS T_COMMA PARM 
           |  PARM
           ;
PARM : T_IDENTIFIER T_COLON MEMBER_TYPES
     ;
BODY : RETURN_CONTROL
     | STATEMENTS
     | STATEMENTS RETURN_CONTROL
     |
     ;
RETURN_CONTROL : T_RETURN EXPRESSION T_SEMICOLON
               ;

STATEMENTS : STATEMENTS STATEMENT
           | STATEMENT
           ;
STATEMENT  : METHODCALL T_SEMICOLON
           | ASSIGNING T_SEMICOLON
           | WHILELOOP
           | REPEAT_UNTIL T_SEMICOLON
           | PRINT T_SEMICOLON
           | IF_ELSE
           | MEMBER T_SEMICOLON
           ;
ASSIGNING : T_IDENTIFIER T_ASSIGNMENT EXPRESSION
          | T_IDENTIFIER T_DOT T_IDENTIFIER T_ASSIGNMENT EXPRESSION
          ;
WHILELOOP : T_WHILESTMNT EXPRESSION T_OBRACET STATEMENTS T_CBRACET
          ;
EXPRESSION : T_OPAREN EXPRESSION T_CPAREN 
           | EXPRESSION T_AND    EXPRESSION
           | EXPRESSION T_OR     EXPRESSION
           | EXPRESSION T_LESSEQ EXPRESSION
           | EXPRESSION T_LESS   EXPRESSION
           | EXPRESSION T_EQUALS EXPRESSION
           | EXPRESSION T_PLUS   EXPRESSION        
           | EXPRESSION T_MINUS  EXPRESSION        
           | EXPRESSION T_MULT   EXPRESSION        
           | EXPRESSION T_DIVIDE EXPRESSION        
           | T_MINUS EXPRESSION  %prec UNARYMINUS
           | T_NOT   EXPRESSION  %prec NOT          
           | T_LITERAL
           | T_IDENTIFIER  
           | T_IDENTIFIER T_DOT T_IDENTIFIER
           | METHODCALL
           | T_TRUE
           | T_FALSE
           | T_NEW CLASS_NAME   
           ;
ARGUMENTS :  ARGUMENTS T_COMMA EXPRESSION 
          |  EXPRESSION
          ; 
METHODCALL : T_IDENTIFIER T_OPAREN ARGUMENTS T_CPAREN
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_OPAREN ARGUMENTS T_CPAREN
           | T_IDENTIFIER T_OPAREN  T_CPAREN
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_OPAREN T_CPAREN
           ;
CLASS_NAME :  T_IDENTIFIER
           |  T_IDENTIFIER T_OPAREN ARGUMENTS T_CPAREN
           ;
WHILELOOP  :  T_WHILESTMNT EXPRESSION T_OBRACET STATEMENTS T_OBRACET
           ;   
REPEAT_UNTIL : T_REPEATSTMNT T_OBRACET STATEMENTS T_CBRACET T_UNTILSTMNT T_OPAREN  EXPRESSION T_CPAREN
             ;
IF_ELSE : T_IFSTMNT EXPRESSION T_OBRACET STATEMENTS T_CBRACET ELSE_BLOCK
        ;
ELSE_BLOCK : T_ELSESTMNT T_OBRACET STATEMENTS T_CBRACET
           |
           ; 
PRINT : T_PRINTSTMNT EXPRESSION
      ;

 /* WRITME: Write your Bison grammar specification here */

%%

extern int yylineno;

void yyerror(const char *s) {
  fprintf(stderr, "%s at line %d\n", s, yylineno);
  exit(1);
}
