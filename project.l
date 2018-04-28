%option yylineno
%{
#include <stdio.h>
#include "y.tab.h"
%}


digit [0-9]
letter [a-zA-Z]
space [ ]

%%
"#include"		{  return INCLUDE;}
"stdio.h"		{  return STDIO;}
"stdlib.h"		{  return STDLIB;}

"break"			{ return  BREAK ; }
"case"			{ return  CASE ; }
"char"			{ return  CHAR ; }
"continue"		{ return  CONTINUE ; }
"default"		{ return  DEFAULT ; }
"do"			{ return  DO ; }
"double"		{ return  DOUBLE ; }
"else"			{ return  ELSE ; }
"float"			{ return  FLOAT ; }
"for"			{ return  FOR ; }
"goto"			{ return  GOTO ; }
"if"			{ return  IF ; }
"int"			{ return  INT ; }
"long"			{ return  LONG ; }
"return"		{ return  RETURN ; }
"short"			{ return  SHORT ; }
"signed"		{ return  SIGNED ; }
"struct"		{ return  STRUCT ; }
"switch"		{ return  SWITCH ; }
"union"			{ return  UNION ; }
"unsigned"		{ return  UNSIGNED ; }
"void"			{ return  VOID ; }
"while"			{ return  WHILE ; }
"main"			{ return  MAIN ;  }
"printf"																									{return PRINTF;}
"scanf"																										{return SCANF;}
"getc"																										{return GETC;}
"gets"																										{return GETS;}
"getchar"																									{return GETCHAR;}
"puts"																										{return PUTS ;}
"putchar"																									{return PUTCHAR;}
"clearerr"																									{return CLEARERR;}
"fopen"																										{return FOPEN ;}
"fclose"																									{return FCLOSE ;}
"getw"																										{return GETW ;}
"putw"																										{return PUTW ;}
"gfetc"																										{return FGETC ;}
"fgetchar"																									{return FGETCHAR;}
"fputc"																										{return FPUTC ;}
"fgets"																										{return FGETS ;}
"fputs"																										{return FPUTS ;}
"feof"																										{return FEOF ;}
"fprintf"																									{return FPRINTF ;}
"fscanf"																									{return FSCANF ;}
"fputchar"																									{return FPUTCHAR ;}
"fseek"																										{return FSEEK ;}
"ftell"																										{return FTELL ;}
"rewind"																									{return REWIND ;}
"putc"																										{return PUTC ;}
"sprintf"																									{return SPRINTF ;}
"sscanf"																									{return SSCANF ;}
"remove"																									{return REMOVE ;}
"fflush"																									{return FFLUSH;}
"malloc"																									{return MALLOC ;}
"free"																										{return FREE;}


{letter}({letter}|{digit}|_)*			{ return IDENTIFIER ;}
{digit}+					{ return INTEGER ;  }
{digit}+"."{digit}+ 				{ return DECIMAL ;   }
\"(\\.|[^\"])*\"				{ return STRING;     }
"%d"|"%c"|"%s"|"%l"|"%ld"|"%f"|"%lf"		{ return TYPE;}
({letter}|{digit})*				{ return LITERAL;}

"<<="			{ return LS_ASSIGN;  }
">>="			{ return RS_ASSIGN;  }
"+="			{ return ADD_ASSIGN; }		
"-="			{ return SUB_ASSIGN; }
"*="			{ return MUL_ASSIGN; }
"/="			{ return DIV_ASSIGN; }
"%="			{ return MOD_ASSIGN; }
"&="			{ return BWAND_ASSIGN; }	
"|="			{ return BWOR_ASSIGN;  }
"^="			{ return BWXOR_ASSIGN; }
"<<"			{ return OP_LS;  }
">>"			{ return OP_RS;  }
"++"			{ return OP_INC; }
"--"			{ return OP_DEC; }
"->"			{ return OP_PTR; }
"&&"			{ return OP_AND; }		//Logical
"||"			{ return OP_OR;  }
">="			{ return OP_GEQ; }
"<="			{ return OP_LEQ; }
"=="			{ return OP_EQU; }		//Relational
"!="			{ return OP_NEQ; }


"+"			|
"-"			|		
"*"			|		
"/"			|		
"%"			|		
"="			|		
">"			|		
"<"			|
"?"			|
"&"			|					// Bitwise
"|"			|
"^"			|		
"{"			|
"}"			|
"("			|		
")"			|		
"["			|			
"]"			|		
"!"			|
","			|
":"			|
";"			|
"."			{ return yytext[0];}


"//"([^\n])*[\n]			;
[/][*]([^*]|[*]*[^*/])*[*]+[/]		;
[ \t\n]			;

%%


int yywrap(void)
{
    return 1;
}

