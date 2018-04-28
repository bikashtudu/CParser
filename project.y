%{
	#include<stdio.h>
	int yylex(void);
	int yyerror(const char *s);
	int success = 1;
%}
%token INCLUDE STDIO MAIN
%token IDENTIFIER CONSTANT STRING_LITERAL SIZEOF INTEGER DECIMAL LITERAL TYPE
%token PRINTF SCANF GETC GETS GETCHAR PUTS PUTCHAR CLEARERR
%token FOPEN FCLOSE GETW PUTW FGETC FGETCHAR
%token FPUTC FGETS FPUTS FEOF FPRINTF FSCANF FPUTCHAR 
%token FSEEK FTELL REWIND PUTC SPRINTF SSCANF REMOVE FFLUSH
%token OP_PTR OP_INC OP_DEC OP_LS OP_RS OP_LEQ OP_GEQ OP_EQU OP_NEQ
%token OP_AND OP_OR MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LS_ASSIGN RS_ASSIGN BWAND_ASSIGN
%token BWXOR_ASSIGN BWOR_ASSIGN
%token STDLIB MALLOC FREE

%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID STRING
%token STRUCT UNION 

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%left '+' '-'
%left '*' '/'
%nonassoc "then"
%nonassoc ELSE
%define parse.error verbose


%%

start : incio inclib code
|		incio code
;

incio:	INCLUDE '<' STDIO '>' 
;

inclib: INCLUDE '<' STDLIB '>'
; 

code : declarations program
|      program ;

declarations : 		declarations declr
| declr 
|stmt_list; 


declr : type_spec variable_list ';'
|		type_spec IDENTIFIER '(' para_list ')' compound_stmt
|		type_spec IDENTIFIER '(' ')' compound_stmt
;

para_list : type_spec variable
|			para_list ',' type_spec variable
;
type_spec :		VOID
|			SHORT INT
|			UNSIGNED SHORT INT
|			UNSIGNED INT
|			INT
|			LONG INT
|			UNSIGNED LONG INT
|			LONG LONG INT
|			UNSIGNED LONG LONG INT
|			CHAR
|			SIGNED CHAR
|			UNSIGNED CHAR
|			FLOAT
|			DOUBLE
|			LONG DOUBLE
|			struct_union_specifier
;		

variable_list :		variable_type
|			variable_list ',' variable_type
;

variable_type :		variable
|			variable '=' initialise
;


variable :		pointer var
|				var
;

pointer : 		'*' pointer
|			'*'
;


var :			IDENTIFIER
|			'&' IDENTIFIER
|			IDENTIFIER OP_PTR IDENTIFIER
|			var '[' INTEGER ']'
|			var '[' ']'	
;

initialise :		assignment_expr
|			'{' init_list '}'
|			'{' init_list ',' '}'
;

init_list :		initialise
|			init_list ',' initialise
;	

struct_union_specifier :	struct_union IDENTIFIER '{' struct_declr_list '}'
|				struct_union IDENTIFIER 
|				struct_union IDENTIFIER '*' 
;	

struct_union :		STRUCT
|			UNION
;

struct_declr_list :	struct_declr 
|			struct_declr_list struct_declr
;

struct_declr :	type_spec struct_variable_list ';';

struct_variable_list :	variable
|			struct_variable_list ',' variable
;


program : main_type MAIN '(' ')' compound_stmt			
;


stmt : func_call 
|	compound_stmt
|	label_stmt
|	expr_stmt
|	iteration_stmt
|	selection_stmt
|	jump_stmt
;

//func_call

func_call :	 pre_func ';'
;

pre_func  :   	  
|		  PRINTF '(' STRING argument_list ')' 						//STDIO
|		  SCANF	 '(' STRING argument_list ')' 
|		  GETS '(' argument ')' 
|		  GETC '(' argument ')' 
|		  GETCHAR '(' ')' 
|		  PUTS	'(' argument ')' 
|		  PUTCHAR  '(' argument ')' 
|		  CLEARERR  '(' argument ')'
|		  FOPEN	 '(' argument ',' argument ')' 
|		  FCLOSE  '(' argument ')' 
|		  GETW   '(' argument ')' 
|		  PUTW   '(' argument ',' argument ')' 
|		  FGETC  '(' argument ')' 
|		  FEOF   '(' argument ')' 
|		  FGETCHAR  '(' ')' 
|		  FPUTC	    '(' argument ',' argument ')' 
|		  FGETS	    '(' argument ')' 
|		  FPUTS     '(' argument ',' argument ')' 
|		  FPRINTF    '(' argument ',' STRING argument_list ')' 
|		  FSCANF     '(' argument ',' STRING argument_list ')' 
|		  FPUTCHAR   '(' argument ')' 
|		  FSEEK	     '(' argument ',' argument ',' argument')' 
|		  FTELL      '(' argument ')' 
|		  REWIND     '(' argument ')' 
|		  PUTC       '(' argument ')' 
|		  SPRINTF    '(' argument ',' STRING argument_list ')' 
|		  SSCANF     '(' argument ',' STRING argument_list ')' 
|		  REMOVE     '(' argument ')'  
|		  FFLUSH     '(' argument ')'
|		  '(' type_spec 	'*' ')' MALLOC 	 '(' argument ')' 
|		  '(' type_spec 	 ')' MALLOC 	 '(' argument ')' 
|		  MALLOC	 '(' argument ')' 
|		  FREE		 '(' argument ')'
|		  IDENTIFIER '(' ')' 
|	  	  IDENTIFIER '(' argument  ')'
|	  	  IDENTIFIER '(' argument argument_list ')' 
;


compound_stmt : '{' '}' 
|		'{' declarations '}'		
| 		'{' stmt_list '}'
|		'{' declarations stmt_list '}'	
|		'{' stmt_list declarations'}'	
;

stmt_list :		stmt
|			stmt_list stmt
|			declarations
;

label_stmt : 		IDENTIFIER ':' stmt
|			CASE constant_expr ':' stmt
|			DEFAULT ':' stmt
;

expr_stmt :		expr ';'
|			';'
;

selection_stmt :	IF '(' expr ')' stmt
|			IF '(' expr ')' stmt ELSE stmt		
|			SWITCH '(' expr ')' '{' stmt_list '}'
;

iteration_stmt :	WHILE '(' expr ')' stmt
|			DO stmt WHILE '(' expr ')' ';'
|			FOR '(' expr ';' expr ';' expr ')' stmt
|			FOR '(' expr ';' expr ';' ')' stmt
|			FOR '(' expr ';' ';' expr ')' stmt
|			FOR '(' ';' expr ';' expr ')' stmt
|			FOR '(' expr ';' ';' ')' stmt
|			FOR '(' ';'  ';' expr ')' stmt
|			FOR '(' ';' expr ';' ')' stmt
|			FOR '(' ';' ';' ')' stmt
;

jump_stmt : 		GOTO IDENTIFIER ';'
|			CONTINUE ';'
|			BREAK ';'
|			RETURN ';'
|			RETURN expr ';'
;

expr :			assignment_expr
|			expr ',' assignment_expr
|			'(' expr ')'
;


assignment_expr  : 	conditional_expr 
|			unary_expr assignment_operator assignment_expr
;

assignment_operator :	'='
|			LS_ASSIGN
|			RS_ASSIGN
|			ADD_ASSIGN
|			SUB_ASSIGN
|			MUL_ASSIGN 
|			DIV_ASSIGN 
|			MOD_ASSIGN 
|			BWAND_ASSIGN
|			BWOR_ASSIGN 
|			BWXOR_ASSIGN
;

constant_expr :		conditional_expr
;

conditional_expr  :	or_expr '?' expr ':' conditional_expr
|			or_expr
;

or_expr :		or_expr OP_OR and_expr 
|			and_expr
;

and_expr :		and_expr OP_AND bwor_expr 
|			bwor_expr
;

bwor_expr :		bwor_expr '|' bwxor_expr 
|			bwxor_expr
;

bwxor_expr :		bwxor_expr '^' bwand_expr 
|			bwand_expr
;

bwand_expr :		bwand_expr '&' equality_expr 
|			equality_expr
;


equality_expr :		equality_expr OP_EQU relational_expr 
|			equality_expr OP_NEQ relational_expr
|			relational_expr
;


relational_expr :	relational_expr '<' shift_expr 
|			relational_expr '>' shift_expr
|			relational_expr OP_LEQ shift_expr
|			relational_expr OP_GEQ shift_expr
|			shift_expr
;

shift_expr: 		shift_expr OP_LS add_expr
| 			shift_expr OP_RS add_expr
|			add_expr
;

add_expr : 		add_expr '+' multiply_expr
| 			add_expr '-' multiply_expr
|			multiply_expr
;

multiply_expr :  	multiply_expr '*' unary_expr
| 			multiply_expr '/' unary_expr
| 			multiply_expr '%' unary_expr
|			unary_expr
;

unary_expr : 		OP_INC unary_expr
|			unary_expr OP_INC
|			OP_DEC unary_expr
|			primary_expr
;

primary_expr : 		operands
;

operands :	variable
|			INTEGER
|			DECIMAL
| 			pre_func
;

argument :			func_call	
|	variable
|			INTEGER
|			DECIMAL
|			STRING
;			

argument_list : 	argument_list ',' argument
|					
;


main_type : 	VOID
|	    	INT
|		DOUBLE 
|		FLOAT
|		CHAR
;

%%

int main()
{
    yyparse();
    if(success)
    	printf("\nParsing Successful\n");
    return 0;
}
int yyerror(const char *msg)
{
	extern int yylineno;
	printf("Parsing Failed\nLine Number: %d %s\n",yylineno,msg);
	success = 0;
	return 0;
}
