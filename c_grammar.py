# This file defines pylangparser's style C grammar. The grammar
# is based on the simplified C grammar from the SableCC project.
# See c_parser_grammar.txt for more details.
#
# Copyright, 2015, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("../..")

from pylangparser import *

# define all tokens in the language
AUTO = Keyword(r'auto')
BREAK = Keyword(r'break')
CASE = Keyword(r'case')
CONTINUE = Keyword(r'continue')
DEFAULT = Keyword(r'default')
DO = Keyword(r'do')
ELSE = Keyword(r'else')
EXTERN = Keyword(r'extern')
FOR = Keyword(r'for')
GOTO = Keyword(r'goto')
IF = Keyword(r'if')
REGISTER = Keyword(r'register')
RETURN = Keyword(r'return')
SIZEOF = Keyword(r'sizeof')
STATIC = Keyword(r'static')
STRUCT = Keyword(r'struct')
SWITCH = Keyword(r'switch')
UNION = Keyword(r'union')
VOLATILE = Keyword(r'volatile')
WHILE = Keyword(r'while')
ENUM = Keyword(r'enum')
TYPEDEF = Keyword(r'typedef')
VOID = Keyword(r'void')
CHAR = Keyword(r'char')
SHORT = Keyword(r'short')
INT = Keyword(r'int')
LONG = Keyword(r'long')
FLOAT = Keyword(r'float')
DOUBLE = Keyword(r'double')
SIGNED = Keyword(r'signed')
UNSIGNED = Keyword(r'unsigned')

DOT = Operator(r'.')
COMMA = Operator(r',')
COLON = Operator(r':')
SEMICOLON = Operator(r';')
L_PAR = Operator(r'(')
R_PAR = Operator(r')')
L_BRACKET = Operator(r'[')
R_BRACKET = Operator(r']')
L_BRACE = Operator(r'{')
R_BRACE = Operator(r'}')
STAR = Operator(r'*')
DIV = Operator(r'/')
MOD = Operator(r'%')
AMPERSAND = Operator(r'&')
PLUS = Operator(r'+')
MINUS = Operator(r'-')
CARET = Operator(r'^')
TILDE = Operator(r'~')
EXCL_MARK = Operator(r'!')
QUEST_MARK = Operator(r'?')
BAR = Operator(r'|')
ELLIPSIS = Operator(r'...')
EQUAL = Operator(r'=')
EQ = Operator(r'==')
NEQ = Operator(r'!=')
LT = Operator(r'<')
LTEQ = Operator(r'<=')
GT = Operator(r'>')
GTEQ = Operator(r'>=')
ARROW = Operator(r'->')
PLUS_PLUS = Operator(r'++')
MINUS_MINUS = Operator(r'--')
SHL = Operator(r'<<')
SHR = Operator(r'>>')
AMPERSAND_AMPERSAND = Operator(r'&&')
BAR_BAR = Operator(r'||')
STAR_EQUAL = Operator(r'*=')
DIV_EQUAL = Operator(r'/=')
MOD_EQUAL = Operator(r'%=')
PLUS_EQUAL = Operator(r'+=')
MINUS_EQUAL = Operator(r'-=')
SHL_EQUAL = Operator(r'<<=')
SHR_EQUAL = Operator(r'>>=')
AMPERSAND_EQUAL = Operator(r'&=')
CARET_EQUAL = Operator(r'^=')
BAR_EQUAL = Operator(r'|=')

IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
STRING_IDENTIFIER = Symbols(r'\".*\"')
INT_CONSTANT = Symbols(r'(0x[0-9A-Fa-f]*|\d+)')
FLOAT_CONSTANT = Symbols(r'(\d+\.(\d*)?|\.\d+)([eE][+-]?\d+)?')
CHAR_CONSTANT = Symbols(r'\'.\'')

CPP_STYLE_COMMENT = Ignore(r'\/\/[^\n]*')
MACROS = Ignore(r'\#.*\n')
IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')
CONST = Ignore(r'const')

# group tokens into sub-groups

# Note: order is important as the first entry in the list that matches a given
# token will be considered. Example:
#
# CONST = Ignore(r'const')
# IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
#
# if IDENTIFIER appears before CONST, the latter will never appear in the
# result list as 'const' matches both tokens

IGNORES = CONST & CPP_STYLE_COMMENT & MACROS & IGNORE_CHARS

KEYWORDS = AUTO & BREAK & CASE & ENUM & CONTINUE & DEFAULT & DO & ELSE & \
    EXTERN & FOR & GOTO & IF & REGISTER & RETURN & SIZEOF & STATIC & STRUCT & \
    SWITCH & UNION & VOLATILE & WHILE & ENUM & TYPEDEF & VOID & CHAR & SHORT & \
    INT & LONG & FLOAT & DOUBLE & SIGNED & UNSIGNED

# order is important as first operator that matches will be considered
# so it is important that '<=' is taken before '<'
OPERATORS = SHL & SHR & AMPERSAND_AMPERSAND & BAR_BAR & STAR_EQUAL & \
    DIV_EQUAL & MOD_EQUAL & PLUS_EQUAL & MINUS_EQUAL & SHL_EQUAL & SHR_EQUAL & \
    AMPERSAND_EQUAL & CARET_EQUAL & BAR_EQUAL & ARROW & DOT & COMMA & COLON & \
    SEMICOLON & L_PAR & R_PAR & L_BRACKET & R_BRACKET & L_BRACE & R_BRACE & \
    STAR & DIV & MOD & AMPERSAND & PLUS_PLUS & MINUS_MINUS & CARET & TILDE & \
    NEQ & EXCL_MARK & QUEST_MARK & BAR & ELLIPSIS & EQ & EQUAL & LTEQ & LT & \
    GTEQ & GT & PLUS & MINUS

IDENTIFIERS = IDENTIFIER & STRING_IDENTIFIER & FLOAT_CONSTANT & INT_CONSTANT & \
    CHAR_CONSTANT

# join all token sub-groups
TOKENS = IGNORES & KEYWORDS & OPERATORS & IDENTIFIERS

#
# list tokens to be IGNORED in the final AST
#
IgnoreTokensInAST(SEMICOLON & L_BRACE & R_BRACE & R_PAR & L_PAR & L_BRACKET & \
    R_BRACKET & COMMA & COLON)


#
# define C grammar
#
unop = \
    OperatorParser(PLUS) | \
    OperatorParser(MINUS) | \
    OperatorParser(TILDE) | \
    OperatorParser(EXCL_MARK)

memberop = \
    OperatorParser(DOT) | \
    OperatorParser(ARROW)

relop = \
    OperatorParser(EQ) | \
    OperatorParser(NEQ) | \
    OperatorParser(LT) | \
    OperatorParser(LTEQ) | \
    OperatorParser(GT) | \
    OperatorParser(GTEQ)

binop = \
    relop | \
    OperatorParser(STAR) | \
    OperatorParser(DIV) | \
    OperatorParser(MOD) | \
    OperatorParser(AMPERSAND) | \
    OperatorParser(PLUS) | \
    OperatorParser(MINUS) | \
    OperatorParser(CARET) | \
    OperatorParser(EXCL_MARK) | \
    OperatorParser(BAR) | \
    OperatorParser(SHL) | \
    OperatorParser(SHR) | \
    OperatorParser(AMPERSAND_AMPERSAND) | \
    OperatorParser(BAR_BAR)

compassignop = \
    OperatorParser(PLUS_EQUAL) | \
    OperatorParser(MINUS_EQUAL) | \
    OperatorParser(STAR_EQUAL) | \
    OperatorParser(DIV_EQUAL) | \
    OperatorParser(MOD_EQUAL) | \
    OperatorParser(AMPERSAND_EQUAL) | \
    OperatorParser(BAR_EQUAL) | \
    OperatorParser(CARET_EQUAL) | \
    OperatorParser(SHL_EQUAL) | \
    OperatorParser(SHR_EQUAL)

compop = \
    OperatorParser(AMPERSAND_AMPERSAND) | \
    OperatorParser(BAR_BAR)

prefop = \
    OperatorParser(PLUS_PLUS) | \
    OperatorParser(MINUS_MINUS)

postfop = \
    OperatorParser(PLUS_PLUS) | \
    OperatorParser(MINUS_MINUS)

void_specifier = KeywordParser(VOID)

char_specifier = KeywordParser(CHAR)

signed_char_specifier = KeywordParser(SIGNED) & KeywordParser(CHAR)

# Note: order is important as only the first sub-group of parsers
# (parser1 & parser2 &...& parsern) which applies will be considered
# That's why it is important to take "short int" before "short"
signed_short_specifier = \
    (KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(SHORT)) | \
    KeywordParser(SHORT)

signed_int_specifier = \
    (KeywordParser(SIGNED) & KeywordParser(INT)) | \
    KeywordParser(INT) | \
    KeywordParser(SIGNED)

signed_long_specifier = \
    (KeywordParser(SIGNED) & KeywordParser(LONG) & KeywordParser(INT)) | \
    (KeywordParser(SIGNED) & KeywordParser(LONG)) | \
    (KeywordParser(LONG) & KeywordParser(INT)) | \
    KeywordParser(LONG)

unsigned_char_specifier = KeywordParser(UNSIGNED) & KeywordParser(CHAR)

unsigned_short_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(SHORT) & KeywordParser(INT)) | \
    (KeywordParser(UNSIGNED) & KeywordParser(SHORT))

unsigned_int_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(INT)) | \
    KeywordParser(UNSIGNED)

unsigned_long_specifier = \
    (KeywordParser(UNSIGNED) & KeywordParser(LONG) & KeywordParser(INT)) | \
    (KeywordParser(UNSIGNED) & KeywordParser(LONG))

float_specifier = KeywordParser(FLOAT)

double_specifier = KeywordParser(DOUBLE)

long_double_specifier = KeywordParser(LONG) & KeywordParser(DOUBLE)

struct_specifier = KeywordParser(STRUCT) & SymbolsParser(IDENTIFIER)

union_specifier = KeywordParser(UNION) & SymbolsParser(IDENTIFIER)

enum_specifier = KeywordParser(ENUM) & SymbolsParser(IDENTIFIER)

type_specifier = \
    void_specifier | \
    char_specifier | \
    signed_char_specifier | \
    signed_short_specifier | \
    signed_int_specifier | \
    signed_long_specifier | \
    unsigned_char_specifier | \
    unsigned_short_specifier | \
    unsigned_int_specifier | \
    unsigned_long_specifier | \
    float_specifier | \
    double_specifier | \
    long_double_specifier | \
    struct_specifier | \
    union_specifier | \
    enum_specifier | \
    SymbolsParser(IDENTIFIER)

abstract_pointer = RecursiveParser()

abstract_array_declarator_tail = \
    OperatorParser(L_BRACKET) & \
    Optional(SymbolsParser(INT_CONSTANT)) & \
    OperatorParser(R_BRACKET)

abstract_array_declarator = \
    (OperatorParser(L_BRACKET) & \
        Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) & \
        Optional(abstract_array_declarator_tail)) | \
    (OperatorParser(L_PAR) & abstract_pointer & OperatorParser(R_PAR) & \
        OperatorParser(L_BRACKET) & Optional(SymbolsParser(INT_CONSTANT)) & \
        OperatorParser(R_BRACKET) & Optional(abstract_array_declarator_tail))

parameter_list = RecursiveParser()

abstract_direct_declarator = abstract_array_declarator | \
    (OperatorParser(L_PAR) & abstract_pointer & \
    OperatorParser(R_PAR) & OperatorParser(L_PAR) & \
    parameter_list & OperatorParser(R_PAR))

# notice the usage of the '+=' operator below
abstract_pointer += \
    (OperatorParser(STAR) & Optional(abstract_direct_declarator)) | \
    (OperatorParser(STAR) & abstract_pointer)

abstract_declarator = abstract_pointer | abstract_direct_declarator

declarator = RecursiveParser()

parameter_declaration = (type_specifier & declarator) | \
    (type_specifier & abstract_declarator) | (SymbolsParser(IDENTIFIER) & \
    declarator) | (SymbolsParser(IDENTIFIER) & abstract_declarator)

# notice the usage of the '+=' operator below
parameter_list += parameter_declaration & \
    Optional(Repeat(OperatorParser(COMMA) & parameter_declaration))

pointer = RecursiveParser()

array_declarator_tail = \
    OperatorParser(L_BRACKET) & Optional(SymbolsParser(INT_CONSTANT)) & \
    OperatorParser(R_BRACKET)

array_declarator = (SymbolsParser(IDENTIFIER) & OperatorParser(L_BRACKET) & \
    Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) << \
        ZeroOrMore(array_declarator_tail)) | \
    (OperatorParser(L_PAR) & pointer & \
        OperatorParser(R_PAR) & OperatorParser(L_BRACKET) & \
        Optional(SymbolsParser(INT_CONSTANT)) & OperatorParser(R_BRACKET) << \
        ZeroOrMore(array_declarator_tail))

function_pointer_declarator = RecursiveParser()

direct_declarator = array_declarator | SymbolsParser(IDENTIFIER) | \
    function_pointer_declarator

# notice the usage of the '+=' operator below
pointer += (OperatorParser(STAR) & direct_declarator) | (OperatorParser(STAR) & \
    pointer)

# notice the usage of the '+=' operator below
function_pointer_declarator += OperatorParser(L_PAR) & pointer & \
    OperatorParser(R_PAR) & OperatorParser(L_PAR) & Optional(parameter_list) & \
    OperatorParser(R_PAR)

# notice the usage of the '+=' operator below
declarator += pointer | direct_declarator

rhs = RecursiveParser()

declarator_with_modifier = \
    (pointer | direct_declarator) & \
    Optional(OperatorParser(EQUAL) & \
    rhs)

member_declaration = type_specifier & declarator & OperatorParser(SEMICOLON)

# main groups:
#              struct_declaration,
#              union_declaration,
#              enum_declaration,
#              typedef_declaration,
#              function_declaration,
#              variable_declaration,
#              function_definition
#

#
# struct declaration
#
struct_declaration = \
    KeywordParser(STRUCT) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & Repeat(member_declaration) & \
    OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

#
# union declaration
#
union_declaration = \
    KeywordParser(UNION) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & Repeat(member_declaration) & \
    OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

#
# typedef declaration
#
typedef_declaration = \
    (KeywordParser(TYPEDEF) & type_specifier & declarator & \
    OperatorParser(SEMICOLON)) | \
    (KeywordParser(TYPEDEF) & SymbolsParser(IDENTIFIER) & \
    declarator & OperatorParser(SEMICOLON));

additional_declarator_with_modifier = \
        OperatorParser(COMMA) & declarator_with_modifier

#
# variable declaration
#
variable_declaration = \
    (type_specifier & declarator_with_modifier << \
        ZeroOrMore(additional_declarator_with_modifier) & \
        OperatorParser(SEMICOLON)) | \
    (SymbolsParser(IDENTIFIER) & declarator_with_modifier << \
        ZeroOrMore(additional_declarator_with_modifier) & \
        OperatorParser(SEMICOLON))

constant = \
    (Optional(unop) & SymbolsParser(FLOAT_CONSTANT)) | \
    (Optional(unop) & SymbolsParser(IDENTIFIER)) | \
    (Optional(unop) & SymbolsParser(INT_CONSTANT)) | \
    (Optional(unop) & SymbolsParser(CHAR_CONSTANT))

enumerator = \
    (SymbolsParser(IDENTIFIER) & OperatorParser(EQUAL) & \
        constant) | \
    SymbolsParser(IDENTIFIER)

#
#  enum declaration
#
enum_declaration = \
    KeywordParser(ENUM) & SymbolsParser(IDENTIFIER) & \
    OperatorParser(L_BRACE) & enumerator << ZeroOrMore(OperatorParser(COMMA) & \
    enumerator) & OperatorParser(R_BRACE) & OperatorParser(SEMICOLON)

#
# function declaration
#

pointer_function = RecursiveParser()

array_function_declarator = \
    (OperatorParser(L_PAR) & pointer_function & \
    OperatorParser(R_PAR) & Repeat(OperatorParser(L_BRACKET) & \
    SymbolsParser(INT_CONSTANT) & OperatorParser(R_BRACKET)))

direct_function_declarator = \
    array_function_declarator | \
    (SymbolsParser(IDENTIFIER) & OperatorParser(L_PAR) << \
        ZeroOrMore(parameter_list) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & pointer_function & \
        OperatorParser(R_PAR) & OperatorParser(L_PAR) << \
        ZeroOrMore(parameter_list) & OperatorParser(R_PAR))

# notice the usage of the '+=' operator below
pointer_function += \
    (OperatorParser(STAR) & direct_function_declarator) | \
    (OperatorParser(STAR) & pointer_function)

function_declarator = \
    direct_function_declarator | pointer_function

function_declaration = \
    (type_specifier & function_declarator & OperatorParser(SEMICOLON)) | \
    (SymbolsParser(IDENTIFIER) & function_declarator & \
        OperatorParser(SEMICOLON))

#
# function definition
#
arrayref_compref = RecursiveParser()

value = RecursiveParser()

# "best pad - buffer ts %" GST_TIME_FORMAT " dur %" GST_TIME_FORMAT
string_value = RecursiveParser()
string_value += \
    (SymbolsParser(STRING_IDENTIFIER) | SymbolsParser(IDENTIFIER)) & \
        ZeroOrMore(string_value)

# [1]
reflist = OperatorParser(L_BRACKET) & value & OperatorParser(R_BRACKET)

# abc[1][2][3]
arrayref = SymbolsParser(IDENTIFIER) & Repeat(reflist)

# .abc
idlist = \
    memberop & SymbolsParser(IDENTIFIER)

# abc.bcd.cde
compref = \
    OperatorParser(L_PAR) & OperatorParser(STAR) & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR) & Repeat(idlist) | \
    SymbolsParser(IDENTIFIER) & Repeat(idlist)

# abc[1][2].bcd.cde[1] || abc.bcd[1][2].cde
arrayref_compref = RecursiveParser()
arrayref_compref += \
    (arrayref & memberop & SymbolsParser(IDENTIFIER) & Optional(memberop & \
        arrayref_compref)) | \
    (compref & memberop & arrayref & Optional(memberop & arrayref_compref))

value += \
    arrayref_compref | \
    arrayref | \
    compref | \
    SymbolsParser(IDENTIFIER) | \
    constant

varname = arrayref_compref | arrayref | compref | SymbolsParser(IDENTIFIER)

type_name = \
    type_specifier & Optional(abstract_declarator) | \
    SymbolsParser(IDENTIFIER) & Optional(abstract_declarator)

arglist = RecursiveParser()

call_expression = \
    SymbolsParser(IDENTIFIER) & OperatorParser(L_PAR) & Optional(arglist) & \
    OperatorParser(R_PAR)

arg = call_expression | value | string_value

arglist += \
    arg << ZeroOrMore(OperatorParser(COMMA) & arg)

simple_expression = \
    varname | \
    constant

sub_conditional_expression = \
    (value & relop & call_expression) | \
    (value & relop & simple_expression) | \
    (value & relop & value) | \
    value | \
    call_expression

main_sub_conditional_expression = \
    (Optional(OperatorParser(EXCL_MARK)) & OperatorParser(L_PAR) & \
        sub_conditional_expression & OperatorParser(R_PAR)) | \
    sub_conditional_expression

conditional_expression = \
    main_sub_conditional_expression << ZeroOrMore(compop & \
        main_sub_conditional_expression)

binary_expression = \
    (OperatorParser(L_PAR) & SymbolsParser(IDENTIFIER) & binop & value & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & constant & binop & value & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & value & binop & value & OperatorParser(R_PAR))

unary_expression = \
    simple_expression | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & OperatorParser(AMPERSAND) & varname & \
        OperatorParser(R_PAR)) | \
    call_expression | \
    (unop & SymbolsParser(IDENTIFIER)) | \
    (OperatorParser(L_PAR) & unop & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & varname) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & constant)

# notice the usage of the '+=' operator below
rhs += \
    call_expression | \
    binary_expression | \
    unary_expression

modify_expression = \
    (varname & OperatorParser(EQUAL) & rhs) | \
    (varname & compassignop & rhs) | \
    (varname & postfop) | \
    (prefop & varname) | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR) & \
        OperatorParser(EQUAL) & rhs)

basic_statement = \
    call_expression | \
    modify_expression | \
    simple_expression | \
    (OperatorParser(L_PAR) & OperatorParser(STAR) & \
        SymbolsParser(IDENTIFIER) & OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & OperatorParser(AMPERSAND) & varname & \
        OperatorParser(R_PAR)) | \
    (unop & SymbolsParser(IDENTIFIER)) | \
    (OperatorParser(L_PAR) & unop & SymbolsParser(IDENTIFIER) & \
        OperatorParser(R_PAR)) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & \
        varname) | \
    (OperatorParser(L_PAR) & type_name & OperatorParser(R_PAR) & \
        constant)

statement = RecursiveParser()

stop_statement = RecursiveParser()

compound_statement = \
    OperatorParser(L_BRACE) & ZeroOrMore(variable_declaration) & \
    ZeroOrMore(statement) & Optional(stop_statement) & \
    OperatorParser(R_BRACE)

default_statement = \
    (KeywordParser(DEFAULT) & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(statement) & \
        Optional(stop_statement) & \
        OperatorParser(R_BRACE)) | \
    (KeywordParser(DEFAULT) & OperatorParser(COLON) & \
        ZeroOrMore(statement) & \
        Optional(stop_statement)) | \
    (KeywordParser(DEFAULT) & OperatorParser(COLON))

case_statement = \
    (KeywordParser(CASE) & constant & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(statement) & \
        Optional(stop_statement) & \
        OperatorParser(R_BRACE)) | \
    (KeywordParser(CASE) & constant & OperatorParser(COLON) & \
        ZeroOrMore(statement) & \
        Optional(stop_statement)) | \
    (KeywordParser(CASE) & constant & OperatorParser(COLON))

case_statements = \
    OperatorParser(L_BRACE) & Repeat(case_statement) & \
        Optional(default_statement) & OperatorParser(R_BRACE)

goto_statement = \
    KeywordParser(GOTO) & SymbolsParser(IDENTIFIER) & OperatorParser(SEMICOLON)

if_statement = \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & statement & \
        KeywordParser(ELSE) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement & KeywordParser(ELSE) & \
        compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement & KeywordParser(ELSE) & \
        statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & statement & KeywordParser(ELSE) & statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(IF) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & statement)

# notice the usage of the '+=' operator below
statement += \
    stop_statement | \
    goto_statement | \
    compound_statement | \
    (basic_statement & OperatorParser(SEMICOLON)) | \
    if_statement | \
    (KeywordParser(WHILE) & OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(DO) & compound_statement & KeywordParser(WHILE) & \
        OperatorParser(L_PAR) & conditional_expression & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(FOR) & OperatorParser(L_PAR) & Optional(basic_statement) & \
        OperatorParser(SEMICOLON) & Optional(conditional_expression) & \
        OperatorParser(SEMICOLON) & Optional(basic_statement) & \
        OperatorParser(R_PAR) & compound_statement) | \
    (KeywordParser(SWITCH) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & case_statements)

dead_code = \
    (KeywordParser(BREAK) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(CONTINUE) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & value & OperatorParser(SEMICOLON)) | \
    (KeywordParser(RETURN) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON))

# notice the usage of the '+=' operator below
stop_statement += \
    (KeywordParser(RETURN) & statement & ZeroOrMore(dead_code)) | \
    (KeywordParser(BREAK) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(CONTINUE) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & binary_expression & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & value & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & OperatorParser(L_PAR) & value & \
        OperatorParser(R_PAR) & OperatorParser(SEMICOLON) & \
        ZeroOrMore(dead_code)) | \
    (KeywordParser(RETURN) & SymbolsParser(STRING_IDENTIFIER) & \
        OperatorParser(SEMICOLON)) | \
    (goto_statement & ZeroOrMore(dead_code))

labeled_statement = \
    (SymbolsParser(IDENTIFIER) & OperatorParser(COLON) & \
        OperatorParser(L_BRACE) & ZeroOrMore(statement) & \
        ZeroOrMore(stop_statement) & OperatorParser(R_BRACE)) | \
    (SymbolsParser(IDENTIFIER) & OperatorParser(COLON) & \
        ZeroOrMore(statement) & ZeroOrMore(stop_statement))

function_body = \
    OperatorParser(L_BRACE) & ZeroOrMore(variable_declaration) & \
    ZeroOrMore(statement) & ZeroOrMore(stop_statement) & \
    ZeroOrMore(labeled_statement) & OperatorParser(R_BRACE)

function_definition = \
    (type_specifier & function_declarator & function_body) | \
    (SymbolsParser(IDENTIFIER) & function_declarator & function_body)

declaration_or_definition = \
    struct_declaration | \
    union_declaration | \
    typedef_declaration | \
    enum_declaration | \
    variable_declaration | \
    function_declaration | \
    function_definition

#
# translation unit will parse the whole program
#
translation_unit = \
    CheckErrors(AllTokensConsumed(Repeat(declaration_or_definition)))
