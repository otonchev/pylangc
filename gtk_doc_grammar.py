# This file defines pylangparser's style GTK-Doc grammar.
#
# Copyright, 2015, Ognyan Tonchev (otonchev at gmail com)
#

import sys
sys.path.append("../..")

from pylangparser import *

TYPE = Keyword(r'\(type [A-Za-z\.]+\)')
ALLOW_NONE = Keyword(r'\(allow-none\)')
FLOATING = Keyword(r'\(transfer floating\)')
FULL = Keyword(r'\(transfer full\)')
NONE = Keyword(r'\(transfer none\)')
CONTAINER = Keyword(r'\(transfer container\)')
NULLABLE = Keyword(r'\(nullable\)')
OUT = Keyword(r'\(out\)')
RETURNS = Keyword(r'Returns')

KEYWORDS = ALLOW_NONE & TYPE & FULL & NONE & CONTAINER & FLOATING & NULLABLE & \
    OUT & RETURNS

COLON = Operator(r':')
AT = Operator(r'@')
COMMA = Ignore(r',')
DOT = Ignore(r'.')

OPERATORS = AT & COLON & COMMA & DOT

VARARGS = Symbols(r'\.\.\.')
IDENTIFIER = Symbols(r'[A-Za-z_]+[A-Za-z0-9_]*')
WORD = Symbols(r'[A-Za-z#%\(\)]+')
COMMENT_START = Symbols(r'/\*\*')
COMMENT_END = Symbols(r'\*/')

SYMBOLS = VARARGS & IDENTIFIER & WORD
COMMENT_SYMBOLS = COMMENT_START & COMMENT_END

COMMENT_LINE = Ignore(r'\*')
IGNORE_CHARS = Ignore(r'[ \t\v\f\n]+')

IGNORES = IGNORE_CHARS & COMMENT_LINE

# order is important, first token that is matches will be considered
# that is why it is important to put '*/' before '*', for example
TOKENS = COMMENT_SYMBOLS & KEYWORDS & SYMBOLS & OPERATORS & IGNORES

IgnoreTokensInAST(AT & COLON & DOT & COMMENT_START & COMMENT_END)

ignored_word = \
    IgnoreResult(SymbolsParser(WORD) | SymbolsParser(IDENTIFIER) | \
    (OperatorParser(AT) & SymbolsParser(IDENTIFIER)))
annotation = \
    KeywordParser(ALLOW_NONE) | KeywordParser(FLOATING) | \
    KeywordParser(NULLABLE) | KeywordParser(FULL) | KeywordParser(TYPE)
func_name = SymbolsParser(IDENTIFIER)
func = func_name & OperatorParser(COLON)
returns = \
    KeywordParser(RETURNS) << OperatorParser(COLON) << \
    Optional(ZeroOrMore(annotation) << OperatorParser(COLON))
arg = \
    OperatorParser(AT) << \
    (SymbolsParser(IDENTIFIER) | SymbolsParser(VARARGS)) << \
    OperatorParser(COLON) << \
    Optional(ZeroOrMore(annotation) << OperatorParser(COLON))
func_desc = ZeroOrMore(arg | returns | ignored_word)
description = \
    SymbolsParser(COMMENT_START) << \
    func << \
    func_desc << \
    SymbolsParser(COMMENT_END)

parser = AllTokensConsumed(ZeroOrMore(description))
