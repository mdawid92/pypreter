from sly import Lexer

import ast


def group(*choices): return '(' + '|'.join(choices) + ')'


class PyLexer(Lexer):
    tokens = {ENDMARKER, NAME, NUMBER, STRING, NEWLINE, LPAR, RPAR, LSQB, RSQB, COLON, COMMA, SEMI, PLUS, MINUS, STAR,
              SLASH,
              VBAR, AMPER, LESS, LESSEQUAL, GREATER, GREATEREQUAL, EQUAL, DOT, PERCENT, LBRACE, RBRACE, EQEQUAL,
              NOTEQUAL, TILDE, CIRCUMFLEX,
              DOUBLESTAR, PLUSEQUAL, MINEQUAL, STAREQUAL, SLASHEQUAL, PERCENTEQUAL, AMPEREQUAL, VBAREQUAL,
              CIRCUMFLEXEQUAL,
              NONE, FALSE, TRUE, NOT, AND, OR, FOR, IN, IF, ELSE}
    ignore = '\t '

    NEWLINE = r'\n'
    ENDMARKER = r'\n\Z'
    NUMBER = r'\d+'  # TODO: only int now
    NOT = r'not'
    AND = r'and'
    OR = r'or'
    TRUE = r'True'
    FALSE = r'False'
    NONE = r'None'
    FOR = r"for"
    IN = r"in"
    IF = r'if'
    ELSE = r'else'
    NAME = r'\w+'

    STRING = r'"[^\n"\\]*(?:\\.[^\n"\\]*)*"'  # TODO: only " type of string
    # INDENT = 5
    # DEDENT = 6
    LPAR = r'\('
    RPAR = r'\)'
    LSQB = r'\['
    RSQB = r'\]'
    COLON = r':'
    COMMA = r','
    SEMI = r';'

    DOT = r'\.'
    LBRACE = r'{'
    RBRACE = r'}'
    EQEQUAL = r'=='
    NOTEQUAL = r'!='
    LESSEQUAL = r'<='
    GREATEREQUAL = r'>='
    TILDE = r'~'
    CIRCUMFLEX = r'\^'
    DOUBLESTAR = r'\*\*'
    PLUSEQUAL = r'\+='
    MINEQUAL = r'-='
    STAREQUAL = r'\*='
    SLASHEQUAL = r'/='
    PERCENTEQUAL = r'%='
    AMPEREQUAL = r'&='
    VBAREQUAL = r'\|='
    CIRCUMFLEXEQUAL = r'\^='
    #
    # DOUBLESLASH = r'//'
    # DOUBLESLASHEQUAL = r'//='
    # AT = r'@'
    # ATEQUAL = r'@='
    EQUAL = r'='
    PLUS = r'\+'
    MINUS = r'-'
    STAR = r'\*'
    SLASH = r'/'
    VBAR = r'\|'
    AMPER = r'&'
    LESS = r'<'
    GREATER = r'>'
    PERCENT = r'%'

    # RARROW = 51
    # ELLIPSIS = 52
    # OP = 53
    # AWAIT = 54
    # ASYNC = 55
    # ERRORTOKEN = 56
    # N_TOKENS = 57
    # NT_OFFSET = 256

    def NONE(self, t):
        t.value = None
        return t

    def TRUE(self, t):
        t.value = True
        return t

    def FALSE(self, t):
        t.value = False
        return t

    def STRING(self, t):
        tmp = ast.Str(s=t.value.strip("\"'"))
        t.value = tmp
        return t

    def NUMBER(self, t):
        tmp = ast.Num(n=int(t.value))
        t.value = tmp
        return t

    def NAME(self, t):
        tmp = ast.Name(id=t.value, ctx=ast.Load())
        t.value = tmp
        return t
