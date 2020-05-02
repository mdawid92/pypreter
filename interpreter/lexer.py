from sly import Lexer
import token


def group(*choices): return '(' + '|'.join(choices) + ')'


class PyLexer(Lexer):
    tokens = {NAME, NUMBER, STRING, NEWLINE, LPAR, RPAR, LSQB, RSQB, COLON, COMMA, SEMI, PLUS, MINUS, STAR, SLASH,
              VBAR, AMPER, LESS, GREATER, EQUAL, DOT, PERCENT, LBRACE, RBRACE, EQEQUAL, NOTEQUAL}
    ignore = '\t '
    # ENDMARKER = 0
    NAME = r'\w+'
    NUMBER = r'\d+'  # TODO: only int now
    STRING = r'"[^\n"\\]*(?:\\.[^\n"\\]*)*"'  # TODO: only " type of string
    NEWLINE = r'\n'
    # INDENT = 5
    # DEDENT = 6
    LPAR = r'\('
    RPAR = r'\)'
    LSQB = r'\['
    RSQB = r'\]'
    COLON = r':'
    COMMA = r','
    SEMI = r';'
    PLUS = r'\+'
    MINUS = r'-'
    STAR = r'\*'
    SLASH = r'/'
    VBAR = r'\|'
    AMPER = r'&'
    LESS = r'<'
    GREATER = r'>'
    DOT = r'\.'
    PERCENT = r'%'
    LBRACE = r'{'
    RBRACE = r'}'
    EQEQUAL = r'=='
    NOTEQUAL = r'!='
    EQUAL = r'='
    # LESSEQUAL = r'<='
    # GREATEREQUAL = r'>='
    # TILDE = r'~'
    # CIRCUMFLEX = r'\^'
    # DOUBLESTAR = r'\*\*'
    # PLUSEQUAL = r'\+='
    # MINEQUAL = r'-='
    # STAREQUAL = r'\*='
    # SLASHEQUAL = r'/\\'
    # PERCENTEQUAL = r'%='
    # AMPEREQUAL = r'&='
    # VBAREQUAL = r'|='
    # CIRCUMFLEXEQUAL = r'\^='
    #
    # DOUBLESLASH = r'//'
    # DOUBLESLASHEQUAL = r'//='
    # AT = r'@'
    # ATEQUAL = r'@='
    # RARROW = 51
    # ELLIPSIS = 52
    # OP = 53
    # AWAIT = 54
    # ASYNC = 55
    # ERRORTOKEN = 56
    # N_TOKENS = 57
    # NT_OFFSET = 256

