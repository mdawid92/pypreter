from sly import Lexer
import token
import ast


def group(*choices): return '(' + '|'.join(choices) + ')'


class PyLexer(Lexer):
    tokens = {ENDMARKER, NAME, NUMBER, STRING, NEWLINE, LPAR, RPAR, LSQB, RSQB, COLON, COMMA, SEMI, PLUS, MINUS, STAR, SLASH,
              VBAR, AMPER, LESS, GREATER, EQUAL, DOT, PERCENT, LBRACE, RBRACE, EQEQUAL, NOTEQUAL, TILDE, CIRCUMFLEX,
              DOUBLESTAR, PLUSEQUAL, MINEQUAL, STAREQUAL, SLASHEQUAL, PERCENTEQUAL, AMPEREQUAL, VBAREQUAL, CIRCUMFLEXEQUAL,
              NONE, FALSE, TRUE, NOT, AND, OR, FOR, IN}
    ignore = '\t '

    NEWLINE = r'\n'
    ENDMARKER = r'\n\Z'
    NUMBER = r'\d+'  # TODO: only int now
    NOT = r'NOT'
    AND = r'and'
    OR = r'or'
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
    # LESSEQUAL = r'<='
    # GREATEREQUAL = r'>='
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

    FOR = r"for"
    IN = r"in"

    @_(r"None")
    def NONE(self, t):
        t.value = None
        return t
    @_(r"True")
    def TRUE(self, t):
        t.value = True
        return t
    @_(r"False")
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
        print(t)
        return t

    def NAME(self, t):
        tmp = ast.Name(id=t.value, ctx=ast.Load())
        t.value = tmp
        print(t)
        return t

