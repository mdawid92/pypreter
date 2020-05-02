from sly import Parser
from interpreter.lexer import PyLexer


class PyParser(Parser):
    tokens = PyLexer.tokens

