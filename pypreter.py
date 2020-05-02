from interpreter.lexer import PyLexer


if __name__ == '__main__':
    lexer = PyLexer()
    while True:
        line = input(">")
        tokens = lexer.tokenize(line)
        for token in tokens:
            print(token)

