from interpreter.lexer import PyLexer
from interpreter.parser import PyParser

if __name__ == '__main__':
    lexer = PyLexer()
    parser = PyParser()
    while True:
        line = input("> ")
        if line == "DEBUG":
            print("globals")
            for k, v in parser.globals.items():
                if not k.startswith("__"):
                    print(f"${k}: ${v}")
            print("locals")
            for k, v in parser.locals.items():
                if not k.startswith("__"):
                    print(f"${k}: ${v}")
            continue
        tokens = lexer.tokenize(line)
        # for token in tokens:
        #     print(token)
        tmp = parser.parse(lexer.tokenize(line))
        # print(tmp)
