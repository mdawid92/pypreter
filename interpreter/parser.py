import sys

from sly import Parser
from interpreter.lexer import PyLexer
import ast


class PyParser(Parser):
    debugfile = 'parser.out'
    tokens = PyLexer.tokens

    def __init__(self):
        self.globals = {}
        self.locals = {}

    def _execute(self, stmt):
        if not isinstance(stmt, ast.AST):
            return stmt
        if isinstance(stmt, ast.Module):
            for i in stmt.body:
                self._execute(i)
            return True
        elif isinstance(stmt, ast.Assign):
            values = [self._execute(value) for value in stmt.value]
            for i, target in enumerate(stmt.targets):
                self.globals[target.id] = values[i]
            return True
        elif isinstance(stmt, ast.AugAssign):
            if isinstance(stmt.op, ast.Add):
                self.globals[stmt.target.id] += self._execute(stmt.value)
            elif isinstance(stmt.op, ast.Sub):
                self.globals[stmt.target.id] -= self._execute(stmt.value)
            elif isinstance(stmt.op, ast.Mult):
                self.globals[stmt.target.id] *= self._execute(stmt.value)
            elif isinstance(stmt.op, ast.Div):
                self.globals[stmt.target.id] /= self._execute(stmt.value)
            elif isinstance(stmt.op, ast.Mod):
                self.globals[stmt.target.id] %= self._execute(stmt.value)
            return True
        elif isinstance(stmt, ast.BinOp):
            if isinstance(stmt.op, ast.Add):
                return self._execute(stmt.left) + self._execute(stmt.right)
            elif isinstance(stmt.op, ast.Sub):
                return self._execute(stmt.left) - self._execute(stmt.right)
            elif isinstance(stmt.op, ast.Mult):
                return self._execute(stmt.left) * self._execute(stmt.right)
            elif isinstance(stmt.op, ast.Div):
                return self._execute(stmt.left) / self._execute(stmt.right)
            elif isinstance(stmt.op, ast.Mod):
                return self._execute(stmt.left) % self._execute(stmt.right)
        elif isinstance(stmt, ast.UnaryOp):
            if isinstance(stmt.op, ast.UAdd):
                return + self._execute(stmt.operand)
            elif isinstance(stmt.op, ast.USub):
                return - self._execute(stmt.operand)
            elif isinstance(stmt.op, ast.Invert):
                return not self._execute(stmt.right)
        elif isinstance(stmt, ast.Num):
            return stmt.n
        elif isinstance(stmt, ast.Str):
            return stmt.s
        elif isinstance(stmt, ast.Dict):
            return {self._execute(key): self._execute(value) for key, value in zip(stmt.keys, stmt.values)}
        elif isinstance(stmt, ast.Set):
            return {self._execute(element) for element in stmt.elts}
        elif isinstance(stmt, ast.List):
            return [self._execute(element) for element in stmt.elts]
        elif isinstance(stmt, ast.Tuple):
            return tuple(self._execute(element) for element in stmt.elts)
        elif isinstance(stmt, ast.Name):
            if isinstance(stmt.ctx, ast.Store):
                return stmt.id
            if isinstance(stmt.ctx, ast.Load):
                return self.globals[stmt.id]
        else:
            raise RuntimeError(f"NOT IMPLEMENTED: {stmt.__class__}")

    def execute(self, stmt):
        unparsed = ast.dump(stmt)
        print(unparsed)

        self._execute(stmt)
        print(self.globals)
        print(self.locals)

    def error(self, token):
        super().error(token)

    @_('expr_stmt')
    def small_stmt(self, p):
        mod = ast.Module(body=[p.expr_stmt])
        self.execute(mod)
        return mod

    # expr_stmt: testlist_star_expr ( augassign (yield_expr|testlist) |
    #                     ('=' (yield_expr|testlist_star_expr))*)
    @_('testlist_star_expr augassign testlist')
    def expr_stmt(self, p):
        augassign = p.augassign
        name = p.testlist_star_expr
        assign = ast.AugAssign(
            target=ast.Name(id=name.id, ctx=ast.Store()),
            op=augassign.op,
            value=p.testlist
        )
        return assign

    @_('testlist_star_expr EQUAL testlist_star_expr')
    def expr_stmt(self, p):
        names = p.testlist_star_expr0
        assign = ast.Assign(
            targets=[ast.Name(id=name.id, ctx=ast.Store()) for name in names],
            value=p.testlist_star_expr1
        )
        return assign

    # testlist_star_expr: (test|star_expr) (',' (test|star_expr))* [',']
    @_("test { COMMA test }")
    def testlist_star_expr(self, p):
        return [p.test0] + p.test1

    # augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
    #             '<<=' | '>>=' | '**=' | '//=')
    # PLUSEQUAL, MINEQUAL, STAREQUAL, SLASHEQUAL, PERCENTEQUAL, AMPEREQUAL, VBAREQUAL, CIRCUMFLEXEQUAL
    @_("PLUSEQUAL")
    def augassign(self, p):
        return ast.AugAssign(op=ast.Add())

    @_("MINEQUAL")
    def augassign(self, p):
        return ast.AugAssign(op=ast.Sub())

    @_("STAREQUAL")
    def augassign(self, p):
        return ast.AugAssign(op=ast.Mult())

    @_("SLASHEQUAL")
    def augassign(self, p):
        return ast.AugAssign(op=ast.Div())

    @_("PERCENTEQUAL")
    def augassign(self, p):
        return ast.AugAssign(op=ast.Mod())

    # test: or_test ['if' or_test 'else' test] | lambdef
    @_('or_test')
    def test(self, p):
        return p.or_test

    # test_nocond: or_test | lambdef_nocond
    # lambdef: 'lambda' [varargslist] ':' test
    # lambdef_nocond: 'lambda' [varargslist] ':' test_nocond
    # or_test: and_test ('or' and_test)*
    @_('and_test')
    def or_test(self, p):
        return p.and_test

    @_('and_test OR and_test')
    def or_test(self, p):
        return ast.BinOp(left=p.and_test0, op=ast.BitOr(), right=p.and_test1)

    # and_test: not_test ('and' not_test)*
    @_('not_test')
    def and_test(self, p):
        return p.not_test

    @_('not_test AND not_test')
    def and_test(self, p):
        return ast.BinOp(left=p.not_test0, op=ast.BitAnd(), right=p.not_test1)

    # not_test: 'not' not_test | comparison
    @_('NOT not_test')
    def not_test(self, p):
        return ast.UnaryOp(op=ast.Invert(), operand=p.factor)

    @_('comparision')
    def not_test(self, p):
        return p.comparision

    # comparison: expr (comp_op expr)*
    @_('expr')
    def comparision(self, p):
        return p.expr

    @_('expr LESS expr')
    def comparision(self, p):
        return ast.BinOp(left=p.expr1, op=ast.Lt, right=p.expr1)

    # comp_op: '<'|'>'|'=='|'>='|'<='|'!='|'in'|'not' 'in'|'is'|'is' 'not'
    # star_expr: '*' expr
    @_('STAR expr')
    def star_expr(self, p):
        return ast.Starred(value=p.expr)

    # expr: xor_expr ('|' xor_expr)*
    @_('xor_expr')
    def expr(self, p):
        return p.xor_expr

    @_('xor_expr VBAR xor_expr')
    def expr(self, p):
        return ast.BinOp(left=p.xor_expr0, op=ast.BitOr(), right=p.xor_expr1)

    # xor_expr: and_expr ('^' and_expr)*
    @_('and_expr')
    def xor_expr(self, p):
        return p.and_expr

    @_('and_expr CIRCUMFLEX and_expr')
    def xor_expr(self, p):
        return ast.BinOp(left=p.and_expr0, op=ast.BitXor(), right=p.and_expr1)

    # and_expr: shift_expr ('&' shift_expr)*
    # and_expr: arith_expr ('&' arith_expr)*
    @_('arith_expr')
    def and_expr(self, p):
        return p.arith_expr

    @_('arith_expr AMPER arith_expr')
    def and_expr(self, p):
        return ast.BinOp(left=p.arith_expr0, op=ast.BitAnd(), right=p.arith_expr1)

    # skipped this shift
    # shift_expr: arith_expr (('<<'|'>>') arith_expr)*

    # arith_expr: term (('+'|'-') term)*
    @_('term PLUS term')
    def arith_expr(self, p):
        return ast.BinOp(left=p.term0, op=ast.Add(), right=p.term1)

    @_('term MINUS term')
    def arith_expr(self, p):
        return ast.BinOp(left=p.term0, op=ast.Sub(), right=p.term1)

    @_('term')
    def arith_expr(self, p):
        return p.term

    # term: factor (('*'|'@'|'/'|'%'|'//') factor)*
    @_("factor STAR factor")
    def term(self, p):
        return ast.BinOp(left=p.factor0, op=ast.Mult(), right=p.factor1)

    @_("factor SLASH factor")
    def term(self, p):
        return ast.BinOp(left=p.factor0, op=ast.Div(), right=p.factor1)

    @_("factor PERCENT factor")
    def term(self, p):
        return ast.BinOp(left=p.factor0, op=ast.Mod(), right=p.factor1)

    @_("factor")
    def term(self, p):
        return p.factor

    # factor: ('+'|'-'|'~') factor | power
    @_('PLUS factor')
    def factor(self, p):
        return ast.UnaryOp(op=ast.UAdd(), operand=p.factor)

    @_('MINUS factor')
    def factor(self, p):
        return ast.UnaryOp(op=ast.USub(), operand=p.factor)

    @_('TILDE factor')
    def factor(self, p):
        return ast.UnaryOp(op=ast.Invert(), operand=p.factor)

    @_('power')
    def factor(self, p):
        return p.power

    # power: atom_expr ['**' factor]
    @_('atom_expr')
    def power(self, p):
        return p.atom_expr

    # atom_expr: [AWAIT] atom trailer*
    @_('atom trailer')
    def atom_expr(self, p):
        return p.atom

    @_('atom')
    def atom_expr(self, p):
        return p.atom

    # atom: ('(' [yield_expr|testlist_comp] ')' |
    #       '[' [testlist_comp] ']' |
    #       '{' [dictorsetmaker] '}' |
    #       NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
    @_("LPAR [ testlist_comp ] RPAR")
    def atom(self, p):
        if p.testlist_comp is not None:
            return ast.Tuple(elts=p.testlist_comp)
        else:
            return ast.Tuple(elts=[])

    @_("LSQB [ testlist_comp ] RSQB")
    def atom(self, p):
        if p.testlist_comp is not None:
            return ast.List(elts=p.testlist_comp)
        else:
            return ast.List(elts=[])

    @_("LBRACE [ dictorsetmaker ] RBRACE")
    def atom(self, p):
        return p.dictorsetmaker or ast.Dict(keys=[], values=[])

    @_("NAME")
    def atom(self, p):
        return p.NAME

    @_("NUMBER")
    def atom(self, p):
        return p.NUMBER

    @_("STRING")
    def atom(self, p):
        return p.STRING

    @_('NONE')
    def atom(self, p):
        return p.NONE

    @_('TRUE')
    def atom(self, p):
        return p.TRUE

    @_('FALSE')
    def atom(self, p):
        return p.FALSE

    # testlist_comp: (test | star_expr)(comp_for | (','(test | star_expr)) * [','])
    @_('test comp_for')
    def testlist_comp(self, p):
        return []

    @_('test { COMMA test }')
    def testlist_comp(self, p):
        return [p.test0] + p.test1

    @_('DOT NAME')
    def trailer(self, p):
        return p.NAME

    # subscriptlist: subscript (',' subscript)* [',']
    @_('subscript { COMMA subscript }')
    def subscriptlist(self, p):
        return [p.subscript0] + p.subscript1

    # subscript: test | [test] ':' [test] [sliceop]
    @_('test')
    def subscript(self, p):
        return p

    # sliceop: ':' [test]
    @_('COLON')
    def sliceop(self, p):
        return p

    @_('COLON test')
    def sliceop(self, p):
        return p

    # exprlist: (expr | star_expr)(','(expr | star_expr)) * [',']
    @_("expr { COMMA expr }")
    def exprlist(self, p):
        return [p.expr0] + p.expr1

    # testlist: test (',' test)* [',']
    @_('test { COMMA test } ')
    def testlist(self, p):
        return [p.test0] + p.test1

    # dictorsetmaker: ( ((test ':' test | '**' expr)
    #                    (comp_for | (',' (test ':' test | '**' expr))* [','])) |
    #                   ((test | star_expr)
    #                    (comp_for | (',' (test | star_expr))* [','])) )
    @_('test COLON test comp_for')
    def dictorsetmaker(self, p):
        return ast.DictComp(key=p.test0, value=p.test1, generators=p.comp_for)

    @_('test COLON test { COMMA test COLON test }')
    def dictorsetmaker(self, p):
        d = ast.Dict(keys=[p.test0] + p.test2, values=[p.test1] + p.test3)
        return d

    @_('test comp_for')
    def dictorsetmaker(self, p):
        return ast.SetComp(elt=p.test, generators=p.comp_for)

    @_('test { COMMA test }')
    def dictorsetmaker(self, p):
        return ast.Set(elts=[p.test0] + p.test1)

    # arglist: argument (',' argument)*  [',']
    @_('argument { COMMA argument }')
    def arglist(self, p):
        return [p.argument0] + p.argument1

    # argument: ( test [comp_for] |
    #            test '=' test |
    #            '**' test |
    #            '*' test )
    @_('test')
    def argument(self, p):
        return p.test

    # comp_iter: comp_for | comp_if
    # comp_for: 'for' exprlist 'in' or_test [comp_iter]
    @_("FOR exprlist IN or_test")
    def comp_for(self, p):
        return (p.exprlist, p.or_test)
    # comp_if: 'if' test_nocond [comp_iter]


def debug(stmt, level=""):
    if isinstance(stmt, ast.AST):
        print(f"{level}: {ast.dump(stmt)}")
    else:
        print(f"{level}: {stmt}")
