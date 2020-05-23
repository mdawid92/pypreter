small_stmt: expr_stmt

expr_stmt: testlist_star_expr augassign testlist
           testlist_star_expr EQUAL testlist_star_expr

testlist_star_expr: test { COMMA test }

augassign: PLUSEQUAL
           MINEQUAL
           STAREQUAL
           SLASHEQUAL
           PERCENTEQUAL

test: or_test
      or_test IF or_test ELSE test

or_test: and_test
         and_test OR and_test

and_test: not_test
          not_test AND not_test

not_test: NOT not_test
          comparision

comparision: expr { comp_op expr }

comp_op: LESS
         LESSEQUAL
         GREATER
         GREATEREQUAL
         EQEQUAL
         NOTEQUAL

star_expr: STAR expr

expr: xor_expr
      xor_expr VBAR xor_expr

xor_expr: and_expr
          and_expr CIRCUMFLEX and_expr

and_expr: arith_expr
          arith_expr AMPER arith_expr

arith_expr: term PLUS term
            term MINUS term
            term

term: factor STAR factor
      factor SLASH factor
      factor PERCENT factor
      factor

factor: PLUS factor
        MINUS factor
        TILDE factor
        power

power: atom_expr
       atom_expr DOUBLESTAR factor

atom_expr: atom trailer
           atom

atom: LPAR [ testlist_comp ] RPAR
      LSQB [ testlist_comp ] RSQB
      LBRACE [ dictorsetmaker ] RBRACE
      NAME
      NUMBER
      STRING
      NONE
      TRUE
      FALSE

testlist_comp: test comp_for
               test { COMMA test }

trailer: DOT NAME

subscriptlist: subscript { COMMA subscript }

subscript: test

sliceop: COLON [ test ]

exprlist: expr { COMMA expr }

testlist: test { COMMA test }

dictorsetmaker: test COLON test comp_for
                test COLON test { COMMA test COLON test }
                test comp_for
                test { COMMA test }

arglist: argument { COMMA argument }

argument: test [ comp_for ]

comp_for: FOR exprlist IN or_test
