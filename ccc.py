# Crappy C Compiler - ccc

from ast import Return
import subprocess
import pathlib
import os
import sys
import pprint

from tokens import *
from cparser import *
from errorlog import *
from codegen import *

SOURCE = """
int main() {
  return 3 + 2 + 1;
}
"""

EXPECTED_AST = """
trans_unit
  + func_def
    + IDENT: main
    + compound_stmnt
      + stmnt
        + jump_stmnt
          + return_stmnt
            + exp
              + assignment_exp
                + conditional_exp
                  + logical_or_exp
                    + logical_and_exp
                      + inclusive_or_exp
                        + exclusive_or_exp
                          + and_exp
                            + equality_exp
                              + relational_exp
                                + shift_exp
                                  + add_exp
                                    + mult_exp
                                    | + cast_exp
                                    |   + unary_exp
                                    |     + postfix_exp
                                    |       + prim_exp
                                    |         + constant
                                    |           + int_const: 3
                                    + add_exp'
                                      + mult_exp
                                      | + cast_exp
                                      |   + unary_exp
                                      |     + postfix_exp
                                      |       + prim_exp
                                      |         + constant
                                      |           + int_const: 2
                                      + add_exp'
                                        + mult_exp
                                          + cast_exp
                                            + unary_exp
                                              + postfix_exp
                                                + prim_exp
                                                  + constant
                                                    + int_const: 1
"""

if __name__ == "__main__":
    print("CWD: {}".format(os.getcwd()))
    
    #source = pathlib.Path('/storage/emulated/0/Documents/Pydroid3/ccc/input.c').read_text()
    source = pathlib.Path('input.c').read_text()
    print(f"SOURCE\n{source}\nEND SOURCE")

    lexer = Lexer()
    tkns = lexer.tokenize(source)
    print("TOKENS")
    pprint.pprint(tkns)
    print("END TOKENS")
    parser = Parser()
    ast = parser.parse(tkns)
    if ast == None:
        print("ERROR:")
        print(f"{GlobalErrorLog}")
        exit(1)
    print(ast.dump())
    code_gen = CodeGenerator()
    asm = code_gen.generate(ast)

    pathlib.Path("out.asm").write_text(asm)

    if True:
        subprocess.run(
            ['../customasm/target/release/customasm', '../../tcsc-8000.asm', 'out.asm', '-f', 'binary', '-o', 'out.bin', '--', '-f', 'symbols', '-o', 'out.symbols.txt', '--', '-f', 'annotated,base:16,group:4', '-o', 'out.annotated.txt'],
            stdout=sys.stdout,
            stderr=sys.stderr,
            check=True)
    
