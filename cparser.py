from __future__ import annotations
from typing import Any, List, Literal, Optional, Tuple, cast
from errorlog import GlobalErrorLog

from tokens import *

DOCS = """
trans_unit					:= func_def* ;

func_def	   				:= 'int', IDENT, '(', ')', compound_stmnt ;

compound_stmnt 				:= '{', stmnt*, '}' ;

stmnt 						:= jump_stmnt ;

jump_stmnt 					:= return_stmnt ;

return_stmnt   				:= "return", exp, ';' ;

exp 						:= assignment_exp, exp' ;
							  
assignment_exp 				:= conditional_exp
			   				 | unary_exp, assignment_op, assignment_exp ;
								
conditional_exp 			:= logical_or_exp
							 | logical_or_exp, '?', exp, ':', conditional_exp ;

logical_or_exp 				:= logical_and_exp, logical_or_exp' ;
								
logical_and_exp 			:= inclusive_or_exp, logical_and_exp' ;

inclusive_or_exp 			:= exclusive_or_exp, inclusive_or_exp' ;
							  
exclusive_or_exp 			:= and_exp, exclusive_or_exp' ;
							  
and_exp 					:= equality_exp, and_exp' ;

equality_exp 				:= relational_exp, equality_exp' ;
							  
relational_exp 				:= shift_exp, relational_exp' ;

shift_exp 					:= add_exp, shift_exp' ;
							   
add_exp 					:= mult_exp, add_exp' ;

mult_exp 					:= cast_exp, mult_exp' ;
							  
cast_exp 					:= unary_exp
		 					 | '(', 'int', ')', cast_exp ;
							  
unary_exp 					:= postfix_exp
		  					 | '++', unary_exp
		  					 | '--', unary_exp
		  					 | unary_op, cast_exp
		  					 | 'sizeof', unary_exp
		  					 | 'sizeof', type_name ;
							   
postfix_exp 				:= prim_exp, postfix_exp' ;

primary_exp 				:= IDENT
							 | constant
							 | string
							 | '(', exp, ')' ;

exp'                        := ',', assignment_exp, exp'
							 | EMPTY ;

logical_or_exp'             := '||', logical_and_exp, logical_or_exp'
							 | EMPTY ;

logical_and_exp'            := '&&', inclusive_or_exp, logical_and_exp'
							 | EMPTY ;

inclusive_or_exp'           := '|', exclusive_or_exp, inclusive_or_exp'
							 | EMPTY ;

exclusive_or_exp'           := '^', and_exp, exclusive_or_exp'
							 | EMPTY ;

and_exp'                    := '&', equality_exp, and_exp'
							 | EMPTY ;

equality_exp'               := '==', relational_exp, equality_exp'
							 | '!=', relational_exp, equality_exp'
							 | EMPTY ;

relational_exp'             := '<', shift_exp, relational_exp'
							 | '>', shift_exp, relational_exp'
							 | '<=', shift_exp, relational_exp'
							 | '>=', shift_exp, relational_exp'
							 | EMPTY ;

shift_exp'                  := '<<', add_exp, shift_exp'
							 | '>>', add_exp, shift_exp'
							 | EMPTY ;

add_exp'                    := '+', mult_exp, add_exp'
							 | '-', mult_exp, add_exp'
							 | EMPTY ;

mult_exp'                   := '*', cast_exp, mult_exp'
							 | '/', cast_exp, mult_exp'
							 | '%', cast_exp, mult_exp'
							 | EMPTY ;

postfix_exp'                := '[', exp, ']', postfix_exp'
							 | '(', assignment_exp*, ')', postfix_exp'
							 | '.' IDENT postfix_exp'
							 | '->' IDENT postfix_exp'
							 | '++' postfix_exp'
							 | '--' postfix_exp'
							 | EMPTY ;

constant 					:= int_const ;

assignment_op 				:= '='
			  				 | '*='
			  				 | '/='
			  				 | '%='
			  				 | '+='
			  				 | '-='
			  				 | '<<='
			  				 | '>>='
			  				 | '&='
			  				 | '^='
			  				 | '|=' ;
							   
unary_op 					:= '&'
		 					 | '*'
		 					 | '+'
		 					 | '-'
		 					 | '~'
		 					 | '!' ;
"""

class BaseExpression:
	def __init__(self):
		pass

	def is_const(self) -> bool:
		return False

class ConstInt(BaseExpression):
	def __init__(self, value: int):
		self.value = value
	
	def is_const(self) -> bool:
		return True

class Const(BaseExpression):
	def __init__(self, value: ConstInt):
		self.value = value

	def is_const(self) -> bool:
		return self.value.is_const()

class PrimExp(BaseExpression):
	def __init__(self, subexp: str | Const | Exp):
		self.subexp = subexp
	
	def is_const(self) -> bool:
		if isinstance(self.subexp, str):
			return False
		else:
			return self.subexp.is_const()

class SubscriptOp:
	def __init__(self, exp: Exp):
		self.exp = exp

class FuncCall:
	def __init__(self, exps: list[AssignmentExp]):
		self.exps = exps

class MemberAccess(BaseExpression):
	def __init__(self, ident: str):
		self.ident = ident
	
	def is_const(self) -> bool:
		return False

class PointerMemberAccess(BaseExpression):
	def __init__(self, ident: str):
		self.ident = ident
	
	def is_const(self) -> bool:
		return False

class PrePostOp:
	def __init__(self, op: Literal['++', '--']):
		self.op = op

class PostfixExpP(BaseExpression):
	def __init__(self, data: tuple[Exp, PostfixExpP | None] | tuple[list[AssignmentExp], PostfixExpP | None] | tuple[MemberAccess, PostfixExpP | None] | tuple[PointerMemberAccess, PostfixExpP | None] | tuple[PrePostOp, PostfixExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			if isinstance(self.data[0], Exp):
				return self.data[0].is_const() and (self.data[1].is_const() if (self.data[1] is not None) else True)
			elif isinstance(self.data[0], list):
				c = True
				for e in self.data[0]:
					if not e.is_const():
						c = False
						break
				return c and (self.data[1].is_const() if (self.data[1] is not None) else True)
			elif isinstance(self.data[0], MemberAccess):
				return self.data[0].is_const() and (self.data[1].is_const() if (self.data[1] is not None) else True)
			elif isinstance(self.data[0], PointerMemberAccess):
				return self.data[0].is_const() and (self.data[1].is_const() if (self.data[1] is not None) else True)
			elif isinstance(self.data[0], PrePostOp):
				return (self.data[1].is_const() if (self.data[1] is not None) else True)
		else:
			return True
		return True

class PostfixExp(BaseExpression):
	def __init__(self, prim_exp: PrimExp, postfix_exp_p: PostfixExpP):
		self.prim_exp = prim_exp
		self.postfix_exp_p = postfix_exp_p
	
	def is_const(self) -> bool:
		return self.prim_exp.is_const() and self.postfix_exp_p.is_const()

class UnaryOp:
	def __init__(self, op: Literal['&', '*', '+', '-', '~', '!']):
		self.op = op

class SizeofOp:
	def __init__(self):
		pass

class TypeName:
	def __init__(self):
		# TODO: Add actual types, right now we just have int
		pass

class UnaryExp(BaseExpression):
	def __init__(self, subexp: PostfixExp | tuple[PrePostOp, UnaryExp] | tuple[UnaryOp, CastExp] | tuple[SizeofOp, UnaryExp] | tuple[SizeofOp, TypeName]):
		self.subexp = subexp
	
	def is_const(self) -> bool:
		if isinstance(self.subexp, PostfixExp):
			return self.subexp.is_const()
		elif isinstance(self.subexp, tuple):
			if isinstance(self.subexp[0], PrePostOp) and isinstance(self.subexp[1], UnaryExp):
				return self.subexp[1].is_const()
			elif isinstance(self.subexp[0], UnaryOp) and isinstance(self.subexp[1], CastExp):
				return self.subexp[1].is_const()
			elif isinstance(self.subexp[0], SizeofOp):
				if isinstance(self.subexp[1], UnaryExp):
					return self.subexp[1].is_const()
				elif isinstance(self.subexp[1], TypeName):
					return True
		return True

class CastExp(BaseExpression):
	def __init__(self, subexp: UnaryExp | tuple[TypeName, CastExp]):
		self.subexp = subexp
	
	def is_const(self) -> bool:
		if isinstance(self.subexp, UnaryExp):
			return self.subexp.is_const()
		elif isinstance(self.subexp, tuple):
			return self.subexp[1].is_const()
		return True

class MultOp:
	def __init__(self, op: Literal['*', '/', '%']):
		self.op = op

class MultExpP(BaseExpression):
	def __init__(self, data: tuple[MultOp, CastExp, MultExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[1].is_const() and (self.data[2].is_const() if self.data[2] is not None else True)
		return True

class MultExp(BaseExpression):
	def __init__(self, cast_exp: CastExp, mult_exp_p: MultExpP):
		self.cast_exp = cast_exp
		self.mult_exp_p = mult_exp_p
	
	def is_const(self) -> bool:
		return self.cast_exp.is_const() and self.mult_exp_p.is_const()

class AddOp:
	def __init__(self, op: Literal['+', '-']):
		self.op = op

class AddExpP(BaseExpression):
	def __init__(self, data: tuple[AddOp, MultExp, AddExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[1].is_const() and (self.data[2].is_const() if self.data[2] is not None else True)
		return True

class AddExp(BaseExpression):
	def __init__(self, mult_exp: MultExp, add_exp_p: AddExpP):
		self.mult_exp = mult_exp
		self.add_exp_p = add_exp_p
	
	def is_const(self) -> bool:
		return self.mult_exp.is_const() and self.add_exp_p.is_const()

class ShiftOp:
	def __init__(self, op: Literal['<<', '>>']):
		self.op = op

class ShiftExpP(BaseExpression):
	def __init__(self, data: tuple[ShiftOp, AddExp, ShiftExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[1].is_const() and (self.data[2].is_const() if self.data[2] is not None else True)
		return True

class ShiftExp(BaseExpression):
	def __init__(self, add_exp: AddExp, shift_exp_p: ShiftExpP):
		self.add_exp = add_exp
		self.shift_exp_p = shift_exp_p
	
	def is_const(self) -> bool:
		return self.add_exp.is_const() and self.shift_exp_p.is_const()

class RelationalOp:
	def __init__(self, op: Literal['<', '>', '<=', '>=']):
		self.op = op

class RelationalExpP(BaseExpression):
	def __init__(self, data: tuple[RelationalOp, ShiftExp, RelationalExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[1].is_const() and (self.data[2].is_const() if self.data[2] is not None else True)
		return True

class RelationalExp(BaseExpression):
	def __init__(self, shift_exp: ShiftExp, relational_exp_p: RelationalExpP):
		self.shift_exp = shift_exp
		self.relational_exp_p = relational_exp_p
	
	def is_const(self) -> bool:
		return self.shift_exp.is_const() and self.relational_exp_p.is_const()

class EqualityOp:
	def __init__(self, op: Literal['==', '!=']):
		self.op = op

class EqualityExpP(BaseExpression):
	def __init__(self, data: tuple[EqualityOp, RelationalExp, EqualityExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[1].is_const() and (self.data[2].is_const() if self.data[2] is not None else True)
		return True

class EqualityExp(BaseExpression):
	def __init__(self, relational_exp: RelationalExp, equality_exp_p: EqualityExpP):
		self.relational_exp = relational_exp
		self.equality_exp_p = equality_exp_p
	
	def is_const(self) -> bool:
		return self.relational_exp.is_const() and self.equality_exp_p.is_const()

class AndExpP:
	def __init__(self, data: tuple[EqualityExp, AndExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True

class AndExp:
	def __init__(self, equality_exp: EqualityExp, and_exp_p: AndExpP):
		self.equality_exp = equality_exp
		self.and_exp_p = and_exp_p
	
	def is_const(self) -> bool:
		return self.equality_exp.is_const() and self.and_exp_p.is_const()

class ExclusiveOrExpP:
	def __init__(self, data: tuple[AndExp, ExclusiveOrExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True

class ExclusiveOrExp:
	def __init__(self, and_exp: AndExp, exclusive_or_exp_p: ExclusiveOrExpP):
		self.and_exp = and_exp
		self.exclusive_or_exp_p = exclusive_or_exp_p
	
	def is_const(self) -> bool:
		return self.and_exp.is_const() and self.exclusive_or_exp_p.is_const()

class InclusiveOrExpP:
	def __init__(self, data: tuple[ExclusiveOrExp, InclusiveOrExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True

class InclusiveOrExp:
	def __init__(self, exclusive_or_exp: ExclusiveOrExp, inclusive_or_exp_p: InclusiveOrExpP):
		self.exclusive_or_exp = exclusive_or_exp
		self.inclusive_or_exp_p = inclusive_or_exp_p
	
	def is_const(self) -> bool:
		return self.exclusive_or_exp.is_const() and self.inclusive_or_exp_p.is_const()

class LogicalAndExpP:
	def __init__(self, data: tuple[InclusiveOrExp, LogicalAndExpP | None] | None):
		self.data = data
  
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True

class LogicalAndExp:
	def __init__(self, inclusive_or_exp: InclusiveOrExp, logical_and_exp_p: LogicalAndExpP):
		self.inclusive_or_exp = inclusive_or_exp
		self.logical_and_exp_p = logical_and_exp_p
	
	def is_const(self) -> bool:
		return self.inclusive_or_exp.is_const() and self.logical_and_exp_p.is_const()

class LogicalOrExpP:
	def __init__(self, data: tuple[LogicalAndExp, LogicalOrExpP | None] | None):
		self.data = data
  
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True

class LogicalOrExp:
	def __init__(self, logical_and_exp: LogicalAndExp, logical_or_exp_p: LogicalOrExpP):
		self.logical_and_exp = logical_and_exp
		self.logical_or_exp_p = logical_or_exp_p
	
	def is_const(self) -> bool:
		return self.logical_and_exp.is_const() and self.logical_or_exp_p.is_const()

class ConditionalExp:
	def __init__(self, subexp: LogicalOrExp | tuple[LogicalOrExp, Exp, ConditionalExp]):
		self.subexp = subexp
	
	def is_const(self) -> bool:
		if isinstance(self.subexp, LogicalOrExp):
			return self.subexp.is_const()
		elif isinstance(self.subexp, tuple):
			return self.subexp[0].is_const() and self.subexp[1].is_const() and self.subexp[2].is_const()
		return True
 
	def dump(self, i) -> str:
		ret = f"{i*' '}ConditionalExp:\n"
		if isinstance(self.subexp, LogicalOrExp):
			pass
			#ret += self.subexp.dump(i+1)
		elif isinstance(self.subexp, tuple) and isinstance(self.subexp[0], LogicalOrExp) and isinstance(self.subexp[1], Exp) and isinstance(self.subexp[2], ConditionalExp):
			pass
		return ret

class AssignmentOp:
	def __init__(self, op: Literal['=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|=']):
		self.op = op
	
	def dump(self, i) -> str:
		return f"{i*' '}AssignmentOp: {repr(self.op)}"

class AssignmentExp:
	def __init__(self, subexp: ConditionalExp | tuple[UnaryExp, AssignmentOp, AssignmentExp]):
		self.subexp = subexp
	
	def is_const(self) -> bool:
		if isinstance(self.subexp, ConditionalExp):
			return self.subexp.is_const()
		elif isinstance(self.subexp, tuple):
			return self.subexp[0].is_const() and self.subexp[2].is_const()
		return True
 
	def dump(self, i) -> str:
		ret = f"{i*' '}AssignmentExp:\n"
		if isinstance(self.subexp, ConditionalExp):
			ret += self.subexp.dump(i+1)
		elif isinstance(self.subexp, tuple) and isinstance(self.subexp[0], UnaryExp) and isinstance(self.subexp[1], AssignmentOp) and isinstance(self.subexp[2], AssignmentExp):
			#ret += self.subexp[0].dump(i+1)
			ret += self.subexp[1].dump(i+1)
			ret += self.subexp[2].dump(i+1)
		return ret

class ExpP:
	def __init__(self, data: tuple[AssignmentExp, ExpP | None] | None):
		self.data = data
	
	def is_const(self) -> bool:
		if isinstance(self.data, tuple):
			return self.data[0].is_const() and (self.data[1].is_const() if self.data[1] is not None else True)
		return True
 
	def dump(self, i) -> str:
		ret = f"{i*' '}ExpP:\n"
		if self.data is not None:
			ret += self.data[0].dump(i+1)
			if self.data[1] is not None:
				ret += self.data[1].dump(i+1)
		return ret

class Exp:
	def __init__(self, assignment_exp: AssignmentExp, exp_p: ExpP):
		self.assignment_exp = assignment_exp
		self.exp_p = exp_p
	
	def is_const(self) -> bool:
		return self.assignment_exp.is_const() and self.exp_p.is_const()
 
	def dump(self, i) -> str:
		ret = f"{i*' '}Exp:\n"
		ret += self.assignment_exp.dump(i+1)
		ret += self.exp_p.dump(i+1)
		return ret

class ReturnStmnt:
	def __init__(self, expression: Exp):
		self.expression = expression
	
	def dump(self, i) -> str:
		ret = f"{i*' '}ReturnStmnt:\n"
		ret += self.expression.dump(i+1)
		return ret

class JumpStmnt:
	def __init__(self, ret_stmnt: ReturnStmnt):
		self.ret_stmnt = ret_stmnt
	
	def dump(self, i) -> str:
		ret = f"{i*' '}JumpStmnt:\n"
		ret += self.ret_stmnt.dump(i+1)
		return ret

class Stmnt:
	def __init__(self, jump_stmnt: JumpStmnt):
		self.jump_stmnt = jump_stmnt
	
	def dump(self, i) -> str:
		ret = f"{i*' '}Stmnt:\n"
		ret += self.jump_stmnt.dump(i+1)
		return ret

class CompoundStmnt:
	def __init__(self, stmnts: list[Stmnt]):
		self.stmnts = stmnts

	def dump(self, i) -> str:
		ret = f"{i*' '}CompoundStmnt:\n"
		for s in self.stmnts:
			ret += s.dump(i+1)
		return ret

class FuncDef:
	def __init__(self, ret_type: str, name: str, compound_statement: CompoundStmnt):
		self.ret_type = ret_type
		self.name = name
		self.compound_statement = compound_statement
	
	def dump(self, i) -> str:
		ret = f"{i*' '}FuncDef:\n"
		ret += f"{i*' '} ret_type: {self.ret_type}\n"
		ret += f"{i*' '} name: {self.name}\n"
		ret += self.compound_statement.dump(i+1)
		return ret

class TransUnit:
	def __init__(self, func_defs: list[FuncDef]):
		self.func_defs = func_defs
	
	def dump(self) -> str:
		ret = "TransUnit:\n"
		for fd in self.func_defs:
			ret += fd.dump(1)
		return ret

class ParserResult[NodeType]:
	def __init__(self, result: bool=False, node: Optional[NodeType]=None, token: int=0):
		self.result = result
		self.node = node
		self.token = token
	
	def __bool__(self):
		return self.result

class Parser:
	def __init__(self):
		self.ast: TransUnit | None = None
		self.tokens: list[CCCToken] | None = None
		self.crnt_tk: int = 0
	
	def peek(self, offset=0) -> CCCToken:
		if self.tokens == None:
			return None
		try:
			return self.tokens[self.crnt_tk + offset]
		except IndexError:
			return EOF
	
	def parse(self, tokens: list[CCCToken]):
		GlobalErrorLog.clear()
		#print("parse")
		self.tokens = tokens
		
		ast = self.parse_trans_unit()
		if ast.result:
			self.ast = ast.node
		else:
			GlobalErrorLog.error("No Translation Unit")
		return self.ast
	
	def parse_trans_unit(self) -> ParserResult[TransUnit]:
		#print("parse_trans_unit")
		fdef = self.parse_func_def(0)
		if fdef:
			if isinstance(fdef.node, FuncDef):
				tmp = TransUnit([fdef.node])
				GlobalErrorLog.clear()
				return ParserResult[TransUnit](True, tmp)
		GlobalErrorLog.error("Unable to get FuncDef")
		return ParserResult[TransUnit]()

	def parse_func_def(self, cur_tk: int) -> ParserResult[FuncDef]:
		#print("parse_parse_func_def")
		rtype = self.peek(cur_tk)
		ident = self.peek(cur_tk+1)
		lparen = self.peek(cur_tk+2)
		rparen = self.peek(cur_tk+3)
		if (not isinstance(rtype, KwTok)) or \
		   (not isinstance(ident, IdentTok)) or \
	  	   (lparen != '(') or \
		   (rparen != ')'):
			GlobalErrorLog.error("Unable to parse Function Definition")
			return ParserResult[FuncDef](token=cur_tk)
		
		cmpnd_stmnt = self.parse_compound_statement(cur_tk+4)
		if cmpnd_stmnt.result:
			if cmpnd_stmnt.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[FuncDef](True, FuncDef(rtype.value, ident.value, cmpnd_stmnt.node), cmpnd_stmnt.token)
		return ParserResult[FuncDef](token=cur_tk)
	
	def parse_compound_statement(self, cur_tk: int) -> ParserResult[CompoundStmnt]:
		#print("parse_compound_statement")
		old_cur_tk = cur_tk
		lcur_tk = cur_tk
		lbrace = self.peek(lcur_tk)
		lcur_tk += 1

		if lbrace != "{":
			GlobalErrorLog.error("Unable to get '{'")
			return ParserResult[CompoundStmnt](token=old_cur_tk)
		
		stmnts: list[Stmnt] = []
		while self.peek(lcur_tk) != "}":
			stmnt = self.parse_statement(lcur_tk)
			if lcur_tk == stmnt.token:
				GlobalErrorLog.error("Made no progress")
				return ParserResult[CompoundStmnt](token=old_cur_tk)
			if stmnt.result:
				if stmnt.node is not None:
					stmnts.append(stmnt.node)
					lcur_tk = stmnt.token
		
		GlobalErrorLog.clear()
		return ParserResult[CompoundStmnt](True, CompoundStmnt(stmnts), lcur_tk+1)

	def parse_statement(self, cur_tk: int) -> ParserResult[Stmnt]:
		old_cur_tk = cur_tk

		jmp_stmnt = self.parse_jump_statement(cur_tk)
		if jmp_stmnt.result:
			if jmp_stmnt.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[Stmnt](True, Stmnt(jmp_stmnt.node), jmp_stmnt.token)
		GlobalErrorLog.error("Unable to Parse Statement")
		return ParserResult[Stmnt](token=old_cur_tk)

	def parse_jump_statement(self, cur_tk: int) -> ParserResult[JumpStmnt]:
		old_cur_tk = cur_tk

		ret_stmnt = self.parse_return_statement(cur_tk)
		if ret_stmnt.result:
			if ret_stmnt.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[JumpStmnt](True, JumpStmnt(ret_stmnt.node), ret_stmnt.token)
		GlobalErrorLog.error("Unable to Parse Return Statement")
		return ParserResult[JumpStmnt](token=old_cur_tk)

	def parse_return_statement(self, cur_tk: int) -> ParserResult[ReturnStmnt]:
		#print("parse_return_statement")
		old_cur_tk = cur_tk
		ret = self.peek(cur_tk)
		cur_tk += 1

		if not isinstance(ret, KwTok):
			GlobalErrorLog.error(f"Unable to get KwTok; got: {repr(ret)}");
			return ParserResult[ReturnStmnt](token=old_cur_tk)
		if ret.value != "return":
			GlobalErrorLog.error(f"Invalid keyword; got: {repr(ret)}");
			return ParserResult[ReturnStmnt](token=old_cur_tk)
		
		exp_t = self.parse_expression(cur_tk)
		if (not exp_t.result) or (exp_t.node is None):
			GlobalErrorLog.error("Unable to parse Expression");
			return ParserResult[ReturnStmnt](token=old_cur_tk)
		
		cur_tk = exp_t.token
		if self.peek(cur_tk) != ';':
			GlobalErrorLog.error(f"Unable to get ';'; got: {repr(self.peek(cur_tk))}")
			return ParserResult[ReturnStmnt](token=old_cur_tk)
		
		GlobalErrorLog.clear()
		return ParserResult[ReturnStmnt](True, ReturnStmnt(exp_t.node), cur_tk+1)
	
	def parse_expression_p(self, cur_tk: int) -> ParserResult[ExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == ','):
			cur_tk += 1
			e = self.parse_assignment_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[ExpP](True, ExpP((e.node, e2.node)), cur_tk)
				else:
					GlobalErrorLog.error("Unable to parse Expression'")
			else:
				GlobalErrorLog.error("Unable to parse Assignment Expression")
		else:
			GlobalErrorLog.error(f"Unable to get ','; got: {repr(tk)}")
		
		GlobalErrorLog.error("Unable to parse Expression'")
		return ParserResult[ExpP](True, ExpP(None), old_cur_tk)

	def parse_expression(self, cur_tk: int) -> ParserResult[Exp]:
		old_cur_tk = cur_tk

		e = self.parse_assignment_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[Exp](True, Exp(e.node, e2.node), cur_tk)
			else:
				GlobalErrorLog.error("Unable to parse Expression'")
		else:
			GlobalErrorLog.error("Unable to parse Assignment Expression")
			
		GlobalErrorLog.error("Unable to parse Expression")
		return ParserResult[Exp](token=old_cur_tk)

	def parse_assignment_expression(self, cur_tk: int) -> ParserResult[AssignmentExp]:
		old_cur_tk = cur_tk

		e = self.parse_conditional_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			GlobalErrorLog.clear()
			return ParserResult[AssignmentExp](True, AssignmentExp(e.node), cur_tk)
		else:
			GlobalErrorLog.error("Unable to parse Conditional Expression")
		
		cur_tk = old_cur_tk
		e = self.parse_unary_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			op = self.parse_assignment_operator(cur_tk)
			if op.result and (op.node is not None):
				cur_tk = op.token
				e2 = self.parse_assignment_expression(cur_tk)
				if e2.result and (e2.node is not None):
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[AssignmentExp](True, AssignmentExp((e.node, op.node, e2.node)), cur_tk)
				else:
					GlobalErrorLog.error("Unable to parse Assignment Expression")
			else:
				GlobalErrorLog.error("Unable to parse Assignment Operator")
		else:
			GlobalErrorLog.error("Unable to parse Unary Expression")
		
		GlobalErrorLog.error("Unable to parse Assignment Expression")
		return ParserResult[AssignmentExp](token=old_cur_tk)
	
	def parse_assignment_operator(self, cur_tk: int) -> ParserResult[AssignmentOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Assignment Operator; got: {repr(tk)}")
			return ParserResult[AssignmentOp](token=old_cur_tk)
		if tk in ['=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|=']:
			tk = cast(Literal['=', '*=', '/=', '%=', '+=', '-=', '<<=', '>>=', '&=', '^=', '|='], tk)
			GlobalErrorLog.clear()
			return ParserResult[AssignmentOp](True, AssignmentOp(tk), cur_tk+1)
		
		GlobalErrorLog.error(f"Unable to parse Assignment Operator; got: {repr(tk)}")
		return ParserResult[AssignmentOp](token=old_cur_tk)

	def parse_conditional_expression(self, cur_tk: int) -> ParserResult[ConditionalExp]:
		old_cur_tk = cur_tk

		e = self.parse_logical_or_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			if (tk := self.peek(cur_tk)) and (tk == '?'):
				cur_tk += 1
				e2 = self.parse_expression(cur_tk)
				if e2.result and (e2.node is not None):
					cur_tk = e2.token
					if (tk2 := self.peek(cur_tk)) and (tk2 == ':'):
						cur_tk += 1
						e3 = self.parse_conditional_expression(cur_tk)
						if e3.result and (e3.node is not None):
							cur_tk = e3.token
							GlobalErrorLog.clear()
							return ParserResult[ConditionalExp](True, ConditionalExp((e.node, e2.node, e3.node)), cur_tk)
						else:
							GlobalErrorLog.error("Unable to parse Conditional Expression")
					else:
						GlobalErrorLog.error(f"Unable to get ':'; got: {repr(tk2)}")
				else:
					GlobalErrorLog.error("Unable to parse Expression")
			else:
				GlobalErrorLog.clear()
				return ParserResult[ConditionalExp](True, ConditionalExp(e.node), cur_tk)
		else:
			GlobalErrorLog.error("Unable to parse Logical Or Expression")
		
		GlobalErrorLog.error("Unable to parse Conditional Expression")
		return ParserResult[ConditionalExp](token=old_cur_tk)

	def parse_logical_or_expression_p(self, cur_tk: int) -> ParserResult[LogicalOrExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == '||'):
			cur_tk += 1
			e = self.parse_logical_and_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_logical_or_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[LogicalOrExpP](True, LogicalOrExpP((e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Logical Or Expression'")
		return ParserResult[LogicalOrExpP](True, LogicalOrExpP(None), old_cur_tk)

	def parse_logical_or_expression(self, cur_tk: int) -> ParserResult[LogicalOrExp]:
		old_cur_tk = cur_tk

		e = self.parse_logical_and_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_logical_or_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[LogicalOrExp](True, LogicalOrExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Logical Or Expression")
		return ParserResult[LogicalOrExp](token=old_cur_tk)

	def parse_logical_and_expression_p(self, cur_tk: int) -> ParserResult[LogicalAndExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == '&&'):
			cur_tk += 1
			e = self.parse_inclusive_or_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_logical_and_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[LogicalAndExpP](True, LogicalAndExpP((e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Logical And Expression'")
		return ParserResult[LogicalAndExpP](True, LogicalAndExpP(None), old_cur_tk)

	def parse_logical_and_expression(self, cur_tk: int) -> ParserResult[LogicalAndExp]:
		old_cur_tk = cur_tk

		e = self.parse_inclusive_or_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_logical_and_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[LogicalAndExp](True, LogicalAndExp(e.node, e2.node), cur_tk)
			
		GlobalErrorLog.error("Unable to parser Logical And Expression")
		return ParserResult[LogicalAndExp](token=old_cur_tk)

	def parse_inclusive_or_expression_p(self, cur_tk: int) -> ParserResult[InclusiveOrExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == '|'):
			cur_tk += 1
			e = self.parse_exclusive_or_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_inclusive_or_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[InclusiveOrExpP](True, InclusiveOrExpP((e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Inclusive Or Expression'")
		return ParserResult[InclusiveOrExpP](True, InclusiveOrExpP(None), old_cur_tk)

	def parse_inclusive_or_expression(self, cur_tk: int) -> ParserResult[InclusiveOrExp]:
		old_cur_tk = cur_tk

		e = self.parse_exclusive_or_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_inclusive_or_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[InclusiveOrExp](True, InclusiveOrExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Inclusive Or Expression")
		return ParserResult[InclusiveOrExp](token=old_cur_tk)
	
	def parse_exclusive_or_expression_p(self, cur_tk: int) -> ParserResult[ExclusiveOrExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == '^'):
			cur_tk += 1
			e = self.parse_and_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_exclusive_or_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[ExclusiveOrExpP](True, ExclusiveOrExpP((e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to Parse Exclusive Or Expression'")
		return ParserResult[ExclusiveOrExpP](True, ExclusiveOrExpP(None), old_cur_tk)

	def parse_exclusive_or_expression(self, cur_tk: int) -> ParserResult[ExclusiveOrExp]:
		old_cur_tk = cur_tk

		e = self.parse_and_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_exclusive_or_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[ExclusiveOrExp](True, ExclusiveOrExp(e.node, e2.node), cur_tk)

		GlobalErrorLog.error("Unable to Parse Exclusive Or Expression")
		return ParserResult[ExclusiveOrExp](token=old_cur_tk)

	def parse_and_expression_p(self, cur_tk: int) -> ParserResult[AndExpP]:
		old_cur_tk = cur_tk

		if (tk := self.peek(cur_tk)) and (tk == '&'):
			cur_tk += 1
			e = self.parse_equality_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_and_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[AndExpP](True, AndExpP((e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to Parse And Expression'")
		return ParserResult[AndExpP](True, AndExpP(None), old_cur_tk)

	def parse_and_expression(self, cur_tk: int) -> ParserResult[AndExp]:
		old_cur_tk = cur_tk

		e = self.parse_equality_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_and_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[AndExp](True, AndExp(e.node, e2.node), cur_tk)
			
		GlobalErrorLog.error("Unable to Parse And Expression")
		return ParserResult[AndExp](token=old_cur_tk)

	def parse_equality_expression_p(self, cur_tk: int) -> ParserResult[EqualityExpP]:
		old_cur_tk = cur_tk

		tk = self.parse_equality_operator(cur_tk)
		if tk.result and (tk.node is not None):
			cur_tk = tk.token
			e = self.parse_relational_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_equality_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[EqualityExpP](True, EqualityExpP((tk.node, e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Equality Expression'")
		return ParserResult[EqualityExpP](True, EqualityExpP(None), old_cur_tk)

	def parse_equality_expression(self, cur_tk: int) -> ParserResult[EqualityExp]:
		old_cur_tk = cur_tk

		e = self.parse_relational_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_equality_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[EqualityExp](True, EqualityExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Equality Expression")
		return ParserResult[EqualityExp](token=old_cur_tk)

	def parse_equality_operator(self, cur_tk: int) -> ParserResult[EqualityOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			return ParserResult[EqualityOp](token=old_cur_tk)
		if tk in ['==', '!=']:
			tk = cast(Literal['==', '!='], tk)
			GlobalErrorLog.clear()
			return ParserResult[EqualityOp](True, EqualityOp(tk), cur_tk+1)
		
		GlobalErrorLog.error("Unable to parse Equality Operator")
		return ParserResult[EqualityOp](token=old_cur_tk)

	def parse_relational_expression_p(self, cur_tk: int) -> ParserResult[RelationalExpP]:
		old_cur_tk = cur_tk

		tk = self.parse_relational_operator(cur_tk)
		if tk.result and (tk.node is not None):
			cur_tk = tk.token
			e = self.parse_shift_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_relational_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[RelationalExpP](True, RelationalExpP((tk.node, e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Relational Expression'")
		return ParserResult[RelationalExpP](True, RelationalExpP(None), old_cur_tk)

	def parse_relational_expression(self, cur_tk: int) -> ParserResult[RelationalExp]:
		old_cur_tk = cur_tk

		e = self.parse_shift_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_relational_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[RelationalExp](True, RelationalExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Relational Expression")
		return ParserResult[RelationalExp](token=old_cur_tk)

	def parse_relational_operator(self, cur_tk: int) -> ParserResult[RelationalOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get relational operator; got: {repr(tk)}")
			return ParserResult[RelationalOp](token=old_cur_tk)
		if tk in ['<', '>', '<=', '>=']:
			tk = cast(Literal['<', '>', '<=', '>='], tk)
			GlobalErrorLog.clear()
			return ParserResult[RelationalOp](True, RelationalOp(tk), cur_tk+1)
		
		GlobalErrorLog.error(f"Unable to get relational operator; got: {repr(tk)}")
		return ParserResult[RelationalOp](token=old_cur_tk)

	def parse_shift_expression_p(self, cur_tk: int) -> ParserResult[ShiftExpP]:
		old_cur_tk = cur_tk

		tk = self.parse_shift_operator(cur_tk)
		if tk.result and (tk.node is not None):
			cur_tk = tk.token
			e = self.parse_add_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_shift_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[ShiftExpP](True, ShiftExpP((tk.node, e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Shift Expression'")
		return ParserResult[ShiftExpP](True, ShiftExpP(None), old_cur_tk)

	def parse_shift_expression(self, cur_tk: int) -> ParserResult[ShiftExp]:
		old_cur_tk = cur_tk

		e = self.parse_add_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_shift_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[ShiftExp](True, ShiftExp(e.node, e2.node), cur_tk)
			else:
				GlobalErrorLog.error("Unable to parse Shift Expression'")
		else:
			GlobalErrorLog.error("Unable to parse Add Expression")
		
		GlobalErrorLog.error("Unable to parse Shift Expression")
		return ParserResult[ShiftExp](token=old_cur_tk)
	
	def parse_shift_operator(self, cur_tk: int) -> ParserResult[ShiftOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Shift Operator; got: {repr(tk)}")
			return ParserResult[ShiftOp](token=old_cur_tk)
		if tk in ['<<', '>>']:
			tk = cast(Literal['<<', '>>'], tk)
			GlobalErrorLog.clear()
			return ParserResult[ShiftOp](True, ShiftOp(tk), cur_tk+1)
		GlobalErrorLog.error(f"Unable to get Shift Operator; got: {repr(tk)}")
		return ParserResult[ShiftOp](token=old_cur_tk)

	def parse_add_expression_p(self, cur_tk: int) -> ParserResult[AddExpP]:
		old_cur_tk = cur_tk

		tk = self.parse_add_operator(cur_tk)
		if tk.result and (tk.node is not None):
			cur_tk = tk.token
			e = self.parse_mult_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_add_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[AddExpP](True, AddExpP((tk.node, e.node, e2.node)), cur_tk)
				else:
					GlobalErrorLog.error("Unable to parse Add Expression'")
			else:
				GlobalErrorLog.error("Unable to parse Mult Expression")
		else:
			GlobalErrorLog.error("Unable to parse Add Operator")
		
		GlobalErrorLog.error("Unable to parse Add Expression'")
		return ParserResult[AddExpP](True, AddExpP(None), old_cur_tk)

	def parse_add_expression(self, cur_tk: int) -> ParserResult[AddExp]:
		old_cur_tk = cur_tk

		e = self.parse_mult_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_add_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[AddExp](True, AddExp(e.node, e2.node), cur_tk)
			else:
				GlobalErrorLog.error("Unable to parse Add Expression'")
		else:
			GlobalErrorLog.error("Unable to parse Mult Expression")
			
		GlobalErrorLog.error("Unable to parse Add Expression")
		return ParserResult[AddExp](token=old_cur_tk)

	def parse_add_operator(self, cur_tk: int) -> ParserResult[AddOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Add Operator; got: {repr(tk)}")
			return ParserResult[AddOp](token=old_cur_tk)
		if tk in ['+', '-']:
			tk = cast(Literal['+', '-'], tk)
			GlobalErrorLog.clear()
			return ParserResult[AddOp](True, AddOp(tk), cur_tk+1)
		GlobalErrorLog.error(f"Unable to get Add Operator; got: {repr(tk)}")
		return ParserResult[AddOp](token=old_cur_tk)

	def parse_mult_expression_p(self, cur_tk: int) -> ParserResult[MultExpP]:
		old_cur_tk = cur_tk

		tk = self.parse_mult_operator(cur_tk)
		if tk.result and (tk.node is not None):
			cur_tk = tk.token
			e = self.parse_cast_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				e2 = self.parse_mult_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[MultExpP](True, MultExpP((tk.node, e.node, e2.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Mult Expression'")
		return ParserResult[MultExpP](True, MultExpP(None), old_cur_tk)

	def parse_mult_expression(self, cur_tk: int) -> ParserResult[MultExp]:
		old_cur_tk = cur_tk

		e = self.parse_cast_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_mult_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[MultExp](True, MultExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Mult Expression")
		return ParserResult[MultExp](token=old_cur_tk)
		
	def parse_mult_operator(self, cur_tk: int) -> ParserResult[MultOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Mult Operator; got: {repr(tk)}")
			return ParserResult[MultOp](token=old_cur_tk)
		if tk in ['*', '/', '%']:
			tk = cast(Literal['*', '/', '%'], tk)
			GlobalErrorLog.clear()
			return ParserResult[MultOp](True, MultOp(tk), cur_tk+1)
		GlobalErrorLog.error(f"Unable to get Mult Operator; got: {repr(tk)}")
		return ParserResult[MultOp](token=old_cur_tk)

	def parse_cast_expression(self, cur_tk: int) -> ParserResult[CastExp]:
		old_cur_tk = cur_tk

		uexp = self.parse_unary_expression(cur_tk)
		if uexp.result:
			if uexp.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[CastExp](True, CastExp(uexp.node), uexp.token)
			else:
				GlobalErrorLog.error("Unable to parse Unary Expression")
				return ParserResult[CastExp](token=old_cur_tk)
		
		if self.peek(cur_tk) != '(':
			GlobalErrorLog.error(f"Unable to get '('; got: {repr(self.peek(cur_tk))}")
			return ParserResult[CastExp](token=old_cur_tk)
		cur_tk += 1

		t = self.peek(cur_tk)
		if not isinstance(t, KwTok):
			GlobalErrorLog.error(f"Unable to get Keyword; got: {repr(t)}")
			return ParserResult[CastExp](token=old_cur_tk)
		if t.value != 'int':
			GlobalErrorLog.error(f"Unable to get `int` Keyword; got: {repr(t)}")
			return ParserResult[CastExp](token=old_cur_tk)
		cur_tk += 1

		if self.peek(cur_tk) != ')':
			GlobalErrorLog.error(f"Unable to get ')'; got: {repr(self.peek(cur_tk))}")
			return ParserResult[CastExp](token=old_cur_tk)
		cur_tk += 1

		cexp = self.parse_cast_expression(cur_tk)
		if cexp.result:
			if cexp.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[CastExp](True, CastExp((TypeName(), cexp.node)), cexp.token)
		
		GlobalErrorLog.error("Unable to parse Cast Expression")
		return ParserResult[CastExp](token=old_cur_tk)
	
	def parse_unary_expression(self, cur_tk: int) -> ParserResult[UnaryExp]:
		old_cur_tk = cur_tk

		pfexp = self.parse_postfix_expression(cur_tk)
		if pfexp.result:
			if pfexp.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[UnaryExp](True, UnaryExp(pfexp.node), pfexp.token)
			else:
				GlobalErrorLog.error("Unable to parse Postfix Expression")
				return ParserResult[UnaryExp](token=old_cur_tk)
		
		if self.peek(cur_tk) in ['++', '--']:
			uexp = self.parse_unary_expression(cur_tk+1)
			if uexp.result and (uexp.node is not None):
				op = PrePostOp(cast(Literal['++', '--'], self.peek(cur_tk)))
				GlobalErrorLog.clear()
				return ParserResult[UnaryExp](True, UnaryExp((op, uexp.node)), uexp.token)
			else:
				GlobalErrorLog.error("Unable to parse Unary Expression")
				return ParserResult[UnaryExp](token=old_cur_tk)
		elif isinstance(self.peek(cur_tk), KwTok) and (cast(KwTok, self.peek(cur_tk)).value == "sizeof"):
			uexp = self.parse_unary_expression(cur_tk+1)
			op = SizeofOp()
			if uexp.result and (uexp.node is not None):
				GlobalErrorLog.clear()
				return ParserResult[UnaryExp](True, UnaryExp((op, uexp.node)), uexp.token)
			
			t = self.peek(cur_tk+1)
			if not isinstance(t, KwTok):
				GlobalErrorLog.error(f"Unable to get Keyword; got: {repr(t)}")
				return ParserResult[UnaryExp](token=old_cur_tk)
			if t.value != 'int':
				GlobalErrorLog.error(f"Unable to parse `int` Keyword; got: {repr(t)}")
				return ParserResult[UnaryExp](token=old_cur_tk)
			GlobalErrorLog.clear()
			return ParserResult[UnaryExp](True, UnaryExp((op, TypeName())), cur_tk+1)
		
		uop = self.parse_unary_operator(cur_tk)
		if (not uop.result) or (uop.node is None):
			GlobalErrorLog.error("Unable to parse Unary Operator")
			return ParserResult[UnaryExp](token=old_cur_tk)
		cur_tk = uop.token
		
		cexp = self.parse_cast_expression(cur_tk)
		if cexp.result:
			if cexp.node is not None:
				GlobalErrorLog.clear()
				return ParserResult[UnaryExp](True, UnaryExp((uop.node, cexp.node)), cexp.token)
			
		GlobalErrorLog.error("Unable to parse Unary Expression")
		return ParserResult[UnaryExp](token=old_cur_tk)

	def parse_unary_operator(self, cur_tk: int) -> ParserResult[UnaryOp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Unary Operator; got: {repr(tk)}")
			return ParserResult[UnaryOp](token=old_cur_tk)
		if tk in ['&', '*', '+', '-', '~', '!']:
			tk = cast(Literal['&', '*', '+', '-', '~', '!'], tk)
			GlobalErrorLog.clear()
			return ParserResult[UnaryOp](True, UnaryOp(tk), cur_tk+1)
		GlobalErrorLog.error(f"Unable to get Unary Operator; got: {repr(tk)}")
		return ParserResult[UnaryOp](token=old_cur_tk)

	def parse_postfix_expression_p(self, cur_tk: int) -> ParserResult[PostfixExpP]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		cur_tk += 1
		if tk == '[':
			e = self.parse_expression(cur_tk)
			if e.result and (e.node is not None):
				cur_tk = e.token
				if (tk2 := self.peek(cur_tk)) and (tk2 == ']'):
					cur_tk += 1
					e2 = self.parse_postfix_expression_p(cur_tk)
					if e2.result:
						cur_tk = e2.token
						GlobalErrorLog.clear()
						return ParserResult[PostfixExpP](True, PostfixExpP((e.node, e2.node)), cur_tk)
		elif tk == '(':
			es: list[AssignmentExp] = []
			e = self.parse_assignment_expression(cur_tk)
			while e.result and (e.node is not None):
				cur_tk = e.token
				es.append(e.node)
			if (tk2 := self.peek(cur_tk)) and (tk2 == ')'):
				cur_tk += 1
				e2 = self.parse_postfix_expression_p(cur_tk)
				if e2.result:
					cur_tk = e2.token
					GlobalErrorLog.clear()
					return ParserResult[PostfixExpP](True, PostfixExpP((es, e2.node)), cur_tk)
		elif tk == '.':
			if (tk2 := self.peek(cur_tk)) and (isinstance(tk2, IdentTok)):
				cur_tk += 1
				e = self.parse_postfix_expression_p(cur_tk)
				if e.result:
					cur_tk = e.token
					GlobalErrorLog.clear()
					return ParserResult[PostfixExpP](True, PostfixExpP((MemberAccess(tk2.value), e.node)), cur_tk)
		elif tk == '->':
			if (tk2 := self.peek(cur_tk)) and (isinstance(tk2, IdentTok)):
				cur_tk += 1
				e = self.parse_postfix_expression_p(cur_tk)
				if e.result:
					cur_tk = e.token
					GlobalErrorLog.clear()
					return ParserResult[PostfixExpP](True, PostfixExpP((PointerMemberAccess(tk2.value), e.node)), cur_tk)
		elif tk in ['++', '--']:
			e = self.parse_postfix_expression_p(cur_tk)
			if e.result:
				cur_tk = e.token
				GlobalErrorLog.clear()
				return ParserResult[PostfixExpP](True, PostfixExpP((PrePostOp(cast(Literal['++', '--'], tk)), e.node)), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Postfix Expression'")
		return ParserResult[PostfixExpP](True, PostfixExpP(None), old_cur_tk)

	def parse_postfix_expression(self, cur_tk: int) -> ParserResult[PostfixExp]:
		old_cur_tk = cur_tk

		e = self.parse_primary_expression(cur_tk)
		if e.result and (e.node is not None):
			cur_tk = e.token
			e2 = self.parse_postfix_expression_p(cur_tk)
			if e2.result and (e2.node is not None):
				cur_tk = e2.token
				GlobalErrorLog.clear()
				return ParserResult[PostfixExp](True, PostfixExp(e.node, e2.node), cur_tk)
		
		GlobalErrorLog.error("Unable to parse Postfix Expression")
		return ParserResult[PostfixExp](token=old_cur_tk)

	def parse_primary_expression(self, cur_tk: int) -> ParserResult[PrimExp]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if not tk:
			GlobalErrorLog.error(f"Unable to get Token; got: {repr(tk)}")
			return ParserResult[PrimExp](token=old_cur_tk)
		
		if isinstance(tk, IdentTok):
			GlobalErrorLog.clear()
			return ParserResult[PrimExp](True, PrimExp(tk.value), cur_tk+1)
		elif tk == '(':
			cur_tk += 1
			e = self.parse_expression(cur_tk)
			if (not e.result) or (e.node is None):
				GlobalErrorLog.error("Unable to parse Expression")
				return ParserResult[PrimExp](token=old_cur_tk)
			cur_tk = e.token
			tk = self.peek(cur_tk)
			if (not tk) or (tk != ')'):
				GlobalErrorLog.error(f"Unable to get ')'; got: {repr(tk)}")
				return ParserResult[PrimExp](token=old_cur_tk)
			GlobalErrorLog.clear()
			return ParserResult[PrimExp](True, PrimExp(e.node), cur_tk+1)
		
		e = self.parse_constant(cur_tk)
		if (not e.result) or (e.node is None):
			GlobalErrorLog.error("Unable to parse Constant")
			return ParserResult[PrimExp](token=old_cur_tk)
		GlobalErrorLog.clear()
		return ParserResult[PrimExp](True, PrimExp(e.node), e.token)

	def parse_constant(self, cur_tk: int) -> ParserResult[Const]:
		old_cur_tk = cur_tk

		e = self.parse_int_constant(cur_tk)
		if (not e.result) or (e.node is None):
			GlobalErrorLog.error("Unable to parse Int Constant")
			return ParserResult[Const](token=old_cur_tk)
		GlobalErrorLog.clear()
		return ParserResult[Const](True, Const(e.node), e.token)

	def parse_int_constant(self, cur_tk: int) -> ParserResult[ConstInt]:
		old_cur_tk = cur_tk

		tk = self.peek(cur_tk)
		if (not tk) or (not isinstance(tk, IntegerTok)):
			GlobalErrorLog.error(f"Unable to get Integer; got: {repr(tk)}")
			return ParserResult[ConstInt](token=old_cur_tk)
		GlobalErrorLog.clear()
		return ParserResult[ConstInt](True, ConstInt(tk.value), cur_tk+1)

