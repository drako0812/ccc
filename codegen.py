from cparser import *
from regalloc import *
import uuid

class CodeGenerator:
	def __init__(self):
		self.ralloc = RegisterAllocator()
		self.ast: TransUnit | None = None
		self.asm: str = ""
	
	def generate_label(self) -> str:
		id = uuid.uuid4()
		return f"_{id.bytes.hex()}"

	def generate(self, ast: TransUnit) -> str:
		self.ralloc.reset()
		self.ast = ast
		self.asm = ""

		for fd in self.ast.func_defs:
			self.asm += self.generate_func_def(fd)
		
		return self.asm

	def generate_func_def(self, fd: FuncDef) -> str:
		asm = f"_{fd.name}:\n"
		asm += "  sto .ret, [rSP]\n"
		asm += "  pop\n"
		
		if isinstance(fd.compound_statement, CompoundStmnt):
			asm += self.generate_compound_statement(fd.compound_statement)
		else:
			print("E: Invalid Statement")
			exit(1)

		asm += ".ret: #d16 0x0000\n"
		return asm

	def generate_compound_statement(self, fd: CompoundStmnt) -> str:
		asm = ""

		for stmnt in fd.stmnts:
			asm += self.generate_statement(stmnt)

		return asm

	def generate_statement(self, stmnt: Stmnt) -> str:
		return self.generate_jump_statement(stmnt.jump_stmnt)
	
	def generate_jump_statement(self, js: JumpStmnt) -> str:
		return self.generate_return_statement(js.ret_stmnt)

	def generate_return_statement(self, stmnt: ReturnStmnt) -> str:
		asm = ""
		
		
		if stmnt.expression is None:
			print("generate_return_statement: E - Return Statement Expression is None")
			exit(1)
		if stmnt.expression.is_const():
			asm += f"  push {stmnt.expression.calculate()}"
		else:
			reg = self.ralloc.allocate()
			asm += self.generate_expression(stmnt.expression, reg)
			asm += f"  push [{reg}]\n"
			self.ralloc.release(reg)

		asm += "  push [.ret]\n"
		asm += "  ret\n"
		return asm
	
	def generate_expression(self, exp: Exp, reg) -> str:
		asm = ""
		asm += self.generate_assignment_expression(exp.assignment_exp, reg)
		asm += self.generate_expression_p(exp.exp_p, reg)
		return asm

	def generate_expression_p(self, exp: ExpP, reg) -> str:
		if exp.data is None:
			return ""
		if not isinstance(exp.data, tuple):
			print("generate_expression_p: E - exp.data is not a tuple")
			exit(1)
		if exp.data[1] is None:
			print("generate_expression_p: E - Expression' is None")
			exit(1)
		asm = ""
		asm += self.generate_assignment_expression(exp.data[0], reg)
		asm += self.generate_expression_p(exp.data[1], reg)
		return asm

	def generate_assignment_expression(self, exp: AssignmentExp, reg) -> str:
		if isinstance(exp.subexp, ConditionalExp):
			return self.generate_conditional_expression(exp.subexp, reg)
		elif isinstance(exp.subexp, tuple):
			asm = ""
			asm += self.generate_unary_expression(exp.subexp[0], reg)
			op = exp.subexp[1]
			regb = self.ralloc.allocate()
			asm += self.generate_assignment_expression(exp.subexp[2], regb)
			if op.op == '=':
				asm += f"  sto {reg}, [{regb}]\n"
			elif op.op == '*=':
				asm += f"  mls _N, {reg}, {regb}\n"
			elif op.op == '/=':
				asm += f"  dvs {reg}, {regb}\n"
			elif op.op == '%=':
				asm += f"  mds {reg}, {regb}\n"
			elif op.op == '+=':
				asm += f"  add {reg}, {regb}\n"
			elif op.op == '-=':
				asm += f"  sub {reg}, {regb}\n"
			elif op.op == '<<=':
				asm += f"  shl {reg}, {regb}\n"
			elif op.op == '>>=':
				asm += f"  asr {reg}, {regb}\n"
			elif op.op == '&=':
				asm += f"  and {reg}, {regb}\n"
			elif op.op == '^=':
				asm += f"  xor {reg}, {regb}\n"
			elif op.op == '|=':
				asm += f"  bor {reg}, {regb}\n"
			else:
				print("generate_assignment_expression: E - Invalid assignment operator")
				exit(1)
			self.ralloc.release(regb)
			return asm
		return ""
	
	def generate_conditional_expression(self, exp: ConditionalExp, reg) -> str:
		if isinstance(exp.subexp, LogicalOrExp):
			return self.generate_logical_or_expression(exp.subexp, reg)
		elif isinstance(exp.subexp, tuple) and (len(exp.subexp) == 3):
			asm = ""
			regb = self.ralloc.allocate()
			asm += self.generate_logical_or_expression(exp.subexp[0], regb)
			asm += f"  cmp {regb}, 0\n"
			lbl = ".conditional_false" + self.generate_label()
			asm += f"  jpc EQ {lbl}\n"
			asm += self.generate_expression(exp.subexp[1], reg)
			lbl2 = ".conditional_end" + self.generate_label()
			asm += f"  jmp {lbl2}\n"
			asm += f"{lbl}:\n"
			asm += self.generate_conditional_expression(exp.subexp[2], reg)
			asm += f"{lbl2}:\n"
			self.ralloc.release(regb)
			return asm
		print("generate_conditional_expression: E - Subexpression is not valid")
		exit(1)
		return ""
	
	def generate_logical_or_expression(self, exp: LogicalOrExp, reg) -> str:
		asm = ""
		asm += self.generate_logical_and_expression(exp.logical_and_exp, reg)
		asm += self.generate_logical_or_expression_p(exp.logical_or_exp_p, reg)
		return asm
	
	def generate_logical_or_expression_p(self, exp: LogicalOrExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_logical_and_expression(exp.data[0], regb)
			asm += f"  bor {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_logical_or_expression_p(cast(LogicalOrExpP, exp.data[1]), reg)
			return asm
		else:
			return ""

	def generate_logical_and_expression(self, exp: LogicalAndExp, reg) -> str:
		asm = ""
		asm += self.generate_inclusive_or_expression(exp.inclusive_or_exp, reg)
		asm += self.generate_logical_and_expression_p(exp.logical_and_exp_p, reg)
		return asm

	def generate_logical_and_expression_p(self, exp: LogicalAndExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_inclusive_or_expression(exp.data[0], regb)
			asm += f"  and {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_logical_and_expression_p(cast(LogicalAndExpP, exp.data[1]), reg)
			return asm
		else:
			return ""

	def generate_inclusive_or_expression(self, exp: InclusiveOrExp, reg) -> str:
		asm = ""
		asm += self.generate_exclusive_or_expression(exp.exclusive_or_exp, reg)
		asm += self.generate_inclusive_or_expression_p(exp.inclusive_or_exp_p, reg)
		return asm

	def generate_inclusive_or_expression_p(self, exp: InclusiveOrExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_exclusive_or_expression(exp.data[0], regb)
			asm += f"  bor {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_inclusive_or_expression_p(cast(InclusiveOrExpP, exp.data[1]), reg)
			return asm
		else:
			return ""

	def generate_exclusive_or_expression(self, exp: ExclusiveOrExp, reg) -> str:
		asm = ""
		asm += self.generate_and_expression(exp.and_exp, reg)
		asm += self.generate_exclusive_or_expression_p(exp.exclusive_or_exp_p, reg)
		return asm

	def generate_exclusive_or_expression_p(self, exp: ExclusiveOrExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_and_expression(exp.data[0], regb)
			asm += f"  xor {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_exclusive_or_expression_p(cast(ExclusiveOrExpP, exp.data[1]), reg)
			return asm
		else:
			return ""
	
	def generate_and_expression(self, exp: AndExp, reg) -> str:
		asm = ""
		asm += self.generate_equality_expression(exp.equality_exp, reg)
		asm += self.generate_and_expression_p(exp.and_exp_p, reg)
		return asm

	def generate_and_expression_p(self, exp: AndExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_equality_expression(exp.data[0], regb)
			asm += f"  and {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_and_expression_p(cast(AndExpP, exp.data[1]), reg)
			return asm
		else:
			return ""

	def generate_equality_expression(self, exp: EqualityExp, reg) -> str:
		asm = ""
		asm += self.generate_relational_expression(exp.relational_exp, reg)
		asm += self.generate_equality_expression_p(exp.equality_exp_p, reg)
		return asm

	def generate_equality_expression_p(self, exp: EqualityExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_relational_expression(exp.data[1], regb)
			asm += f"  cmp {reg}, [{regb}]\n"
			lbl = f".if_false_{self.generate_label()}"
			lbl2 = f".end_if_{self.generate_label()}"
			if exp.data[0].op == '==':
				asm += f"  jpc NE, {lbl}\n"
			elif exp.data[0].op == '!=':
				asm += f"  jpc EQ, {lbl}\n"
			asm += f"  sto {reg}, 1\n"
			asm += f"  jmp {lbl2}\n"
			asm += f"{lbl}:\n"
			asm += f"  sto {reg}, 0\n"
			asm += f"{lbl2}:\n"
			self.ralloc.release(regb)
			asm += self.generate_equality_expression_p(cast(EqualityExpP, exp.data[2]), reg)
			return asm
		else:
			return ""
		
	def generate_relational_expression(self, exp: RelationalExp, reg) -> str:
		asm = ""
		asm += self.generate_shift_expression(exp.shift_exp, reg)
		asm += self.generate_relational_expression_p(exp.relational_exp_p, reg)
		return asm

	def generate_relational_expression_p(self, exp: RelationalExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_shift_expression(exp.data[1], regb)
			asm += f"  cmp {reg}, [{regb}]\n"
			lbl = f".if_false_{self.generate_label()}"
			lbl2 = f".end_if_{self.generate_label()}"
			if exp.data[0].op == '<':
				asm += f"  jpc GT, {lbl}\n"
				asm += f"  jpc EQ, {lbl}\n"
			elif exp.data[0].op == '>':
				asm += f"  jpc LT, {lbl}\n"
				asm += f"  jpc EQ, {lbl}\n"
			elif exp.data[0].op == '<=':
				asm += f"  jpc GT, {lbl}\n"
			elif exp.data[0].op == '>=':
				asm += f"  jpc LT, {lbl}\n"
			asm += f"  sto {reg}, 1\n"
			asm += f"  jmp {lbl2}\n"
			asm += f"{lbl}:\n"
			asm += f"  sto {reg}, 0\n"
			asm += f"{lbl2}:\n"
			self.ralloc.release(regb)
			asm += self.generate_relational_expression_p(cast(RelationalExpP, exp.data[2]), reg)
			return asm
		else:
			return ""

	def generate_shift_expression(self, exp: ShiftExp, reg) -> str:
		asm = ""
		asm += self.generate_add_expression(exp.add_exp, reg)
		asm += self.generate_shift_expression_p(exp.shift_exp_p, reg)
		return asm
	
	def generate_shift_expression_p(self, exp: ShiftExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_add_expression(exp.data[1], regb)
			if exp.data[0].op == '<<':
				asm += f"  shl {reg}, [{regb}]\n"
			elif exp.data[0].op == '>>':
				asm += f"  asr {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_shift_expression_p(cast(ShiftExpP, exp.data[2]), reg)
			return asm
		else:
			return ""
	
	def generate_add_expression(self, exp: AddExp, reg) -> str:
		asm = ""
		asm += self.generate_mult_expression(exp.mult_exp, reg)
		asm += self.generate_add_expression_p(exp.add_exp_p, reg)
		return asm
	
	def generate_add_expression_p(self, exp: AddExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_mult_expression(exp.data[1], regb)
			if exp.data[0].op == '+':
				asm += f"  add {reg}, [{regb}]\n"
			elif exp.data[0].op == '-':
				asm += f"  sub {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_add_expression_p(cast(AddExpP, exp.data[2]), reg)
			return asm
		else:
			return ""
		
	def generate_mult_expression(self, exp: MultExp, reg) -> str:
		asm = ""
		asm += self.generate_cast_expression(exp.cast_exp, reg)
		asm += self.generate_mult_expression_p(exp.mult_exp_p, reg)
		return asm
	
	def generate_mult_expression_p(self, exp: MultExpP, reg) -> str:
		if isinstance(exp.data, tuple):
			regb = self.ralloc.allocate()
			asm = ""
			asm += self.generate_cast_expression(exp.data[1], regb)
			if exp.data[0].op == '*':
				asm += f"  mls _N, {reg}, [{regb}]\n"
			elif exp.data[0].op == '/':
				asm += f"  dvs {reg}, [{regb}]\n"
			elif exp.data[0].op == '%':
				asm += f"  mds {reg}, [{regb}]\n"
			self.ralloc.release(regb)
			asm += self.generate_mult_expression_p(cast(MultExpP, exp.data[2]), reg)
			return asm
		else:
			return ""
		
	def generate_cast_expression(self, exp: CastExp, reg) -> str:
		asm = ""
		if isinstance(exp.subexp, UnaryExp):
			asm += self.generate_unary_expression(exp.subexp, reg)
		elif isinstance(exp.subexp, tuple):
			tname = exp.subexp[0]
			e = exp.subexp[1]
			asm += self.generate_cast_expression(e, reg)
		return asm

	def generate_unary_expression(self, exp: UnaryExp, reg) -> str:
		asm = ""
		if isinstance(exp.subexp, PostfixExp):
			asm += self.generate_postfix_expression(exp.subexp, reg)
		elif isinstance(exp.subexp, tuple):
			if isinstance(exp.subexp[0], PrePostOp):
				asm += self.generate_unary_expression(cast(UnaryExp, exp.subexp[1]), reg)
				if exp.subexp[0].op == '++':
					asm += f"  inc {reg}\n"
				elif exp.subexp[0].op == '--':
					asm += f"  dec {reg}\n"
			elif isinstance(exp.subexp[0], UnaryOp):
				asm += self.generate_cast_expression(cast(CastExp, exp.subexp[1]), reg)
				op = exp.subexp[0].op
				if op == '&':
					print("generate_unary_expression: E - Address-of operator not yet implemented")
					exit(1)
				elif op == '*':
					asm += f"  sto {reg}, [[{reg}]+0]\n"
				elif op == '+':
					# We don't do anything.
					pass
				elif op == '-':
					# Negate the value
					regb = self.ralloc.allocate()
					asm += f"  sto {regb}, [{reg}]\n"
					asm += f"  sto {reg}, 0\n"
					asm += f"  sub {reg}, [{regb}]\n"
					self.ralloc.release(regb)
				elif op == '~':
					# Bitwise NOT
					asm += f"  not {reg}\n"
				elif op == '!':
					# Logical NOT
					"""
					cmp reg, 0
					jpc NE, .if_not_z
					sto reg, 1
					jmp .if_end
					.if_not_z:
					sto reg, 0
					.if_end:
					"""
					asm += f"  cmp {reg}, 0\n"
					if_not_z = ".if_not_z_" + self.generate_label()
					asm += f"  jpc NE, {if_not_z}\n"
					asm += f"  sto {reg}, 1\n"
					if_end = ".if_end_" + self.generate_label()
					asm += f"  jmp, {if_end}\n"
					asm += f"{if_not_z}:\n"
					asm += f"  sto {reg}, 0\n"
					asm += f"{if_end}:\n"
			elif isinstance(exp.subexp[0], SizeofOp):
				# Right now everything is `int` so everything is 2 bytes
				asm += f"sto {reg}, 2\n"
		return asm

	def generate_postfix_expression(self, exp: PostfixExp, reg) -> str:
		asm = ""
		asm += self.generate_primary_expression(exp.prim_exp, reg)
		asm += self.generate_postfix_expression_p(exp.postfix_exp_p, reg)
		return asm
	
	def generate_postfix_expression_p(self, exp: PostfixExpP, reg) -> str:
		asm = ""
		if not isinstance(exp.data, tuple):
			return asm
		se: PostfixExpP | None = None
		if isinstance(exp.data[0], Exp):
			print("generate_postfix_expression_p: E - Array Access not yet supported")
			exit(1)
		elif isinstance(exp.data[0], list):
			for e in exp.data[0]:
				asm += self.generate_assignment_expression(e, reg)
			se = exp.data[1]
		elif isinstance(exp.data[0], MemberAccess):
			print("generate_postfix_expression_p: E - Member Access not yet supported")
			exit(1)
		elif isinstance(exp.data[0], PointerMemberAccess):
			print("generate_postfix_expression_p: E - Pointer Member Access not yet supported")
			exit(1)
		elif isinstance(exp.data[0], PrePostOp):
			op = exp.data[0].op
			if op == '++':
				asm += f"  inc {reg}\n"
			elif op == '--':
				asm += f"  dec {reg}\n"
			se = exp.data[1]
		
		if se is not None:
			asm += self.generate_postfix_expression_p(se, reg)

		return asm

	def generate_primary_expression(self, exp: PrimExp, reg) -> str:
		asm = ""
		
		if isinstance(exp.subexp, str):
			print("generate_primary_expression: E - Variables are not yet supported")
			exit(1)
		elif isinstance(exp.subexp, Const):
			asm += f"  sto {reg}, {exp.subexp.value.value}\n"
		elif isinstance(exp.subexp, Exp):
			asm += self.generate_expression(exp.subexp, reg)
		
		return asm
	
