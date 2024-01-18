class RegisterStatus:
	def __init__(self):
		self.used = False
		self.type: str = 'void'

class RegisterAllocator2:
	def __init__(self):
		self.registers: dict[str, dict[str, RegisterStatus]] = {"int": {
			"r0": RegisterStatus(),
			"r1": RegisterStatus(),
			"r2": RegisterStatus(),
			"r3": RegisterStatus(),
			"r4": RegisterStatus(),
			"r5": RegisterStatus(),
			"r6": RegisterStatus(),
			"r7": RegisterStatus()},
			"float": {
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
			}}
	
	def reset(self):
		self.registers: dict[str, dict[str, RegisterStatus]] = {"int": {
			"r0": RegisterStatus(),
			"r1": RegisterStatus(),
			"r2": RegisterStatus(),
			"r3": RegisterStatus(),
			"r4": RegisterStatus(),
			"r5": RegisterStatus(),
			"r6": RegisterStatus(),
			"r7": RegisterStatus()},
			"float": {
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
				"rf0": RegisterStatus(),
			}}
	
	def allocate(self, type: str = "int") -> str:
		if type in ["int", "char", "short"]:
			for key, value in self.registers['int'].items():
				if not value:
					self.registers['int'][key].used = True
					self.registers['int'][key].type = type
					return key
			raise RuntimeError("Out of registers")
		elif type in ["float", "double"]:
			for key, value in self.registers['float'].items():
				if not value:
					self.registers['float'][key].used = True
					self.registers['float'][key].type = type
					return key
			raise RuntimeError("Out of registers")
		raise RuntimeError("Invalid register type requested")
	
	def release(self, reg: str):
		if reg.startswith('rf'):
			self.registers['float'][reg].used = False
			self.registers['float'][reg].type = 'void'
		else:
			self.registers['int'][reg].used = False
			self.registers['int'][reg].type = 'void'

class RegisterAllocator:
	def __init__(self):
		self.registers = {
			"r0": False,
			"r1": False,
			"r2": False,
			"r3": False,
			"r4": False,
			"r5": False,
			"r6": False,
			"r7": False,
			"r8": False}
	
	def reset(self):
		self.registers = {
			"r0": False,
			"r1": False,
			"r2": False,
			"r3": False,
			"r4": False,
			"r5": False,
			"r6": False,
			"r7": False,
			"r8": False}
	
	def allocate(self) -> str:
		for key, value in self.registers.items():
			if not value:
				self.registers[key] = True
				return key
		raise RuntimeError("Out of registers")
	
	def release(self, reg: str):
		self.registers[reg] = False

