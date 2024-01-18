BASIC_TOKENS = [
	"==", "=", "!=", "!", "++", "+=",
	"--", "-=", "+", "->", "-",
	"<<=", ">>=", "^=", "^",
	"<<", ">>", "<=", ">=", "<", ">",
	"&&", "&=", "&", "||", "|=", "|",
	"{", "}", "(", ")", "[", "]",
	"*=", "*", "/=", "/", "%=", "%",
	"...", ".", "~", ",", ";"]

KW_TOKENS = [
	"int",
	"if",
	"else",
	"switch",
	"case",
	"break",
	"struct",
	"sizeof",
	"return"]

class IntegerTok:
	def __init__(self, value: int):
		self.value = value
	
	def __repr__(self):
		return f"IntegerTok[{self.value}]"

class FloatTok:
	def __init__(self, value: float):
		self.value = value
	
	def __repr__(self):
		return f"FloatTok[{self.value}]"
	
class CharTok:
	def __init__(self, value: int):
		self.value = value
	
	def __repr__(self):
		return f"CharTok[{repr(chr(self.value))}]"

class StringTok:
	def __init__(self, value: str):
		self.value = value
	
	def __repr__(self):
		return f"StringTok[{repr(self.value)}]"

class IdentTok:
	def __init__(self, value: str):
		self.value = value
	
	def __repr__(self):
		return f"IdentTok[{repr(self.value)}]"

class KwTok:
	def __init__(self, value: str):
		self.value = value
	
	def __repr__(self):
		return f"KwTok[{repr(self.value)}]"

class EOFTok:
	def __init__(self):
		pass
		
	def __repr__(self):
		return "EOFTok"

	def __bool__(self):
		return False

EOF = EOFTok()

CCCToken = str | IntegerTok | FloatTok | CharTok | StringTok | IdentTok | KwTok | EOFTok | None
CCCTokenResult = tuple[bool, None | CCCToken, int]

class Lexer:
	def __init__(self):
		self.tokens: list[CCCToken] = []
		self.current_char: int = 0
		self.source: str = ""
	
	def reset(self):
		self.tokens = []
		self.current_char = 0
		self.source = ""
	
	def tokenize(self, source):
		self.source = source
		end = False
		while (not end) and (self.current_char < len(self.source)):
			self.skip()
			tk: list[None | CCCTokenResult] = [None, None, None, None, None, None, None]
			tk[0] = self.try_basic_token()
			tk[1] = self.try_int_token()
			tk[2] = self.try_float_token()
			tk[3] = self.try_char_token()
			tk[4] = self.try_string_token()
			tk[5] = self.try_ident_token()
			tk[6] = self.try_kw_token()
			found: bool = False
			for t in tk:
				if t == None:
					continue
				if t[0]:
					found = True
					self.tokens.append(t[1])
					self.current_char += t[2]
			if not found:
				end = True
		self.tokens.append(EOF)
		return self.tokens
	
	def peek(self, offset=0):
		return self.source[self.current_char + offset]
	
	def peek_str(self, len: int, offset=0):
		ret = ""
		for i in range(len):
			ret += self.peek(offset + i)
		return ret
	
	def next_char(self):
		ret = self.source[self.current_char]
		self.current_char += 1
		return ret
	
	def try_basic_token(self) -> CCCTokenResult:
		found = False
		try:
			for btk in BASIC_TOKENS:
				for i in range(len(btk)):
					if self.peek(i) != btk[i]:
						found = False
						break
					else:
						found = True
				if found:
					return (True, btk, len(btk))
		except IndexError:
			pass
		return (False, None, 0)
	
	def try_int_token(self) -> CCCTokenResult:
		try:
			offset = 0
			accum = ""
			while self.peek(offset).isdecimal():
				accum += self.peek(offset)
				offset += 1
			if len(accum) > 0:
				return (True, IntegerTok(int(accum)), offset)
		except IndexError:
			pass
		return (False, None, 0)
	
	def try_float_token(self) -> CCCTokenResult:
		return (False, None, 0)
	
	def try_char_token(self) -> CCCTokenResult:
		return (False, None, 0)
	
	def try_string_token(self) -> CCCTokenResult:
		return (False, None, 0)
	
	def try_ident_token(self) -> CCCTokenResult:
		try:
			if self.peek_str(4) == "main":
				return (True, IdentTok("main"), 4)
		except IndexError:
			pass
		return (False, None, 0)
	
	def try_kw_token(self) -> CCCTokenResult:
		try:
			for kwtk in KW_TOKENS:
				if self.peek_str(len(kwtk)) == kwtk:
					return (True, KwTok(kwtk), len(kwtk))
		except IndexError:
			pass
		return (False, None, 0)
	
	def skip(self):
		try:
			while self.peek().isspace() or (self.peek_str(2)) == "//" or (self.peek_str(2) == "/*"):
				if self.peek().isspace():
					self.skip_ws()
				elif self.peek_str(2) == "//":
					self.skip_line_comment()
				elif self.peek_str(2) == "/*":
					self.skip_block_comment()
		except IndexError:
			pass
	
	def skip_ws(self):
		while self.peek().isspace():
			self.current_char += 1
	
	def skip_line_comment(self):
		count = 0
		if self.peek_str(2) == "//":
			count = 2
			i = 0
			while self.peek(2 + i) != "\n":
				i += 1
			count += i
			self.current_char += count
	
	def skip_block_comment(self):
		count = 0
		if self.peek_str(2) == "/*":
			count = 2
			i = 0
			while self.peek_str(2, 2 + i) != "*/":
				i += 1
			count += i + 1
			self.current_char += count

