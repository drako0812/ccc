import inspect
from types import FrameType
from typing import cast

class ErrorLog:
    def __init__(self, output_py_info: bool = True):
        self.output_py_info: bool = output_py_info
        self.errors: list[str] = []
    
    def clear(self):
        self.errors.clear()
    
    def error(self, message: str, file: str | None = None, line: str | None = None, col: str | None = None):
        final_msg: str = ""
        cf = cast(FrameType, inspect.currentframe()).f_back
        fi = inspect.getframeinfo(cast(FrameType, cf))
        if self.output_py_info:
            final_msg += f"{fi.function}@{fi.filename}:{fi.lineno} :: "
        final_msg += f"{message}"
        if file is not None:
            final_msg += f" {file}"
        if line is not None:
            final_msg += f":{line}"
        if col is not None:
            final_msg += f":{col}"
        
        self.errors.append(final_msg)
    
    def is_empty(self) -> bool:
        return len(self.errors) == 0

    def __str__(self) -> str:
        output = ""

        for e in self.errors:
            output += f"{e}\n"
        
        return output

GlobalErrorLog = ErrorLog()

