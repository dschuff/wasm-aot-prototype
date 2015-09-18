#ifndef WASM_AST
#define WASM_AST

#include "wasm.h"

#include <memory>
#include <string>
#include <vector>

namespace wasm {

class Literal {
 public:
  WasmType type = WASM_TYPE_VOID;
  union {
    uint32_t i32;
    uint64_t i64;
    float f32;
    double f64;
  } value;
  void dump();
};

class Variable {
 public:
  WasmType type = WASM_TYPE_VOID;
  std::string local_name;  // Empty if none bound
};

class Expression {
 public:
  WasmOpType opcode = WASM_OP_NOP;
  WasmType expr_type = WASM_TYPE_VOID;

  Literal literal = {};
  std::vector<std::unique_ptr<Expression>> exprs;
  Expression(WasmOpType op) : opcode(op) {}
  void dump();
};

class Callable {
 public:
  WasmType result_type = WASM_TYPE_VOID;
  std::vector<Variable> args;
  std::string local_name;  // Empty if none bound

  // Functions can be declared with a single parameter list like
  // (func (param i32 i64)) or with a split parameter list like
  // (func (param i32) (param i32)
  // If any of the params have name bindings e.g. (param $n i32) then the
  // style must be used. However imports are required to use the single
  // style. If this is fixed, arg dumping can be shared here.
  void dump_result();
  void dump();
};

class Function : public Callable {
 public:
  std::vector<Variable> locals;
  std::string export_name;  // Empty if not exported.
  std::vector<std::unique_ptr<Expression>> body;
  int index_in_module = 0;
  bool is_external = false;
  int depth = 0;

  void dump_var_list(const std::vector<Variable>& lst, const char* name);
  void dump();
};

class Import : public Callable {
 public:
  std::string module_name;
  std::string func_name;
  void dump();
};

class Segment {
 public:
  size_t size = 0;
  size_t address = 0;
  std::vector<char> initial_data;
  std::string as_string() const {
    return std::string(initial_data.begin(), initial_data.end());
  }
  void dump() {
    printf("(segment %zu \"%s\")\n", address, as_string().c_str());
  }
};

class Module {
 public:
  std::vector<Function> functions;
  std::vector<Function*> exports;
  std::vector<Segment> segments;
  std::vector<Import> imports;
  uint32_t initial_memory_size = 0;
  uint32_t max_memory_size = 0;

  void dump();
};

}  // namespace wasm
#endif  // WASM_AST
