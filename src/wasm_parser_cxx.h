#ifndef WASM_CPP
#define WASM_CPP

#include "wasm.h"
#include "wasm-parse.h"
#include "wasm_ast.h"

#include <limits>
#include <string>
#include <unordered_map>
#include <cassert>
#include <cstring>

namespace wasm {

#define EACH_CALLBACK0                     \
  CALLBACK(after_nop, void)                \
  CALLBACK(before_block, WasmParserCookie) \
  CALLBACK(before_if, WasmParserCookie)    \
  CALLBACK(before_return, void)            \
  CALLBACK(before_assert_eq, WasmParserCookie)

#define EACH_CALLBACK1                       \
  CALLBACK(before_call, void, int)           \
  CALLBACK(before_call_import, void, int)    \
  CALLBACK(after_return, void, WasmType)     \
  CALLBACK(after_get_local, void, int)       \
  CALLBACK(before_set_local, void, int)      \
  CALLBACK(before_compare, void, WasmOpcode) \
  CALLBACK(before_module, void, WasmModule*) \
  CALLBACK(after_module, void, WasmModule*)  \
  CALLBACK(after_invoke, void, WasmParserCookie)

#define EACH_CALLBACK2                                        \
  CALLBACK(error, void, WasmSourceLocation, const char*)      \
  CALLBACK(before_function, void, WasmModule*, WasmFunction*) \
  CALLBACK(before_invoke, WasmParserCookie, const char*, int) \
  CALLBACK(after_assert_eq, void, WasmType, WasmParserCookie)

#define EACH_CALLBACK3                                            \
  CALLBACK(after_block, void, WasmType, int, WasmParserCookie)    \
  CALLBACK(after_if, void, WasmType, int, WasmParserCookie)     \
  CALLBACK(after_const, void, WasmOpcode, WasmType, WasmNumber)   \
  CALLBACK(after_function, void, WasmModule*, WasmFunction*, int) \
  CALLBACK(after_export, void, WasmModule*, WasmFunction*, const char*)

class Parser {
 public:
  Parser(const char* start,
         const char* end,
         const std::string& filename,
         bool desugar)
      : filename_(filename), desugar_(desugar) {
    source_.filename = filename_.c_str();
    source_.start = start;
    source_.end = end;
    parser.user_data = this;

#define CALLBACK(name, retty, ...) parser.name = wrapper_##name;
    EACH_CALLBACK0
    EACH_CALLBACK1
    EACH_CALLBACK2
    EACH_CALLBACK3
#undef CALLBACK
  }
  int Parse(bool spec_script_mode) {
    if (spec_script_mode)
      return wasm_parse_file(&source_, &parser);
    return wasm_parse_module(&source_, &parser);
  }

  UniquePtrVector<Module> modules;
  UniquePtrVector<TestScriptExpr> test_script;

 private:
#define CALLBACK(name, retty, ...) retty name(__VA_ARGS__);
  EACH_CALLBACK0
  EACH_CALLBACK1
  EACH_CALLBACK2
  EACH_CALLBACK3
#undef CALLBACK

  Module* module = nullptr;
  WasmParserCallbacks parser = {};
  WasmSource source_;
  std::string filename_;
  bool desugar_;
  Function* current_func_ = nullptr;
  TestScriptExpr* current_assert_eq_ = nullptr;
  Type current_type_ = Type::kVoid;

  std::unordered_map<WasmFunction*, Function*> functions_;
  std::unordered_map<std::string, Export*> exports_by_name_;

  // Expression insertion points are a UniquePtrVector onto which the expression
  // will be appended. Keep the insertion points in a stack. For expressions
  // which have a sub-list (call args, binary ops, return val), keep a count of
  // how many exprs we expect, so we can pop the stack after we see them (this
  // is because there is no "after_foo" event for these. Block is special
  // because it has an unknown number of subexprs, but it has an after_block
  // callback (likewise for the implicit block of a function body).
  struct InsertionState {
    InsertionState(UniquePtrVector<Expression>* p, int e)
        : point(p), exprs(e) {}
    UniquePtrVector<Expression>* point;
    int exprs;
  };
  const static int kUnknownExpectedExprs = std::numeric_limits<int>::max();
  std::vector<InsertionState> insertion_points_;
  void Insert(Expression* ex) {
    assert(insertion_points_.size());
    InsertionState& is = insertion_points_.back();
    is.point->emplace_back(ex);
    if (is.exprs != kUnknownExpectedExprs && --is.exprs == 0)
      PopInsertionPoint();
  }
  void ResetInsertionPoint(UniquePtrVector<Expression>* point,
                           int expected_exprs) {
    assert(insertion_points_.empty());
    insertion_points_.emplace_back(point, expected_exprs);
  }
  void InsertAndPush(Expression* ex, int expected_exprs) {
    Insert(ex);
    PushInsertionPoint(&ex->exprs, expected_exprs);
  }
  void PushInsertionPoint(UniquePtrVector<Expression>* point,
                          int expected_exprs) {
    if (expected_exprs > 0)
      insertion_points_.emplace_back(point, expected_exprs);
  }
  void PopInsertionPoint() {
    assert(insertion_points_.size() > 0 &&
           (insertion_points_.back().exprs == kUnknownExpectedExprs ||
            insertion_points_.back().exprs == 0));
    insertion_points_.pop_back();
  }

// The callbacks unfortunately are split by arity because we need to
// interleave the va-args (which are the arg types) with arg names, and
// variadic macros don't have a way to do that.
#define CALLBACK(name, retty)                  \
  static retty wrapper_##name(void* user) {    \
    return static_cast<Parser*>(user)->name(); \
  }
  EACH_CALLBACK0
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty)                    \
  static retty wrapper_##name(arg1ty arg1, void* user) { \
    return static_cast<Parser*>(user)->name(arg1);       \
  }
  EACH_CALLBACK1
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty, arg2ty)                         \
  static retty wrapper_##name(arg1ty arg1, arg2ty arg2, void* user) { \
    return static_cast<Parser*>(user)->name(arg1, arg2);              \
  }
  EACH_CALLBACK2
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty, arg2ty, arg3ty)                \
  static retty wrapper_##name(arg1ty arg1, arg2ty arg2, arg3ty arg3, \
                              void* user) {                          \
    return static_cast<Parser*>(user)->name(arg1, arg2, arg3);       \
  }
  EACH_CALLBACK3
#undef CALLBACK

  void ParseCall(bool is_import, int index);
};

}  // namespace wasm
#endif  // WASM_CPP
