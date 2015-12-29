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

#define EACH_CALLBACK0               \
  CALLBACK(before_module)            \
  CALLBACK(after_module)             \
  CALLBACK(before_function)          \
  CALLBACK(after_nop)                \
  CALLBACK(before_if)                \
  CALLBACK(before_return)            \
  CALLBACK(after_invoke)             \
  CALLBACK(before_assert_return)     \
  CALLBACK(before_assert_return_nan) \
  CALLBACK(before_assert_trap)

#define EACH_CALLBACK1                    \
  CALLBACK(error, const char*)            \
  CALLBACK(after_function, int)           \
  CALLBACK(after_export, const char*)     \
  CALLBACK(before_block, int)             \
  CALLBACK(before_call, int)              \
  CALLBACK(before_call_import, int)       \
  CALLBACK(after_return, WasmType)        \
  CALLBACK(after_get_local, int)          \
  CALLBACK(before_set_local, int)         \
  CALLBACK(before_unary, WasmOpcode)      \
  CALLBACK(before_binary, WasmOpcode)     \
  CALLBACK(before_compare, WasmOpcode)    \
  CALLBACK(before_convert, WasmOpcode)    \
  CALLBACK(after_assert_return, WasmType) \
  CALLBACK(after_assert_return_nan, WasmType)

#define EACH_CALLBACK2                 \
  CALLBACK(after_block, WasmType, int) \
  CALLBACK(after_if, WasmType, int)    \
  CALLBACK(before_invoke, const char*, int)

#define EACH_CALLBACK3 CALLBACK(after_const, WasmOpcode, WasmType, WasmNumber)

#define EACH_CALLBACK4 \
  CALLBACK(before_store, WasmOpcode, WasmMemType, uint32_t, uint64_t)

#define EACH_CALLBACK5 \
  CALLBACK(before_load, WasmOpcode, WasmMemType, uint32_t, uint64_t, int)

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

#define CALLBACK(name, ...) parser.name = wrapper_##name;
    EACH_CALLBACK0
    EACH_CALLBACK1
    EACH_CALLBACK2
    EACH_CALLBACK3
    EACH_CALLBACK4
    EACH_CALLBACK5
#undef CALLBACK
  }
  int Parse(bool spec_script_mode) {
    if (spec_script_mode)
      return wasm_parse_file(&source_, &parser, &parser_options_);
    return wasm_parse_module(&source_, &parser, &parser_options_);
  }
  void SetCurrentCallbackInfo(WasmParserCallbackInfo* info) {
    current_callback_info_ = info;
  }

  UniquePtrVector<Module> modules;
  UniquePtrVector<TestScriptExpr> test_script;

 private:
  template <typename T>
  void SetCookie(T value) {
    current_callback_info_->cookie = reinterpret_cast<WasmParserCookie>(value);
  }
  template <typename T>
  T GetCookie() {
    assert(current_callback_info_->cookie != 0);
    return reinterpret_cast<T>(current_callback_info_->cookie);
  }

#define CALLBACK(name, ...) void name(__VA_ARGS__);
  EACH_CALLBACK0
  EACH_CALLBACK1
  EACH_CALLBACK2
  EACH_CALLBACK3
  EACH_CALLBACK4
  EACH_CALLBACK5
#undef CALLBACK

  Module* module = nullptr;
  WasmParserCallbacks parser = {};
  WasmParserOptions parser_options_ = {0};
  WasmParserCallbackInfo* current_callback_info_ = nullptr;
  WasmSource source_;
  std::string filename_;
  bool desugar_;
  Function* current_func_ = nullptr;
  TestScriptExpr* current_assert_return_ = nullptr;
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
#define CALLBACK(name)                                       \
  static void wrapper_##name(WasmParserCallbackInfo* info) { \
    Parser* p(static_cast<Parser*>(info->user_data));        \
    p->SetCurrentCallbackInfo(info);                         \
    return p->name();                                        \
  }
  EACH_CALLBACK0
#undef CALLBACK

#define CALLBACK(name, arg1ty)                                            \
  static void wrapper_##name(WasmParserCallbackInfo* info, arg1ty arg1) { \
    Parser* p(static_cast<Parser*>(info->user_data));                     \
    p->SetCurrentCallbackInfo(info);                                      \
    return p->name(arg1);                                                 \
  }
  EACH_CALLBACK1
#undef CALLBACK

#define CALLBACK(name, arg1ty, arg2ty)                          \
  static void wrapper_##name(                                   \
      WasmParserCallbackInfo* info, arg1ty arg1, arg2ty arg2) { \
    Parser* p(static_cast<Parser*>(info->user_data));           \
    p->SetCurrentCallbackInfo(info);                            \
    return p->name(arg1, arg2);                                 \
  }
  EACH_CALLBACK2
#undef CALLBACK

#define CALLBACK(name, arg1ty, arg2ty, arg3ty)                               \
  static void wrapper_##name(                                                \
      WasmParserCallbackInfo* info, arg1ty arg1, arg2ty arg2, arg3ty arg3) { \
    Parser* p(static_cast<Parser*>(info->user_data));                        \
    p->SetCurrentCallbackInfo(info);                                         \
    return p->name(arg1, arg2, arg3);                                        \
  }
  EACH_CALLBACK3
#undef CALLBACK

#define CALLBACK(name, arg1ty, arg2ty, arg3ty, arg4ty)     \
  static void wrapper_##name(WasmParserCallbackInfo* info, \
                             arg1ty arg1,                  \
                             arg2ty arg2,                  \
                             arg3ty arg3,                  \
                             arg4ty arg4) {                \
    Parser* p(static_cast<Parser*>(info->user_data));      \
    p->SetCurrentCallbackInfo(info);                       \
    return p->name(arg1, arg2, arg3, arg4);                \
  }
  EACH_CALLBACK4
#undef CALLBACK

#define CALLBACK(name, arg1ty, arg2ty, arg3ty, arg4ty, arg5ty) \
  static void wrapper_##name(WasmParserCallbackInfo* info,     \
                             arg1ty arg1,                      \
                             arg2ty arg2,                      \
                             arg3ty arg3,                      \
                             arg4ty arg4,                      \
                             arg5ty arg5) {                    \
    Parser* p(static_cast<Parser*>(info->user_data));          \
    p->SetCurrentCallbackInfo(info);                           \
    return p->name(arg1, arg2, arg3, arg4, arg5);              \
  }
  EACH_CALLBACK5
#undef CALLBACK

  void ParseCall(bool is_import, int index);
};

}  // namespace wasm
#endif  // WASM_CPP
