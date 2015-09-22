#include "wasm_ast.h"

namespace wasm {

// There's really no need for this to be in a separate cc file, other
// that to avoid removing wasm_ast.cc altogether. If it turns out that
// it's unneeded once we are doing real codegen, then it can go away.

std::string Segment::as_string() const {
  return std::string(initial_data.begin(), initial_data.end());
}

}  // namespace wasm
