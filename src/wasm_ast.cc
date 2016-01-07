/*
 * Copyright 2016 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "wasm_ast.h"

namespace wasm {

// There's really no need for this to be in a separate cc file, other
// that to avoid removing wasm_ast.cc altogether. If it turns out that
// it's unneeded once we are doing real codegen, then it can go away.

std::string Segment::as_string() const {
  return std::string(initial_data.begin(), initial_data.end());
}

}  // namespace wasm
