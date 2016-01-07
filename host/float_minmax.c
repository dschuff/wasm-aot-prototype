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

float __wasm_float_min_f32(float lhs, float rhs) {
  // If either operand is NaN, return it
  if (lhs != lhs)
    return lhs;
  if (rhs != rhs)
    return rhs;
  return lhs < rhs ? lhs : rhs;
}

double __wasm_float_min_f64(double lhs, double rhs) {
  // If either operand is NaN, return it
  if (lhs != lhs)
    return lhs;
  if (rhs != rhs)
    return rhs;
  return lhs < rhs ? lhs : rhs;
}

float __wasm_float_max_f32(float lhs, float rhs) {
  // If either operand is NaN, return it
  if (lhs != lhs)
    return lhs;
  if (rhs != rhs)
    return rhs;
  return lhs > rhs ? lhs : rhs;
}

double __wasm_float_max_f64(double lhs, double rhs) {
  // If either operand is NaN, return it
  if (lhs != lhs)
    return lhs;
  if (rhs != rhs)
    return rhs;
  return lhs > rhs ? lhs : rhs;
}
