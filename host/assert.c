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

#include <inttypes.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>

extern int32_t __wasm_exit_status;
// Define an assert failure function for each type. We could have used a
// vararg function, but we can't pass floats in ... because C promotes all
// of those args to int/double.
void __wasm_assert_fail_i32(int32_t line_num,
                            int32_t expected,
                            int32_t actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return on line %d: expected %d, got %d\n",
      line_num,
      expected,
      actual);
  __wasm_exit_status = 1;
}

void __wasm_assert_fail_i64(int32_t line_num,
                            int64_t expected,
                            int64_t actual) {
  fprintf(stderr,
          "Assertion failure in assert_return on line %d: expected %" PRId64
          ", got %" PRId64 "\n",
          line_num,
          expected,
          actual);
  __wasm_exit_status = 1;
}

void __wasm_assert_fail_f32(int32_t line_num, float expected, float actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return on line %d: expected %f, got %f\n",
      line_num,
      expected,
      actual);
  __wasm_exit_status = 1;
}

void __wasm_assert_fail_f64(int32_t line_num, double expected, double actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return on line %d: expected %a, got %a\n",
      line_num,
      expected,
      actual);
  __wasm_exit_status = 1;
}

void __wasm_assert_trap_fail(int32_t line_num) {
  fprintf(stderr,
          "Trap Assertion failure in assert_trap on line %d: no trap raised\n",
          line_num);
  __wasm_exit_status = 1;
}

void __wasm_assert_return_nan_f32(int32_t line_num, float value) {
  if (isnan(value))
    return;
  fprintf(stderr,
          "Assertion failure in assert_return_nan on line %d: expected NaN, "
          "got %f\n",
          line_num,
          value);
  __wasm_exit_status = 1;
}

void __wasm_assert_return_nan_f64(int32_t line_num, double value) {
  if (isnan(value))
    return;
  fprintf(stderr,
          "Assertion failure in assert_return_nan on line %d: expected NaN, "
          "got %lf\n",
          line_num,
          value);
  __wasm_exit_status = 1;
}
