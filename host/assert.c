#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

extern int32_t exit_status;
// Define an assert failure function for each type. We could have used a
// vararg function, but we can't pass floats in ... because C promotes all
// of those args to int/double.
void __assert_fail_i32(int32_t assert_num, int32_t expected, int32_t actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return expression %d: expected %d, got %d\n",
      assert_num, expected, actual);
  exit_status = 1;
}

void __assert_fail_i64(int32_t assert_num, int64_t expected, int64_t actual) {
  fprintf(stderr,
          "Assertion failure in assert_return expression %d: expected %" PRId64
          ", got %" PRId64 "\n",
          assert_num, expected, actual);
  exit_status = 1;
}

void __assert_fail_f32(int32_t assert_num, float expected, float actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return expression %d: expected %f, got %f\n",
      assert_num, expected, actual);
  exit_status = 1;
}

void __assert_fail_f64(int32_t assert_num, double expected, double actual) {
  fprintf(
      stderr,
      "Assertion failure in assert_return expression %d: expected %a, got %a",
      assert_num, expected, actual);
  exit_status = 1;
}
