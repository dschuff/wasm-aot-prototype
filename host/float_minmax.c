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
