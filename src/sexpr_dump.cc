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

#include "ast_dumper.h"
#include "wasm.h"
#include "wasm_parser_cxx.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string> InputFilename(
    llvm::cl::Positional,
    llvm::cl::desc("<input sexpr file>"),
    llvm::cl::init("-"));

static llvm::cl::opt<bool> DumpInput(
    "i",
    llvm::cl::desc("Dump input as well as output"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> DumpTypes(
    "t",
    llvm::cl::desc("Dump types of expressions"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> g_spec_test_script_mode(
    "spec-test-script",
    llvm::cl::desc(
        "Run in spec test script mode (allow multiple modules per file and"
        "test assertions"),
    llvm::cl::init(false));

int main(int argc, char** argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "wasm IR dumper\n");

  if (DumpInput) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer> > ErrorOrBuffer =
        llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);
    if (ErrorOrBuffer.getError()) {
      llvm::errs() << "unable to read " << InputFilename << "\n";
      return 1;
    }

    auto& Buffer = ErrorOrBuffer.get();
    llvm::errs() << "INPUT:\n";
    llvm::errs() << Buffer->getBuffer();
    llvm::errs() << "OUTPUT:\n";
  }
  wasm::Parser parser(InputFilename.c_str(), false);
  if (parser.Parse(g_spec_test_script_mode)) {
    return 1;
  }

  wasm::AstDumper dumper(DumpTypes);
  for (auto& module : parser.modules) {
    dumper.Visit(*module);
  }
  for (auto& script_expr : parser.test_script) {
    dumper.Visit(script_expr.get());
  }
  return 0;
}
