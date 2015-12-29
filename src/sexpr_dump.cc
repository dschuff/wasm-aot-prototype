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

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer> > ErrorOrBuffer =
      llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);

  if (ErrorOrBuffer.getError()) {
    llvm::errs() << "unable to read " << InputFilename << "\n";
    return 1;
  }

  auto& Buffer = ErrorOrBuffer.get();

  if (DumpInput) {
    llvm::errs() << "INPUT:\n";
    llvm::errs() << Buffer->getBuffer();
    llvm::errs() << "OUTPUT:\n";
  }
  wasm::Parser parser(Buffer->getBufferStart(),
                      Buffer->getBufferEnd(),
                      InputFilename.c_str(),
                      false);
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
