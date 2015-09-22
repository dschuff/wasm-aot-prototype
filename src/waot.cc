#include "ast_dumper.h"
#include "waot.h"
#include "wasm.h"
#include "wasm_parser_cxx.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"

static llvm::cl::opt<std::string> g_input_filename(
    llvm::cl::Positional,
    llvm::cl::desc("<input sexpr file>"),
    llvm::cl::init("-"));

static llvm::cl::opt<bool> g_dump_input(
    "i",
    llvm::cl::desc("Dump input as well as output"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> g_dump_ast(
    "a",
    llvm::cl::desc("Dump AST as well as output"),
    llvm::cl::init(false));

int main(int argc, char** argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "wasm IR dumper\n");

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ErrorOrBuffer =
      llvm::MemoryBuffer::getFileOrSTDIN(g_input_filename);

  if (ErrorOrBuffer.getError()) {
    llvm::errs() << "unable to read " << g_input_filename << "\n";
    return 1;
  }

  auto& Buffer = ErrorOrBuffer.get();

  if (g_dump_input) {
    llvm::errs() << "INPUT:\n";
    llvm::errs() << Buffer->getBuffer();
    llvm::errs() << "OUTPUT:\n";
  }
  wasm::Parser TheParser(Buffer->getBufferStart(), Buffer->getBufferEnd(),
                         g_input_filename.c_str(), false);
  if (TheParser.Parse()) {
    return 1;
  }

  if (g_dump_ast) {
    wasm::AstDumper dumper;
    dumper.Visit(TheParser.module);
  }
  WAOTVisitor converter;
  auto llvm_module = converter.Visit(TheParser.module);
  llvm_module->dump();
  return 0;
}
