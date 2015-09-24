#include "ast_dumper.h"
#include "waot.h"
#include "wasm.h"
#include "wasm_parser_cxx.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/ToolOutputFile.h"

using llvm::errs;

static llvm::cl::opt<std::string> g_input_filename(
    llvm::cl::Positional,
    llvm::cl::desc("input sexpr file"),
    llvm::cl::init("-"));

static llvm::cl::opt<std::string> g_output_filename(
    "o",
    llvm::cl::desc("output LLVM file"),
    llvm::cl::value_desc("filename"));

static llvm::cl::opt<bool> g_dump_input(
    "i",
    llvm::cl::desc("Dump input as well as output"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> g_dump_ast(
    "a",
    llvm::cl::desc("Dump AST as well as output"),
    llvm::cl::init(false));

static llvm::cl::opt<bool> g_print_asm(
    "S",
    llvm::cl::desc("Print LLVM assembly output"),
    llvm::cl::init(true));

static llvm::cl::opt<bool> g_spec_test_script_mode(
    "spec-test-script",
    llvm::cl::desc(
        "Run in spec test script mode (allow multiple modules per file and"
        "test assertions"),
    llvm::cl::init(false));

int main(int argc, char** argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();
  llvm::PrettyStackTraceProgram X(argc, argv);
  llvm::llvm_shutdown_obj Shutdown;  // Call llvm_shutdown() on exit.
  llvm::cl::ParseCommandLineOptions(argc, argv, "wasm IR dumper\n");

  // Get the input buffer.
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ErrorOrBuffer =
      llvm::MemoryBuffer::getFileOrSTDIN(g_input_filename);

  if (ErrorOrBuffer.getError()) {
    errs() << "unable to read " << g_input_filename << "\n";
    return 1;
  }
  auto& Buffer = ErrorOrBuffer.get();

  // Open the output file. Default to standard output.
  if (g_output_filename.empty())
    g_output_filename = "-";

  std::error_code EC;
  auto output = llvm::make_unique<llvm::tool_output_file>(
      g_output_filename, EC, llvm::sys::fs::F_None);
  if (EC) {
    errs() << EC.message() << '\n';
    return 1;
  }

  if (g_dump_input) {
    errs() << "INPUT:\n";
    errs() << Buffer->getBuffer();
    errs() << "OUTPUT:\n";
  }
  wasm::Parser parser(Buffer->getBufferStart(), Buffer->getBufferEnd(),
                      g_input_filename.c_str(), false);
  if (parser.Parse(g_spec_test_script_mode)) {
    return 1;
  }

  llvm::ModulePassManager mpm{};
  assert(g_print_asm);  // For now, only support printing assembly.
  mpm.addPass(llvm::VerifierPass());
  mpm.addPass(llvm::PrintModulePass(output->os()));

  parser.modules.front()->name = llvm::sys::path::stem(g_input_filename);
  auto llvm_module = llvm::make_unique<llvm::Module>(
      parser.modules.front()->name, llvm::getGlobalContext());
  WAOTVisitor converter(llvm_module.get());
  for (auto& module : parser.modules) {
    if (g_dump_ast) {
      wasm::AstDumper dumper;
      dumper.Visit(*module);
    }
    converter.Visit(*module);
  }
  mpm.run(*llvm_module);
  output->keep();
  return 0;
}
