#include "ast_dumper.h"
#include "waot_visitor.h"
#include "wasm.h"
#include "wasm_parser_cxx.h"

#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LLVMContext.h"
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

static llvm::cl::opt<bool> g_disable_verify(
    "disable-verify",
    llvm::cl::desc("Disable the module verifier"),
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

  llvm::LLVMContext& context = llvm::getGlobalContext();
  llvm::ModulePassManager mpm{};
  assert(g_print_asm);  // For now, only support printing assembly.
  if (!g_disable_verify)
    mpm.addPass(llvm::VerifierPass());
  mpm.addPass(llvm::PrintModulePass(output->os()));

  parser.modules.front()->name = llvm::sys::path::stem(g_input_filename);
  auto llvm_module =
      llvm::make_unique<llvm::Module>(parser.modules.front()->name, context);
  WAOTVisitor converter(llvm_module.get());
  wasm::AstDumper dumper(true);
  for (auto& module : parser.modules) {
    if (g_dump_ast)
      dumper.Visit(*module);
    converter.Visit(*module);
  }

  if (g_spec_test_script_mode) {
    for (auto& script_expr : parser.test_script) {
      if (g_dump_ast)
        dumper.Visit(script_expr.get());
      converter.Visit(script_expr.get());
    }
  } else {
    // If there's an export called "_start", add it to the end of the ini list.
    assert(parser.modules.size() == 1);
    for (auto& exp : parser.modules.front()->exports) {
      if (exp->name == "_start") {
        if (!converter.SetEntryExport(exp->function)) {
          fprintf(stderr, "error: _start export is not of type void ()*\n");
          exit(1);
        }
      }
    }
  }
  converter.FinishLLVMModule();

  mpm.run(*llvm_module);
  output->keep();

  return 0;
}
