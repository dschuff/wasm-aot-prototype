#include "wasm.h"
#include "wasm-parse.h"

#include "wasm-cpp.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

static llvm::cl::opt<std::string>
    InputFilename(llvm::cl::Positional, llvm::cl::desc("<input sexpr file>"),
                  llvm::cl::init("-"));

static llvm::cl::opt<bool>
    DumpInput("i", llvm::cl::desc("Dump input as well as output"),
              llvm::cl::init(false));

int main(int argc, char **argv) {
  llvm::cl::ParseCommandLineOptions(argc, argv, "wasm IR dumper\n");

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> ErrorOrBuffer =
      llvm::MemoryBuffer::getFileOrSTDIN(InputFilename);

  if (ErrorOrBuffer.getError()) {
    llvm::errs() << "unable to read " << InputFilename << "\n";
    return 1;
  }

  auto &Buffer = ErrorOrBuffer.get();

  if (DumpInput) {
    llvm::errs() << "INPUT:\n";
    llvm::errs() << Buffer->getBuffer();
    llvm::errs() << "OUTPUT:\n";
  }
  wasm::Parser DumbParser(Buffer->getBufferStart(), Buffer->getBufferEnd(),
                          InputFilename.c_str(), false);
  if (DumbParser.Parse()) {
    return 1;
  }
  DumbParser.module.dump();

  return 0;
}
