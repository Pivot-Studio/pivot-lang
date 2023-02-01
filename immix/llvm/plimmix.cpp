// lib/MyGC/MyGC.cpp - Example LLVM GC plugin

// #include "llvm/CodeGen/GCStrategy.h"
#include "llvm/IR/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/Support/Compiler.h"
#include "plimmixprinter.cpp"

using namespace llvm;

namespace {
  class LLVM_LIBRARY_VISIBILITY PLImmixGC : public GCStrategy {
  public:
    PLImmixGC() {
        UsesMetadata = true;
        // NeededSafePoints = true;
    }
  };

  GCRegistry::Add<PLImmixGC>
  X("plimmix", "pivot-lang immix garbage collector.");
}

#include <llvm/IR/BuiltinGCs.h>

extern "C" void LLVMLinkPLImmixGC() {
}