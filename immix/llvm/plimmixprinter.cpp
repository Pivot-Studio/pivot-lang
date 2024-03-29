#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/Support/Compiler.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DataLayout.h"
// #include "llvm/Target/TargetAsmInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/MC/MCAsmInfo.h"
using namespace llvm;

namespace
{
  /*
    Stackmap printer for immix.
  */
  class LLVM_LIBRARY_VISIBILITY PLImmixGCPrinter : public GCMetadataPrinter
  {
  public:
    virtual void beginAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP);

    virtual void finishAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP);
    virtual bool emitStackMaps(StackMaps &SM, AsmPrinter &AP) { return true; }
  };

  GCMetadataPrinterRegistry::Add<PLImmixGCPrinter>
      P("plimmix", "pivot-lang immix garbage collector.");
}


void PLImmixGCPrinter::beginAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP)
{
  // Nothing to do.
}


/*
  We need to emit the stack map in the data section.
*/
void PLImmixGCPrinter::finishAssembly(Module &M, GCModuleInfo &Info, AsmPrinter &AP)
{
  unsigned IntPtrSize = AP.getPointerSize();
  AP.emitAlignment(llvm::Align(8));
  // Put this in the data section.
  AP.OutStreamer.get()->switchSection(AP.getObjFileLowering().getDataSection());
  std::string symbol;
  symbol += "_IMMIX_GC_MAP_";
  symbol += M.getSourceFileName();
  // printf("symbol: %s \n", symbol.c_str());

  // *AP.OutStreamer.get()<< AP.MAI->getGlobalDirective();
  AP.emitGlobalConstant(M.getDataLayout(), M.getOrInsertGlobal(symbol, Type::getVoidTy(M.getContext())));
  AP.OutStreamer.get()->emitLabel(AP.GetExternalSymbolSymbol(symbol));
  AP.OutStreamer.get()->AddComment("plimmix stackmap format version");
  AP.emitInt32(1);
  AP.emitAlignment(llvm::Align(8));
  AP.OutStreamer.get()->AddComment("function numbers");

  int i = 0;
  for (auto FI = Info.funcinfo_begin(), FE = Info.funcinfo_end(); FI != FE; ++FI)
  {
    i++;
  }
  AP.emitInt32(i);
  // For each function...
  for (auto FI = Info.funcinfo_begin(), FE = Info.funcinfo_end(); FI != FE; ++FI)
  {
    GCFunctionInfo &MD = **FI;

    // Align to address width.
    AP.emitAlignment(llvm::Align(8));

    AP.OutStreamer.get()->AddComment("function address");
    const GlobalValue *GV = &MD.getFunction();
    AP.emitLabelPlusOffset(AP.getSymbol(GV) /*Hi*/, 0 /*Offset*/, IntPtrSize /*Size*/);

    // Stack information never change in safe points! Only print info from the
    // first call-site.
    GCFunctionInfo::iterator PI = MD.begin();

    // Emit the stack frame size.
    AP.OutStreamer.get()->AddComment("stack frame size (in words)");
    AP.emitInt32(MD.getFrameSize() / IntPtrSize);

    // Emit stack arity, i.e. the number of stacked arguments.
    unsigned RegisteredArgs = IntPtrSize == 4 ? 5 : 6;
    unsigned StackArity = MD.getFunction().arg_size() > RegisteredArgs ? MD.getFunction().arg_size() - RegisteredArgs : 0;
    AP.OutStreamer.get()->AddComment("stack arity");
    AP.emitInt32(StackArity);

    // Emit the number of live roots in the function.
    AP.OutStreamer.get()->AddComment("live root count");
    AP.emitInt32(MD.live_size(PI));

    // Emit PointCount.
    AP.OutStreamer.get()->AddComment("safe point count");
    AP.emitInt32(MD.size());

    // And each safe point...
    for (GCFunctionInfo::iterator PI = MD.begin(),
                                  PE = MD.end();
         PI != PE; ++PI)
    {
      // Emit the address of the safe point.
      AP.OutStreamer.get()->AddComment("safe point address");
      MCSymbol *Label = PI->Label;
      AP.emitLabelPlusOffset(Label /*Hi*/, 0 /*Offset*/, IntPtrSize /*Size*/);
    }

    // And for each live root...
    for (GCFunctionInfo::live_iterator LI = MD.live_begin(PI),
                                       LE = MD.live_end(PI);
         LI != LE; ++LI)
    {
      // Emit live root's offset within the stack frame.
      AP.OutStreamer.get()->AddComment("stack index (offset / wordsize)");
      AP.emitInt32(LI->StackOffset);
      auto meta = LI->Metadata->getName();
      if (meta.contains("ATOMIC"))
      {
        AP.emitInt32(0);
      }
      else if (meta.contains("TRAIT"))
      {
        AP.emitInt32(1);
      }
      else if (meta.contains("COMPLEX"))
      {
        AP.emitInt32(2);
      }
      else if (meta.contains("POINTER"))
      {
        AP.emitInt32(3);
      }
      else
      {
        AP.emitInt32(4);
      }
      // printf("%s\n", meta.bytes());
    }
  }
  AP.OutStreamer.get()->AddComment("global numbers");
  int ii = 0;
  for (auto GI = M.global_begin(), GE = M.global_end(); GI != GE; ++GI)
  {
    const GlobalValue *GV = &*GI;
    if (GV->getName().contains("llvm.")) // skip magic variables e.g. @llvm.global_ctors
    {
      continue;
    }

    if (isa<GlobalVariable>(GV) && cast<GlobalVariable>(GV)->isConstant()) // skip constants
    {
      // printf("skip %s\n", GV->getName().bytes());
      continue;
    }

    if (GV->getName().startswith("_IMMIX_GC_MAP") ||
        GV->getName().contains("_@IMMIX_OBJTYPE_") ||
        GV->getName().startswith(".str")) // skip generated globals
    {
      continue;
    }
    // printf("%s\n", GV->getName().bytes());

    ii++;
  }
  auto g = ii; // skip magic variables e.g. @llvm.global_ctors
  AP.emitInt32(g);
  // Align to address width.
  AP.emitAlignment(llvm::Align(8));
  for (auto GI = M.global_begin(), GE = M.global_end(); GI != GE; ++GI)
  {
    AP.OutStreamer.get()->AddComment("global address");
    const GlobalValue *GV = &*GI;
    if (GV->getName().contains("llvm.")) // skip magic variables e.g. @llvm.global_ctors
    {
      continue;
    }

    if (isa<GlobalVariable>(GV) && cast<GlobalVariable>(GV)->isConstant()) // skip constants
    {
      // printf("skip %s\n", GV->getName().bytes());
      continue;
    }

    if (GV->getName().startswith("_IMMIX_GC_MAP") ||
        GV->getName().contains("_@IMMIX_OBJTYPE_") ||
        GV->getName().startswith(".str")) // skip generated globals
    {
      continue;
    }

    AP.emitLabelPlusOffset(AP.getSymbol(GV) /*Hi*/, 0 /*Offset*/, IntPtrSize /*Size*/);
  }
}
