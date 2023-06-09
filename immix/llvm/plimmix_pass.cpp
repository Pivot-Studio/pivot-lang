#include "llvm/IR/PassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
using namespace llvm;
namespace
{
  struct Immix : public ModulePass
  {
    static char ID;
    Immix() : ModulePass(ID) {}

    bool runOnModule(Module &M) override
    {
      for (auto FB = M.functions().begin(), FE = M.functions().end(); FB != FE; ++FB)
      {
        Function *FV = &*FB;
        FV->setGC("plimmix");
      }
      // auto gc_init_c = M.getOrInsertFunction("__gc_init_stackmap", Type::getVoidTy(M.getContext()));
      auto immix_init_c = M.getOrInsertFunction("immix_gc_init", Type::getVoidTy(M.getContext()), PointerType::get(IntegerType::get(M.getContext(), 8), 0));
      auto immix_init_f = cast<Function>(immix_init_c.getCallee());
      immix_init_f->setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      SmallVector<Type *, 1> argTypes;
      argTypes.push_back(PointerType::get(IntegerType::get(M.getContext(), 8), 0));
      std::string symbol;
      symbol += "_IMMIX_GC_MAP_";
      symbol += M.getSourceFileName();
      auto g = M.getOrInsertGlobal(symbol, Type::getInt8Ty(M.getContext()));
      GlobalVariable *g_c = cast<GlobalVariable>(g);
      g_c->setLinkage(GlobalValue::LinkageTypes::ExternalWeakLinkage);
      // auto g = M.getNamedGlobal(symbol);
      SmallVector<Value *, 1> assertArgs;
      assertArgs.push_back(g);
      Function *gc_init_f;
      std::tie(gc_init_f, std::ignore) = createSanitizerCtorAndInitFunctions(M, "__gc_init_stackmap", "immix_gc_init", argTypes, assertArgs);
      gc_init_f->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
      // appendToCompilerUsed(M, gc_init_f);
      appendToGlobalCtors(M, gc_init_f, 1000);

      return true;
    }
  };
}
char Immix::ID = 0;
static RegisterPass<Immix> X("plimmix", "plimmix gc Pass",
                             false /* Only looks at CFG */,
                             false /* Analysis Pass */);

// static llvm::RegisterStandardPasses Y(
//     llvm::PassManagerBuilder::EP_EarlyAsPossible,
//     [](const llvm::PassManagerBuilder &Builder,
//        llvm::legacy::PassManagerBase &PM)
//     { PM.add(new Immix()); });
