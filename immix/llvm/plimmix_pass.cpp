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
      auto gc_init_c = M.getOrInsertFunction("__gc_init_stackmap", Type::getVoidTy(M.getContext()));
      auto immix_init_c = M.getOrInsertFunction("immix_gc_init", Type::getVoidTy(M.getContext()), PointerType::get(IntegerType::get(M.getContext(), 8), 0));
      auto immix_init_f = cast<Function>(immix_init_c.getCallee());
      immix_init_f->setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      Function *gc_init_f = cast<Function>(gc_init_c.getCallee());
      gc_init_f->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
      BasicBlock *block = BasicBlock::Create(M.getContext(), "entry", gc_init_f);
      IRBuilder<> builder(block);

      std::string symbol ;
      symbol += "_IMMIX_GC_MAP_";
      symbol += M.getSourceFileName();
      auto g = M.getOrInsertGlobal(symbol, Type::getInt8Ty(M.getContext()));
      GlobalVariable *g_c = cast<GlobalVariable>(g);
      g_c->setLinkage(GlobalValue::LinkageTypes::ExternalWeakLinkage);
      // auto g = M.getNamedGlobal(symbol);
      SmallVector<Value *, 1> assertArgs;
      assertArgs.push_back(g);
      builder.CreateCall(immix_init_c, assertArgs);
      builder.CreateRetVoid();
      // ctor_c->setInitializer(ConstantArray::get(ArrayType::get(stp,1),{ConstantStruct::get(stp,{ConstantInt::get(IntegerType::get(M.getContext(), 32), 65535),gc_init_f, ConstantExpr::getNullValue(Type::getInt8PtrTy(M.getContext()))})}));
      appendToCompilerUsed(M, gc_init_f);
      appendToGlobalCtors(M, gc_init_f, 1000);
      // errs() << "Hello: ";
      // errs().write_escaped(M.getName()) << '\n';
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
