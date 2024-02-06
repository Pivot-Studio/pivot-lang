/*
  Some LLVM passes helpful for
  integrating immix with LLVM
*/

#include "llvm/IR/PassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Analysis/CaptureTracking.h"
using namespace llvm;
namespace
{
  void immixPassLogic(Module &M);
  // The new pass manager plugin
  class ImmixPass : public PassInfoMixin<ImmixPass>
  {
    static char ID;

  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  };
  PreservedAnalyses ImmixPass::run(Module &M, ModuleAnalysisManager &AM)
  {
    // printf("running immix pass\n");
    immixPassLogic(M);
    return PreservedAnalyses::none();
  }

  class EscapePass : public PassInfoMixin<EscapePass>
  {
    static char ID;

  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
  };


  PreservedAnalyses EscapePass::run(Module &M, ModuleAnalysisManager &AM)
  {
    // printf("running escape analysis\n");
    // new builder
    IRBuilder<> builder(M.getContext());

    std::vector<CallInst *> mallocs;
    std::vector<PHINode *> phis;
    std::vector<AllocaInst *> allocas;

    for (auto FB = M.functions().begin(), FE = M.functions().end(); FB != FE; ++FB)
    {
      Function *FV = &*FB;
      if (!FV->getName().ends_with("visitorf@") && !FV->getName().starts_with("llvm") && !FV->isDeclaration())
      {
        // doing escape analysis
        for (auto &BB : *FV)
        {
          for (auto &I : BB)
          {
            // if it's a call instruction and the alloc-family is gc
            if (auto *call = dyn_cast<CallInst>(&I))
            {
              if (auto *F = call->getCalledFunction())
              {
                if (!F->getName().contains("malloc"))
                {
                   continue;
                }
                
                // if (F->getFnAttribute("alloc-family").getValueAsString() == "gc")
                // {
                // }
                if (!PointerMayBeCaptured(call, true, true))
                {
                  // printf("not captured\n");
                  // if the pointer may be captured, then we change this gc malloc
                  // to stack alloca
                  
                  // the first argument of gc malloc is the size
                  auto *size = call->getArgOperand(0);
                  // size is a number, get it's value

                  auto attrs = call->getAttributes();
                  if (!attrs.hasRetAttr("pl_atomic"))
                  {
                    continue;
                  }
                  auto *sizeValue = dyn_cast<ConstantInt>(size);
                  auto sizeInt = sizeValue->getZExtValue();
                  // get the size value

                  // replace it with alloca
                  builder.SetInsertPoint(&FV->front().front());
                  auto &alloca = *builder.CreateAlloca(ArrayType::get(IntegerType::get(M.getContext(), 8), sizeInt));
                  // auto &cast = *builder.CreateAddrSpaceCast(&alloca, PointerType::get(IntegerType::get(M.getContext(), 8), 1));
                  // replace the gc malloc with alloca
                  call->replaceAllUsesWith(&alloca);
                  allocas.push_back(&alloca);
                  
                  
                  mallocs.push_back(call);


                  // // get function from call
                  // auto *F = call->getCalledFunction();
                  // // get first bb
                  // auto &firstBB = FV->front();
                  // // get the first instruction
                  // auto &firstI = firstBB.front();
                
                  // // generate an alloca, allocationg i8 array of size
                  // // alloca should in the first block of the function
                  // auto &alloca = *builder.CreateAlloca(ArrayType::get(IntegerType::get(M.getContext(), 8), sizeInt));
                  // // cast addressspace to 1
                  // auto &cast = *builder.CreateAddrSpaceCast(&alloca, PointerType::get(IntegerType::get(M.getContext(), 8), 1));
                  // // replace the gc malloc with alloca
                  // call->replaceAllUsesWith(&cast);
                  // printf("replaced in function %s\n", FV->getName().str().c_str());

                }
                
              }
            }
          }
        }
      }
      
    }
    for (auto *alloca : allocas)
    {
      // correect phi
      auto uses = alloca->uses();
      while (true)
      {
        auto noMorePhi = true;
        for (auto &U : uses)
        {
          
          auto *user = U.getUser();
          // if it's phi
          if (auto *phi = dyn_cast<PHINode>(user))
          {
            noMorePhi = false;
            // build a new phi which address space is 0
            builder.SetInsertPoint(phi->getParent()->getFirstNonPHI());
            auto newphi = builder.CreatePHI(PointerType::get(IntegerType::get(M.getContext(), 8), 0), phi->getNumIncomingValues());
            for (unsigned i = 0; i < phi->getNumIncomingValues(); i++)
            {
              auto *incoming = phi->getIncomingValue(i);
              auto *incomingBlock = phi->getIncomingBlock(i);
              newphi->addIncoming(incoming, incomingBlock);
            }
            phi->replaceAllUsesWith(newphi);
            phis.push_back(phi);
            // printf("replaced phi\n");
            uses = newphi->uses();

          }

        }
        if (noMorePhi)
        {
          break;
        }
      }
    }
    for (auto *call : mallocs)
    {
      call->eraseFromParent();
      // printf("erased gcmalloc\n");
    }
    for (auto *phi : phis)
    {
      if (phi->getParent())
      {
        phi->eraseFromParent();
      }
      
      // printf("erased phi\n");
    }
    return PreservedAnalyses::none();
  }



  /*
    This pass helps integrate immix with LLVM.

    It does the following:
    - Sets the GC name to "statepoint-example" for all functions. 
      TODO: current llvm-16 does not support custom gc strategy using
      RewriteStatepointsForGC pass. So we need to use the default gc strategy.
    - Adds a call to immix_gc_init in the global constructor
    - Adds a global variable declaration for the module stack map if the main function is present.

    However, it does not generate the stack map. This is done by the
    immix compiler plugin.

    Also note that mauch more work is needed to get immix working with
    LLVM. Besides the pass, you need to:
    - Implement visit functions for all complex types
    - replace all malloc calls with immix::alloc
    ...
  */
  void immixPassLogic(Module &M)
  {
    auto isMain = false;
    for (auto FB = M.functions().begin(), FE = M.functions().end(); FB != FE; ++FB)
    {
      Function *FV = &*FB;
      if (FV->getName() == "main")
      {
        isMain = true;
      }
      if (!FV->getName().ends_with("visitorf@") && !FV->getName().starts_with("llvm"))
      {
        FV->setGC("statepoint-example");

      }
      
    }
    if (isMain)
    {
      // auto gc_init_c = M.getOrInsertFunction("__gc_init_stackmap", Type::getVoidTy(M.getContext()));
      auto immix_init_c = M.getOrInsertFunction("immix_gc_init", Type::getVoidTy(M.getContext()), PointerType::get(IntegerType::get(M.getContext(), 8), 0));
      auto immix_init_f = cast<Function>(immix_init_c.getCallee());
      immix_init_f->setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      SmallVector<Type *, 1> argTypes;
      argTypes.push_back(PointerType::get(IntegerType::get(M.getContext(), 8), 0));
      std::string symbol;
      // mac's symbol name has only one underscore
      #ifdef __APPLE__ 
      symbol += "_LLVM_StackMaps";
      #else
      symbol += "__LLVM_StackMaps";
      #endif
      // symbol += M.getSourceFileName();
      auto g = M.getOrInsertGlobal(symbol, Type::getInt8Ty(M.getContext()));
      GlobalVariable *g_c = cast<GlobalVariable>(g);
      
      g_c->setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);
      g_c->setDSOLocal(true);
      g_c->setConstant(true);
      // g_c->setSection("__llvm_stackmaps");
      g_c->setAlignment(llvm::Align(4));
      // M.appendModuleInlineAsm(".globl __LLVM_StackMaps");
      // auto g = M.getNamedGlobal(symbol);
      SmallVector<Value *, 1> assertArgs;
      assertArgs.push_back(g);
      Function *gc_init_f;
      std::tie(gc_init_f, std::ignore) = createSanitizerCtorAndInitFunctions(M, "__gc_init_stackmap", "immix_gc_init", argTypes, assertArgs);
      gc_init_f->setLinkage(GlobalValue::LinkageTypes::InternalLinkage);
      // appendToCompilerUsed(M, gc_init_f);
      appendToGlobalCtors(M, gc_init_f, 1000);
    }
    
  }

  // The old pass manager plugin
  struct ImmixLegacy : public ModulePass
  {
    static char ID;
    ImmixLegacy() : ModulePass(ID) {}

    bool runOnModule(Module &M) override
    {
      immixPassLogic(M);

      return true;
    }
  };
}

// char ImmixPass::ID = 0;
char ImmixLegacy::ID = 0;
static RegisterPass<ImmixLegacy> X("plimmix", "plimmix gc Pass",
                                   false /* Only looks at CFG */,
                                   false /* Analysis Pass */);
