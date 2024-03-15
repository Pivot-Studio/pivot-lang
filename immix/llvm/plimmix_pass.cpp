/*
  Some LLVM passes helpful for
  integrating immix with LLVM
*/

#include "llvm/IR/PassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IntrinsicInst.h"
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

  /*
    # An escape analysis pass

    This pass would find any gc heap allocation of `Atomic` type that's
    not necessary and replace them with stack allocation.

    ## About `Atomic` type

    A type that don't contain any gc pointers is an `Atomic` type.
    All primitives are `Atomic`.
  */
  class EscapePass : public PassInfoMixin<EscapePass>
  {
    static char ID;
    bool escaped;

  public:
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    void replace_geps(llvm::iterator_range<llvm::Value::user_iterator> &users, llvm::IRBuilder<> &builder, llvm::Value * alloca, std::__1::vector<llvm::GetElementPtrInst *> &geps);
    EscapePass() : escaped(false) {}
    EscapePass(bool escaped) : escaped(escaped) {}
  };


  PreservedAnalyses EscapePass::run(Module &M, ModuleAnalysisManager &AM)
  {
    IRBuilder<> builder(M.getContext());

    std::vector<CallInst *> mallocs;
    std::vector<PHINode *> phis;
    std::vector<AllocaInst *> allocas;
    std::vector<GetElementPtrInst *> geps;


    // find all gc atomic type allocations, analysis to get non-escaped set.
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
            // if it's a call instruction and has pl_atomic attribute
            if (auto *call = dyn_cast<CallInst>(&I))
            {
              if (auto *F = call->getCalledFunction())
              {
                auto attrs = call->getAttributes();
                if (!attrs.hasRetAttr("pl_atomic"))
                {
                  continue;
                }
                // if (!F->getName().startswith("DioGC__malloc"))
                // {
                //   continue;
                // }
                if (!PointerMayBeCaptured(call, true, true))
                {

                  auto info = attrs.getRetAttrs().getAttribute("pl_atomic").getValueAsString();
                  if (info.size() > 0 && this->escaped)
                  {
                    printf("variable moved to stack: %s\n", info.str().c_str());
                  }
                  
                  // if the pointer may be captured, then we change this gc malloc
                  // to stack alloca
                  
                  // the first argument of gc malloc is the size
                  auto *size = call->getArgOperand(0);
                  // size is a number, get it's value
                  auto *sizeValue = dyn_cast<ConstantInt>(size);
                  auto sizeInt = sizeValue->getZExtValue();
                  // get the size value

                  // replace it with alloca
                  builder.SetInsertPoint(&FV->front().front());
                  auto &alloca = *builder.CreateAlloca(ArrayType::get(IntegerType::get(M.getContext(), 8), sizeInt));
                  
                  // find all gep, replace address space with 0
                  auto users = call->users();
                  replace_geps(users, builder, &alloca, geps);


                  // find all memset, regenrate memset
                  for (auto *U : call->users())
                  {
                    if (auto *memset = dyn_cast<MemSetInst>(U))
                    {
                      auto *dest = memset->getDest();
                      auto *len = memset->getLength();
                      auto *value = memset->getValue();
                      builder.SetInsertPoint(memset);
                      auto *newmemset = builder.CreateMemSet(&alloca, value, len, memset->getDestAlign(), memset->isVolatile());
                      memset->replaceAllUsesWith(newmemset);
                      memset->eraseFromParent();
                    }
                  }
                  


                  call->replaceAllUsesWith(&alloca);
                  allocas.push_back(&alloca);
                  
                  
                  mallocs.push_back(call);

                }
                
              }
            }
          }
        }
      }
      
    }

    // There's a special case where `replaceAllUsesWith` won't cover:
    // Some LLVM pass would generate phi node, which may contians
    // non-escaped values. `replaceAllUsesWith` won't replace value after phi,
    // so we need to manually correct it.
    //
    // As phi is generated by pass, we can assume that the input values
    // must have same escape state.
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
    // delete previous nodes.
    for (auto *call : mallocs)
    {
      call->eraseFromParent();
    }
    for (auto *phi : phis)
    {
      if (phi->getParent())
      {
        phi->eraseFromParent();
      }
    }
    for (auto *gep : geps)
    {
      if (gep->getParent())
      {
        gep->eraseFromParent();
      }
    }
    return PreservedAnalyses::none();
  }

  void EscapePass::replace_geps(llvm::iterator_range<llvm::Value::user_iterator> &users, llvm::IRBuilder<> &builder, llvm::Value * ptr, std::__1::vector<llvm::GetElementPtrInst *> &geps)
  {
    for (auto *U : users)
    {
      if (auto *gep = dyn_cast<GetElementPtrInst>(U))
      {
        std::vector<Value *> arr;
        // Value *v = nullptr;
        for (unsigned i = 0; i < gep->getNumOperands(); ++i)
        {
          if (i == 0)
          {
            // first operand is the pointer, skip it. we only need index
            continue;
          }

          arr.push_back(gep->getOperand(i));
        }
        builder.SetInsertPoint(gep);
        auto *newgep = builder.CreateGEP(gep->getSourceElementType(), ptr, arr, gep->getName(), gep->isInBounds());
        auto newusers = gep->users();
        gep->replaceAllUsesWith(newgep);
        replace_geps(newusers, builder, newgep, geps);
        // gep->eraseFromParent();
        geps.push_back(gep);
      }
    }
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
