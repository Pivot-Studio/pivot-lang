// #include "llvm/CodeGen/GCStrategy.h"
#include "llvm/IR/GCStrategy.h"
#include "llvm/IR/BuiltinGCs.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/Compiler.h"
#include "plimmixprinter.cpp"
#include "plimmix_pass.cpp"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include "llvm-c/Types.h"
#include "llvm-c/BitWriter.h"
#include "llvm-c/Transforms/PassBuilder.h"
#include "llvm/Transforms/Scalar/RewriteStatepointsForGC.h"
#include "stripRelocationPass.cpp"

namespace
{
    /*
        This is the GC strategy for immix.
        It is used to register our immix GC with LLVM.
    */
    class LLVM_LIBRARY_VISIBILITY PLImmixGC : public GCStrategy
    {
    public:
        PLImmixGC()
        {
            UsesMetadata = true;
            NeededSafePoints = true;
        }
    };

    GCRegistry::Add<PLImmixGC>
        X("plimmix", "pivot-lang immix garbage collector.");
}

extern "C" LLVMMemoryBufferRef parse_ir(char* ir)
{
    SMDiagnostic Err;
    auto Ctx = std::make_unique<LLVMContext>();
    auto sIr = std::string(ir);
    auto Mod = parseIRFile(sIr, Err, *Ctx);
    if (!Mod)
    {
        Err.print("immix", errs());
        return nullptr;
    }
    auto mb = LLVMWriteBitcodeToMemoryBuffer(wrap(Mod.release()));

    return mb;
}

extern "C" void LLVMLinkPLImmixGC()
{
    linkAllBuiltinGCs();
}
#include "llvm-c/Transforms/PassManagerBuilder.h"

extern "C" void add_module_pass(llvm::legacy::PassManagerBase *PB) {
    PB->add(new ImmixLegacy());

}
/*
    param: opt opt level

    The new LLVM Pass Manager does not have official C bindings yet.
    So we have to write one ourselves.
*/
extern "C" void run_module_pass(LLVMModuleRef  M, int opt, int debug) {
    // These must be declared in this order so that they are destroyed in the
    // correct order due to inter-analysis-manager references.
    LoopAnalysisManager LAM;
    FunctionAnalysisManager FAM;
    CGSCCAnalysisManager CGAM;
    ModuleAnalysisManager MAM;

    // Create the new pass manager builder.
    // Take a look at the PassBuilder constructor parameters for more
    // customization, e.g. specifying a TargetMachine or various debugging
    // options.
    PassBuilder PB;

    // Register all the basic analyses with the managers.
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    auto O = OptimizationLevel::O2;
    ModulePassManager MPM;
    switch (opt)
    {
    case 0:
        O = OptimizationLevel::O0;
        MPM = PB.buildO0DefaultPipeline(O);
        break;
    case 1:
        O = OptimizationLevel::O1;
        MPM = PB.buildPerModuleDefaultPipeline(O);
        break;
    case 2:
        O = OptimizationLevel::O2;
        MPM = PB.buildPerModuleDefaultPipeline(O);
        break;
    case 3:
        O = OptimizationLevel::O3;
        MPM = PB.buildPerModuleDefaultPipeline(O);
        break;
    default:
        MPM = PB.buildPerModuleDefaultPipeline(O);
        break;
    }

    MPM.addPass(EscapePass());
    // MPM.addPass(EscapeFinalPass());

    // Create the pass manager.
    // This one corresponds to a typical -O2 optimization pipeline.

    MPM.addPass(ImmixPass());
    MPM.addPass(RewriteStatepointsForGC());
    if (debug)
    {
        MPM.addPass(createModuleToFunctionPassAdaptor(PLStripGCRelocates()));
    }
    
    

    // Optimize the IR!
    MPM.run(*unwrap(M), MAM);


}


/*
    Shadow stack implementation for immix. (used in JIT mode, but may not work now)
*/
extern "C"
{
    /// The map for a single function's stack frame.  One of these is
    ///        compiled as constant data into the executable for each function.
    ///
    /// Storage of metadata values is elided if the %metadata parameter to
    /// @llvm.gcroot is null.
    struct FrameMap
    {
        int32_t NumRoots;    //< Number of roots in stack frame.
        int32_t NumMeta;     //< Number of metadata entries.  May be < NumRoots.
        const void *Meta[0]; //< Metadata for each root.
    };

    /// A link in the dynamic shadow stack.  One of these is embedded in
    ///        the stack frame of each function on the call stack.
    struct StackEntry
    {
        StackEntry *Next;    //< Link to next stack entry (the caller's).
        const FrameMap *Map; //< Pointer to constant FrameMap.
        void *Roots[0];      //< Stack roots (in-place array).
    };

    /// The head of the singly-linked list of StackEntries.  Functions push
    ///        and pop onto this in their prologue and epilogue.
    ///
    /// Since there is only a global list, this technique is not threadsafe.
    thread_local StackEntry **llvm_gc_root_chain;

    void SetShadowStackAddr(void *addr)
    {
        llvm_gc_root_chain = (StackEntry **)addr;
    }

    /// Calls Visitor(root, meta) for each GC root on the stack.
    ///        root and meta are exactly the values passed to
    ///        @llvm.gcroot.
    ///
    /// Visitor could be a function to recursively mark live objects.  Or it
    /// might copy them to another heap or generation.
    ///
    /// @param Visitor A function to invoke for every GC root on the stack.
    void visitGCRoots(void (*Visitor)(void **Root, const void *Meta))
    {
        for (StackEntry *R = *llvm_gc_root_chain; R; R = R->Next)
        {
            unsigned i = 0;

            // For roots [0, NumMeta), the metadata pointer is in the FrameMap.
            for (unsigned e = R->Map->NumMeta; i != e; ++i)
            {
                Visitor(&R->Roots[i], R->Map->Meta[i]);
            }

            // For roots [NumMeta, NumRoots), the metadata pointer is null.
            for (unsigned e = R->Map->NumRoots; i != e; ++i)
            {
                Visitor(&R->Roots[i], NULL);
            }
        }
    }
}