// lib/MyGC/MyGC.cpp - Example LLVM GC plugin

// #include "llvm/CodeGen/GCStrategy.h"
#include "llvm/IR/GCStrategy.h"
#include "llvm/IR/BuiltinGCs.h"
#include "llvm/Support/Compiler.h"
#include "plimmixprinter.cpp"

using namespace llvm;

namespace
{
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

extern "C" void LLVMLinkPLImmixGC()
{
    linkAllBuiltinGCs();
}

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