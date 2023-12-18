//===- StripGCRelocates.cpp - Remove gc.relocates inserted by RewriteStatePoints===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This is a little utility pass that removes the gc.relocates inserted by
// RewriteStatepointsForGC. Note that the generated IR is incorrect,
// but this is useful as a single pass in itself, for analysis of IR, without
// the GC.relocates. The statepoint and gc.result intrinsics would still be
// present.
//===----------------------------------------------------------------------===//
//
// 这个Pass从LLVM项目中的StripGCRelocates改造而来，相比原pass，它还原了relocate的指针，
// 但是没有移除relocate指令，因为这样会导致无法生成正确的stackmap
// 此Pass在debug模式下使用，需要禁用GC的evacuation功能才能保证程序的行为正确。
// 如果在启用了evacuation的情况下使用此Pass，会导致Undefined Behavior。
//===----------------------------------------------------------------------===//

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Statepoint.h"
#include "llvm/IR/DIBuilder.h"

using namespace llvm;

namespace llvm {

class Function;

class PLStripGCRelocates : public PassInfoMixin<PLStripGCRelocates> {
public:
  PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

} // end namespace llvm


static bool stripGCRelocates(Function &F) {
  std::unique_ptr<DIBuilder> DBuilder;
  // Nothing to do for declarations.
  if (F.isDeclaration())
    return false;
  SmallVector<GCRelocateInst *, 20> GCRelocates;
  // TODO: We currently do not handle gc.relocates that are in landing pads,
  // i.e. not bound to a single statepoint token.
  for (Instruction &I : instructions(F)) {
    if (auto *GCR = dyn_cast<GCRelocateInst>(&I))
      if (isa<GCStatepointInst>(GCR->getOperand(0)))
        GCRelocates.push_back(GCR);
  }
  // All gc.relocates are bound to a single statepoint token. The order of
  // visiting gc.relocates for deletion does not matter.
  for (GCRelocateInst *GCRel : GCRelocates) {
    Value *OrigPtr = GCRel->getDerivedPtr();
    Value *ReplaceGCRel = OrigPtr;

    // All gc_relocates are i8 addrspace(1)* typed, we need a bitcast from i8
    // addrspace(1)* to the type of the OrigPtr, if the are not the same.
    if (GCRel->getType() != OrigPtr->getType())
      ReplaceGCRel = new BitCastInst(OrigPtr, GCRel->getType(), "cast", GCRel);

    // Replace all uses of gc.relocate and delete the gc.relocate
    // There maybe unncessary bitcasts back to the OrigPtr type, an instcombine
    // pass would clear this up.
    GCRel->replaceAllUsesWith(ReplaceGCRel);
    // 不去掉relocate指令，如果去掉了stackmap里的location会变成空的
    // GCRel->eraseFromParent();
  }
  return !GCRelocates.empty();
}

PreservedAnalyses PLStripGCRelocates::run(Function &F,
                                        FunctionAnalysisManager &AM) {
  if (!stripGCRelocates(F))
    return PreservedAnalyses::all();

  // Removing gc.relocate preserves the CFG, but most other analysis probably
  // need to re-run.
  PreservedAnalyses PA;
  PA.preserveSet<CFGAnalyses>();
  return PA;
}
