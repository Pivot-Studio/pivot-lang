/*
  LLVM memory manager for Pivot Lang
  We are using this to get immix working with JIT.
*/

#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Core.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/EPCDynamicLibrarySearchGenerator.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"


using namespace llvm;
using namespace orc;

static bool didCallAllocateCodeSection;

static SmallVector<uint8_t *, 10> stackMaps = {};
typedef void (*stackmap_cb)(uint8_t *map);
typedef int (*mainf)();
static stackmap_cb finalize_cb = nullptr;


static uint8_t *roundTripAllocateCodeSection(void *object, uintptr_t size,
                                             unsigned alignment,
                                             unsigned sectionID,
                                             const char *sectionName)
{

  didCallAllocateCodeSection = true;
  return static_cast<SectionMemoryManager *>(object)->allocateCodeSection(
      size, alignment, sectionID, sectionName);
}

static uint8_t *roundTripAllocateDataSection(void *object, uintptr_t size,
                                             unsigned alignment,
                                             unsigned sectionID,
                                             const char *sectionName,
                                             LLVMBool isReadOnly)
{
  auto alloc_addr = static_cast<SectionMemoryManager *>(object)->allocateDataSection(
      size, alignment, sectionID, sectionName, isReadOnly);
// the stackmap section on mac is `__llvm_stackmaps`, while on other platform is .llvm_stackmaps
// see https://llvm.org/docs/StackMaps.html#stack-map-section
#ifdef __APPLE__
  auto llvmSectionName = "__llvm_stackmaps";
#else
  auto llvmSectionName = ".llvm_stackmaps";
#endif
  if (strcmp(sectionName, llvmSectionName) == 0)
  {
    // printf("push back\n");
    stackMaps.push_back(alloc_addr);
  }
  return alloc_addr;
}

static LLVMBool roundTripFinalizeMemory(void *object, char **errMsg)
{
  std::string errMsgString;
  bool result =
      static_cast<SectionMemoryManager *>(object)->finalizeMemory(&errMsgString);
  if (result)
  {
    *errMsg = LLVMCreateMessage(errMsgString.c_str());
    return 1;
  }
  else
  {
    // initialize the stack map
    if (finalize_cb == nullptr)
    {
      *errMsg = LLVMCreateMessage(
          "stack map initialize function is not registered. Please register on by RegisterStackMapLoadCallback");
    }
    else
    {
      for (int i = 0; i < stackMaps.size(); ++i)
      {
        // printf("finalize\n");
        finalize_cb(stackMaps[i]);
      }
    }
  }
  return 0;
}
static void roundTripDestroy(void *object)
{
  delete static_cast<SectionMemoryManager *>(object);
}





// create pivot sectionmemory manager

class PivotSectionMemoryManager : public SectionMemoryManager
{
public:
  PivotSectionMemoryManager() {}

  ~PivotSectionMemoryManager() noexcept override {}

  uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                               unsigned SectionID,
                               StringRef SectionName,
                               bool IsReadOnly) override
  {
    auto alloc_addr = SectionMemoryManager::allocateDataSection(Size, Alignment, SectionID,
                                                     SectionName, IsReadOnly);
    // the stackmap section on mac is `__llvm_stackmaps`, while on other platform is .llvm_stackmaps
    // see https://llvm.org/docs/StackMaps.html#stack-map-section
  #ifdef __APPLE__
    auto llvmSectionName = "__llvm_stackmaps";
  #else
    auto llvmSectionName = ".llvm_stackmaps";
  #endif
    if (SectionName.equals(llvmSectionName))
    {
      // printf("push back\n");
      stackMaps.push_back(alloc_addr);
    }
    return alloc_addr;
  }

  uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                               unsigned SectionID,
                               StringRef SectionName) override
  {
    return SectionMemoryManager::allocateCodeSection(Size, Alignment, SectionID,
                                                     SectionName);
  }

  bool finalizeMemory(std::string *ErrMsg = nullptr) override
  {
    auto result = SectionMemoryManager::finalizeMemory(ErrMsg);
    if (result)
    {
      return 1;
    }
    else
    {
      // initialize the stack map
      if (finalize_cb == nullptr)
      {
        *ErrMsg = LLVMCreateMessage(
            "stack map initialize function is not registered. Please register on by RegisterStackMapLoadCallback");
      }
      else
      {
        for (int i = 0; i < stackMaps.size(); ++i)
        {
          // printf("finalize\n");
          finalize_cb(stackMaps[i]);
        }
      }
    }
    return 0;
  }
};
