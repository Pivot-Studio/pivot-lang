#include "llvm-c/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm-c/Core.h"

using namespace llvm;

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
  // printf("sec name: %s\n", sectionName);
  auto mapName = "_GC_MAP_";
  if (strncmp(sectionName, mapName, strlen(mapName)) == 0)
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
extern "C"
{
  void CreatePLJITEngine(LLVMExecutionEngineRef *jit, LLVMModuleRef module, unsigned int opt, stackmap_cb cb)
  {
    finalize_cb = cb;
    auto mem_manager = LLVMCreateSimpleMCJITMemoryManager(new SectionMemoryManager(),
                                                          roundTripAllocateCodeSection,
                                                          roundTripAllocateDataSection,
                                                          roundTripFinalizeMemory,
                                                          roundTripDestroy);
    LLVMMCJITCompilerOptions Options;
    LLVMInitializeMCJITCompilerOptions(&Options, sizeof(Options));
    Options.MCJMM = mem_manager;
    Options.OptLevel = opt;

    char *Error;
    //      LLVMExecutionEngineRef jit;

    if (LLVMCreateMCJITCompilerForModule(jit, module, &Options,
                                         sizeof(Options), &Error))
    {
      printf("fatal: create mcjit engine error!\n");
      printf("%s\n", Error);
      exit(1945);
    }

    //      LLVMRunStaticConstructors(jit);
    //      auto f  = LLVMGetFunctionAddress(jit, "main");
    //      auto m = (mainf)f;
    //      auto ret = m();
    //      printf("ret = %d \n", ret);
  }
}