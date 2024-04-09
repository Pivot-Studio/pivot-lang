/*
  LLVM memory manager for Pivot Lang
  We are using this to get immix working with JIT.
*/

#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Core.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IRReader/IRReader.h"


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


class PivotJIT {
private:
  std::unique_ptr<ExecutionSession> ES;

  DataLayout DL;
  MangleAndInterner Mangle;

  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;

  JITDylib &MainJD;

public:
  PivotJIT(std::unique_ptr<ExecutionSession> ES,
                  JITTargetMachineBuilder JTMB, DataLayout DL)
      : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
        ObjectLayer(*this->ES,
                    []() { return std::make_unique<PivotSectionMemoryManager>(); }),
        CompileLayer(*this->ES, ObjectLayer,
                     std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
        MainJD(this->ES->createBareJITDylib("<main>")) {
    MainJD.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
  }

  ~PivotJIT() {
    if (auto Err = ES->endSession())
      ES->reportError(std::move(Err));
  }

  static Expected<std::unique_ptr<PivotJIT>> Create() {
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
      return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError();

    return std::make_unique<PivotJIT>(std::move(ES), std::move(JTMB),
                                             std::move(*DL));
  }

  const DataLayout &getDataLayout() const { return DL; }

  JITDylib &getMainJITDylib() { return MainJD; }

  Error addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr) {
    if (!RT)
      RT = MainJD.getDefaultResourceTracker();
    return CompileLayer.add(RT, std::move(TSM));
  }

  Expected<ExecutorSymbolDef> lookup(StringRef Name) {
    auto addr = llvm::sys::DynamicLibrary::SearchForAddressOfSymbol(Name.str().c_str());
    if (addr)
    {
      return ExecutorSymbolDef(ExecutorAddr::fromPtr(addr), JITSymbolFlags::Exported);
    }
    
    return ES->lookup({&MainJD}, Mangle(Name.str()));
  }
};

static ExitOnError ExitOnErr;

#include "llvm/Support/SourceMgr.h"
#include "llvm/Bitcode/BitcodeReader.h"
extern "C"
{

  int CreateAndRunPLJITEngine( LLVMModuleRef module, unsigned int opt, stackmap_cb cb)
  {
    LLVMExecutionEngineRef *jit = new LLVMExecutionEngineRef();
    finalize_cb = cb;
    auto sm = new SectionMemoryManager();
    auto mem_manager = LLVMCreateSimpleMCJITMemoryManager(sm,
                                                          roundTripAllocateCodeSection,
                                                          roundTripAllocateDataSection,
                                                          roundTripFinalizeMemory,
                                                          roundTripDestroy);
    LLVMMCJITCompilerOptions Options;
    LLVMInitializeMCJITCompilerOptions(&Options, sizeof(Options));
    Options.MCJMM = mem_manager;
    Options.OptLevel = opt;
    Options.NoFramePointerElim = 1;

    char *Error;
    //      LLVMExecutionEngineRef jit;

    if (LLVMCreateMCJITCompilerForModule(jit, module, &Options,
                                         sizeof(Options), &Error))
    {
      printf("fatal: create mcjit engine error!\n");
      printf("%s\n", Error);
      exit(1945);
    }
    auto f = LLVMGetFunctionAddress(*jit, "main");
    auto m = (mainf)f;
    auto ret = m();
    LLVMDisposeExecutionEngine(*jit);
    delete jit;
    return ret;
  }

  int CreateAndRunPLOrcJITEngine(char * module_path, unsigned int opt, stackmap_cb cb)
  {
    finalize_cb = cb;
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
    // llvm::sys::DynamicLibrary::LoadLibraryPermanently("/Users/bobli/src/pivot-lang/target/debug/libvm.dylib", nullptr); 
    auto jit = ExitOnErr(PivotJIT::Create());
    auto finit = ExitOnErr(jit->lookup("immix_gc_init"));
    auto finitf = finit.getAddress().toPtr<stackmap_cb>();
    // finalize_cb = finitf;

    auto RT = jit->getMainJITDylib().createResourceTracker();
    SMDiagnostic E;
    auto Ctx = std::make_unique<LLVMContext>();
    StringRef modPath(module_path);
    std::unique_ptr<llvm::Module> Mod;
    if (modPath.ends_with(".ll"))
    {
      Mod = parseIRFile(modPath, E, *Ctx);
      if (!Mod)
      {
        printf("fatal: parse ir file error!\n");
        exit(1945);
      }
    }
    else if (modPath.ends_with(".bc"))
    {
      auto Buf = ExitOnErr(errorOrToExpected(MemoryBuffer::getFile(modPath)));
      Mod = ExitOnErr(parseBitcodeFile(*Buf, *Ctx));
      if (!Mod)
      {
        printf("fatal: parse bitcode file error!\n");
        exit(1945);
      }
    }
    else
    {
      printf("fatal: unknown file type!\n");
      exit(1945);
    }

    
    
    auto TSM = ThreadSafeModule(std::move(Mod),std::move(Ctx));
    ExitOnErr(jit->addModule(std::move(TSM), RT));
    auto f = ExitOnErr(jit->lookup("main"));
    auto m = f.getAddress().toPtr<mainf>();
    auto ret = m();
    return ret;

  }
}