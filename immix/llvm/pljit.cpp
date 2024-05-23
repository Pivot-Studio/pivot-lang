#include "memory_manager.cpp"


class PivotJIT {
private:
  std::unique_ptr<ExecutionSession> ES;

  DataLayout DL;
  MangleAndInterner Mangle;

  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;
  IRTransformLayer TransformLayer;

  JITDylib &MainJD;

  static Expected<ThreadSafeModule>
optimizeModule(ThreadSafeModule M, MaterializationResponsibility &R) {
  // Create a function pass manager.
  M.withModuleDo([](Module &M) {
    auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);

    // Add some optimizations.
    FPM->add(createInstructionCombiningPass());
    FPM->add(createReassociatePass());
    // FPM->add(createGVNPass());
    FPM->add(createCFGSimplificationPass());
    FPM->doInitialization();

    // Run the optimizations over all functions in the module being added to
    // the JIT.
    for (auto &F : M)
      FPM->run(F);
  });

  return M;
}

public:
  PivotJIT(std::unique_ptr<ExecutionSession> ES,
                  JITTargetMachineBuilder JTMB, DataLayout DL)
      : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
        ObjectLayer(*this->ES,
                    []() { return std::make_unique<PivotSectionMemoryManager>(); }),
        CompileLayer(*this->ES, ObjectLayer,
                     std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
        TransformLayer(*this->ES, CompileLayer, optimizeModule),
        MainJD(this->ES->createBareJITDylib("<main>")) {
    MainJD.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
    if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
      ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
      ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
    }
  }

  ~PivotJIT() {
    if (auto Err = ES->endSession())
      ES->reportError(std::move(Err));
  }

  // mangle
  SymbolStringPtr mangle(StringRef Name) {
    return Mangle(Name);
  }

  Expected<JITDylib &> loadPlatformDynamicLibrary(const char *Path) {
    auto G = EPCDynamicLibrarySearchGenerator::Load(*ES, Path);
    if (!G)
      return G.takeError();

    if (auto *ExistingJD = ES->getJITDylibByName(Path))
      return *ExistingJD;

    auto &JD = ES->createBareJITDylib(Path);
    JD.addGenerator(std::move(*G));
    return JD;
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
    return TransformLayer.add(RT, std::move(TSM));
  }

  Expected<ExecutorSymbolDef> lookup(StringRef Name) {
    return ES->lookup({&MainJD}, Mangle(Name.str()));
  }
};

static std::string getLibvmPath() {
    auto root = std::string(std::getenv("PL_ROOT"));
    #ifdef __APPLE__
    auto lib_path = "libvm.dylib";
    #elif __linux__
    auto lib_path = "libvm.so";
    #elif _WIN32
    auto lib_path = "vm.dll";
    #endif
    // join path
    std::string lib_full_path;
    if (StringRef(root).ends_with("/"))
    {
      lib_full_path = root + lib_path;
    }
    else
    {
      lib_full_path = root + "/" + lib_path;
    }
    return lib_full_path;
}

static ExitOnError ExitOnErr;
static std::unique_ptr<PivotJIT> TheJIT;
static std::map<std::string, ResourceTrackerSP> ResourceMap;
static std::unique_ptr<ThreadSafeModule> TMod;
static std::vector<std::string *> InitFns;
static int REPLCounter = 0;
#include "llvm/Support/SourceMgr.h"
#include "llvm/Bitcode/BitcodeReader.h"
extern "C"
{

  int CreateAndRunPLJITEngine( LLVMModuleRef module, unsigned int opt, stackmap_cb cb)
  {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    std::string lib_full_path = getLibvmPath();
    std::string errMsgString;
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(lib_full_path.c_str(), &errMsgString);
    printf("load libvm: %s\n", errMsgString.c_str()); 
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
    std::string lib_full_path = getLibvmPath();
    



    finalize_cb = cb;
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    
    // llvm::sys::DynamicLibrary::LoadLibraryPermanently(lib_full_path.c_str()); 
    auto jit = ExitOnErr(PivotJIT::Create());

    auto libvm = jit->loadPlatformDynamicLibrary(lib_full_path.c_str());
    if (!libvm)
    {
      printf("fatal: load libvm error!\n");
      exit(1945);
    }
    jit->getMainJITDylib().addToLinkOrder(*libvm);
    auto finit = ExitOnErr(jit->lookup("immix_gc_init"));
    auto finitf = finit.getAddress().toPtr<stackmap_cb>();
    finalize_cb = finitf;

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

  void CreateGlobalOrcJITEngine()
  {
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();
    std::string lib_full_path = getLibvmPath();
    // llvm::sys::DynamicLibrary::LoadLibraryPermanently(lib_full_path.c_str()); 
    auto jit = ExitOnErr(PivotJIT::Create());

    auto libvm = jit->loadPlatformDynamicLibrary(lib_full_path.c_str());
    if (!libvm)
    {
      printf("fatal: load libvm error!\n");
      exit(1945);
    }
    jit->getMainJITDylib().addToLinkOrder(*libvm);
    auto finit = ExitOnErr(jit->lookup("immix_gc_init"));
    auto finitf = finit.getAddress().toPtr<stackmap_cb>();
    finalize_cb = finitf;

    auto Ctx = std::make_unique<LLVMContext>();
    // create a new module
    auto Mod = std::make_unique<llvm::Module>("pivot", *Ctx);
    TMod =  std::make_unique<ThreadSafeModule>(std::move(Mod),std::move(Ctx));
    ExitOnErr(jit->addModule(std::move(*TMod)));
    TheJIT = std::move(jit);
  }

  void AddModuleToOrcJIT(LLVMModuleRef module)
  {
    auto RT = TheJIT->getMainJITDylib().createResourceTracker();
    std::unique_ptr<Module> mod;
    mod.reset(unwrap(module));
    // if resource tracker exists, remove it
    auto ModId = mod->getModuleIdentifier();
    if (ResourceMap.find(ModId) != ResourceMap.end())
    {
      // printf("Removing module from JIT %s\n", mod->getModuleIdentifier().c_str());
      auto RTOld = ResourceMap[ModId];
      ExitOnErr(RTOld->remove());
    }
    // mod->print(errs(), nullptr);
    // printf("Adding module to JIT %s\n", mod->getModuleIdentifier().c_str());
    auto FNS = mod->functions();
    // find ends with "..__init_global"
    auto init_global = std::find_if(FNS.begin(), FNS.end(), [](Function &F) {
      return F.getName().endswith("..__init_global");
    });
    InitFns.push_back(new std::string(init_global->getName()));
    auto ctx = std::make_unique<LLVMContext>();
    auto TSM = ThreadSafeModule(std::move(mod),std::move(ctx));
    ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
    ResourceMap[ModId] = RT;
    // // run init global
    // auto ExprSymbol = ExitOnErr(TheJIT->lookup(init_global->getName()));
    // void (*FP)() = ExprSymbol.getAddress().toPtr<void (*)()>();
    // FP();
  }

  void RunExpr(LLVMModuleRef module)
  {
      // Create a ResourceTracker to track JIT'd memory allocated to our
      // anonymous expression -- that way we can free it after executing.
      auto RT = TheJIT->getMainJITDylib().createResourceTracker();

      std::unique_ptr<Module> mod;
      mod.reset(unwrap(module));
      auto FNS = mod->functions();
      auto init_global = std::find_if(FNS.begin(), FNS.end(), [](Function &F) {
        return F.getName().endswith("..__init_global");
      });
      auto ModId = mod->getModuleIdentifier();
      // add REPLCounter
      auto ModIdStr = ModId + std::to_string(REPLCounter);
      mod->setModuleIdentifier(ModIdStr);


      #if _WIN32
      auto init_name = "@__repl__\\__anon__.pi..__init_global" + std::to_string(REPLCounter);
      #else
      auto init_name = "@__repl__/__anon__.pi..__init_global" + std::to_string(REPLCounter);
      #endif
      init_global->setName(init_name);
      // init_global->print(errs());
      InitFns.push_back(new std::string(init_global->getName()));
      auto ctx = std::make_unique<LLVMContext>();
      auto TSM = ThreadSafeModule(std::move(mod),std::move(ctx));
      ExitOnErr(TheJIT->addModule(std::move(TSM), RT));
      // "@__repl__/main.pi..__anon__{}" REPLCounter
      #if _WIN32
      auto name = "@__repl__\\__anon__.pi..__anon__" + std::to_string(REPLCounter);
      #else
      auto name = "@__repl__/__anon__.pi..__anon__" + std::to_string(REPLCounter);
      #endif
      auto ExprSymbol = ExitOnErr(TheJIT->lookup(name));
      REPLCounter++;

      // pop all init fns, run them
      for (auto &fn : InitFns)
      {
        auto ExprSymbol = ExitOnErr(TheJIT->lookup(*fn));
        void (*FP)() = ExprSymbol.getAddress().toPtr<void (*)()>();
        FP();
        delete fn;
      }
      InitFns.clear();

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      void (*FP)() = ExprSymbol.getAddress().toPtr<void (*)()>();
      // printf("Running expression...%p\n", FP);
      FP();

      // // Delete the anonymous expression module from the JIT.
      // ExitOnErr(RT->remove());
      
  }


}