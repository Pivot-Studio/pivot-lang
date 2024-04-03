# Set the minimum version of CMake that can be used
# To find the cmake version run
# $ cmake --version
cmake_minimum_required(VERSION 3.5)
# Set the project name
project (plimmix)

find_package(LLVM REQUIRED CONFIG)

# check version is 18
if (LLVM_VERSION_MAJOR EQUAL 18)
  message(STATUS "Found LLVM 18")
else()
  message(FATAL_ERROR "LLVM version is not 18")
endif()

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
include_directories(${LLVM_INCLUDE_DIRS})


if (WIN32)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++17")
else()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
endif()

set(SOURCES
  memory_manager.cpp
  plimmix_pass.cpp
  plimmix.cpp
  plimmixprinter.cpp
)
set(CMAKE_INSTALL_PREFIX .)

add_library(plimmix_plugin_shared SHARED ${SOURCES})
install(TARGETS plimmix_plugin_shared
    DESTINATION ${CMAKE_INSTALL_PREFIX}
)
add_library(plimmix_plugin STATIC ${SOURCES})
install(TARGETS plimmix_plugin
    DESTINATION ${CMAKE_INSTALL_PREFIX}
)

if (NOT LLVM_ENABLE_RTTI)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")
endif()

llvm_map_components_to_libnames(
    llvm_libs
    Analysis
    Core
    ExecutionEngine
    InstCombine
    Object
    OrcJIT
    RuntimeDyld
    ScalarOpts
    Support
    TransformUtils
    native
    irreader
)

message(STATUS "LLVM_LIBS: ${llvm_libs}")
target_link_libraries(plimmix_plugin_shared plimmix_plugin ${llvm_libs})
