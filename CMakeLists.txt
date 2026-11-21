# =============================================================================
# expander2.haskell - CMake front end for GHC (no C/C++ toolchain involved)
#
#   cmake -B build-cmake                       # 默认 profile = extreme（最强劲）
#   cmake --build build-cmake
#   cmake --build build-cmake --target shell   # 交互壳子
#   cmake -B build-cmake -DEXPANDER2_PROFILE=strong   # 降档
#
# Optimisation flags and the GHC location are read from ghc-flags.mk, the same
# file used by build.ps1 and the Makefile. No -march=native / -mcpu=native.
# =============================================================================
cmake_minimum_required(VERSION 3.16)

project(expander2_hs LANGUAGES NONE)

# ---- flags: single source of truth -----------------------------------------
set(_mk "${CMAKE_CURRENT_SOURCE_DIR}/ghc-flags.mk")
file(STRINGS "${_mk}" _mk_lines)
set(DEFAULT_PROFILE "extreme")
set(GHC_HOME "")
foreach(_line IN LISTS _mk_lines)
  if(_line MATCHES "^PROFILE_(FAST|STRONG|EXTREME|COMPAT)[ \t]*:=[ \t]*(.*)$")
    set("PROFILE_${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
  elseif(_line MATCHES "^DEFAULT_PROFILE[ \t]*:=[ \t]*(.*)$")
    set(DEFAULT_PROFILE "${CMAKE_MATCH_1}")
  elseif(_line MATCHES "^GHC_HOME[ \t]*:=[ \t]*(.*)$")
    set(GHC_HOME "${CMAKE_MATCH_1}")
  endif()
endforeach()
string(TOUPPER "${DEFAULT_PROFILE}" _default_upper)

# ---- options ---------------------------------------------------------------
set(EXPANDER2_PROFILE "${_default_upper}" CACHE STRING "fast | strong | extreme | compat")
set_property(CACHE EXPANDER2_PROFILE PROPERTY STRINGS fast strong extreme compat)
set(EXPANDER2_LLVM "auto" CACHE STRING "auto | on | off")
set_property(CACHE EXPANDER2_LLVM PROPERTY STRINGS auto on off)
set(EXPANDER2_RTSOPTS "-A64m" CACHE STRING "default RTS options (-with-rtsopts)")
set(EXPANDER2_JOBS "auto" CACHE STRING "ghc -j<N>; 0 = no -j, auto = cores-1")
set(EXPANDER2_GHC "" CACHE FILEPATH "explicit path to the ghc executable")
set(EXPANDER2_OUT_DIR "" CACHE PATH "executable output dir; default <build>/bin")

# ---- toolchain -------------------------------------------------------------
if(EXPANDER2_GHC)
  set(GHC_EXECUTABLE "${EXPANDER2_GHC}")
elseif(WIN32 AND GHC_HOME AND EXISTS "${GHC_HOME}/ghc.exe")
  set(GHC_EXECUTABLE "${GHC_HOME}/ghc.exe")
elseif(GHC_HOME AND EXISTS "${GHC_HOME}/ghc")
  set(GHC_EXECUTABLE "${GHC_HOME}/ghc")
else()
  find_program(GHC_EXECUTABLE NAMES ghc ghc.exe)
endif()
if(NOT GHC_EXECUTABLE)
  message(FATAL_ERROR "ghc not found (GHC_HOME=${GHC_HOME}); pass -DEXPANDER2_GHC=/path/to/ghc")
endif()
execute_process(COMMAND "${GHC_EXECUTABLE}" --version
                OUTPUT_VARIABLE _ghc_version OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "GHC: ${_ghc_version}  (${GHC_EXECUTABLE})")

# ---- profile ---------------------------------------------------------------
string(TOUPPER "${EXPANDER2_PROFILE}" _pname)
if(NOT DEFINED PROFILE_${_pname})
  message(FATAL_ERROR "unknown EXPANDER2_PROFILE='${EXPANDER2_PROFILE}' (fast|strong|extreme|compat)")
endif()

set(_profile_flags "${PROFILE_${_pname}}")
separate_arguments(_profile_flag_list UNIX_COMMAND "${_profile_flags}")

if("${_profile_flags}" MATCHES "(march|mcpu)=native")
  message(FATAL_ERROR "native tuning is disabled for this project (${_profile_flags})")
endif()

# ---- LLVM backend ----------------------------------------------------------
set(_llvm_flags "")
if(EXPANDER2_LLVM STREQUAL "on")
  find_program(LLC_EXECUTABLE NAMES llc)
  find_program(OPT_EXECUTABLE NAMES opt)
  if(NOT (LLC_EXECUTABLE AND OPT_EXECUTABLE))
    message(FATAL_ERROR "EXPANDER2_LLVM=on requires llc and opt in PATH")
  endif()
  set(_llvm_flags -fllvm -optlo=-O3 -optlc=-O3)
  message(STATUS "LLVM backend: on (forced)")
elseif(EXPANDER2_LLVM STREQUAL "auto")
  find_program(LLC_EXECUTABLE NAMES llc)
  find_program(OPT_EXECUTABLE NAMES opt)
  if(LLC_EXECUTABLE AND OPT_EXECUTABLE)
    set(_llvm_flags -fllvm -optlo=-O3 -optlc=-O3)
    message(STATUS "LLVM backend: on (llc + opt found)")
  else()
    message(STATUS "LLVM backend: off (llc/opt not found, using NCG)")
  endif()
elseif(EXPANDER2_LLVM STREQUAL "off")
  message(STATUS "LLVM backend: off (forced)")
else()
  message(FATAL_ERROR "EXPANDER2_LLVM must be auto|on|off, got '${EXPANDER2_LLVM}'")
endif()

# ---- base flags ------------------------------------------------------------
# 兼容模式：只用 GHC 内置 NCG 后端，LLVM 一律关闭（不依赖 llc/opt 与外部工具链）
if(EXPANDER2_PROFILE STREQUAL "compat" AND NOT _llvm_flags STREQUAL "")
  message(STATUS "compat profile -> GHC NCG backend only (LLVM forced off)")
  set(_llvm_flags "")
endif()

set(_base_flags -isrc -iapp -rtsopts)
if(NOT EXPANDER2_RTSOPTS STREQUAL "")
  list(APPEND _base_flags "-with-rtsopts=${EXPANDER2_RTSOPTS}")
endif()

# 并行度：默认 cores-1，且每个 job 预留约 1.5 GB 内存（与 build.ps1 同一口径）；
# -j 不影响 .hi/.o 内容，因此不参与下面的 flags 比较
set(_jobs "${EXPANDER2_JOBS}")
if(_jobs STREQUAL "auto")
  cmake_host_system_information(RESULT _nproc QUERY NUMBER_OF_LOGICAL_CORES)
  cmake_host_system_information(RESULT _mem_mb QUERY TOTAL_PHYSICAL_MEMORY)
  math(EXPR _by_ram "${_mem_mb} / 1536")
  math(EXPR _jobs "${_nproc} - 1")
  if(_by_ram LESS 1)
    set(_by_ram 1)
  endif()
  if(_by_ram LESS _jobs)
    set(_jobs "${_by_ram}")
  endif()
  if(_jobs LESS 1)
    set(_jobs 1)
  endif()
endif()
if(NOT _jobs STREQUAL "0")
  list(APPEND _base_flags "-j${_jobs}")
endif()
list(APPEND _base_flags --make)

set(GHC_FLAGS ${_profile_flag_list} ${_llvm_flags} ${_base_flags})
message(STATUS "GHC flags: ${GHC_FLAGS}")

# ---- output layout ---------------------------------------------------------
set(OBJ_MAIN "${CMAKE_BINARY_DIR}/obj-main")
set(OBJ_SHELL "${CMAKE_BINARY_DIR}/obj-shell")
if(EXPANDER2_OUT_DIR)
  set(BIN_DIR "${EXPANDER2_OUT_DIR}")
else()
  set(BIN_DIR "${CMAKE_BINARY_DIR}/bin")
endif()
if(WIN32)
  set(EXE_SUFFIX ".exe")
else()
  set(EXE_SUFFIX "")
endif()

# Reproducibility: .hi/.o are flag-sensitive. If the optimisation flag string
# changed since the last configure, drop the object dirs (and stale executables)
# so GHC never links objects produced with a different optimisation set.
string(REPLACE ";" " " _flags_str "${_profile_flag_list} ${_llvm_flags}")
if(DEFINED EXPANDER2_LAST_FLAGS AND NOT EXPANDER2_LAST_FLAGS STREQUAL _flags_str)
  message(STATUS "GHC flags changed -> dropping stale objects")
  file(REMOVE_RECURSE "${OBJ_MAIN}" "${OBJ_SHELL}")
  file(REMOVE "${BIN_DIR}/expander2-hs${EXE_SUFFIX}" "${BIN_DIR}/expander2-shell${EXE_SUFFIX}")
endif()
set(EXPANDER2_LAST_FLAGS "${_flags_str}" CACHE INTERNAL "flags used at last configure" FORCE)

file(GLOB_RECURSE EXPANDER2_SRC CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/src/*.hs")

# ---- targets ---------------------------------------------------------------
add_custom_command(
  OUTPUT "${BIN_DIR}/expander2-hs${EXE_SUFFIX}"
  COMMAND "${CMAKE_COMMAND}" -E make_directory "${BIN_DIR}" "${OBJ_MAIN}"
  COMMAND "${GHC_EXECUTABLE}" ${GHC_FLAGS}
          -outputdir "${OBJ_MAIN}"
          -o "${BIN_DIR}/expander2-hs${EXE_SUFFIX}"
          "${CMAKE_CURRENT_SOURCE_DIR}/app/Main.hs"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS ${EXPANDER2_SRC} "${CMAKE_CURRENT_SOURCE_DIR}/app/Main.hs" "${_mk}"
  COMMENT "ghc: building expander2-hs (profile=${EXPANDER2_PROFILE})"
  VERBATIM)

add_custom_target(expander2-hs ALL
  DEPENDS "${BIN_DIR}/expander2-hs${EXE_SUFFIX}")

add_custom_command(
  OUTPUT "${BIN_DIR}/expander2-shell${EXE_SUFFIX}"
  COMMAND "${CMAKE_COMMAND}" -E make_directory "${BIN_DIR}" "${OBJ_SHELL}"
  COMMAND "${GHC_EXECUTABLE}" ${GHC_FLAGS}
          -outputdir "${OBJ_SHELL}"
          -o "${BIN_DIR}/expander2-shell${EXE_SUFFIX}"
          "${CMAKE_CURRENT_SOURCE_DIR}/app/Shell.hs"
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  DEPENDS ${EXPANDER2_SRC} "${CMAKE_CURRENT_SOURCE_DIR}/app/Shell.hs" "${_mk}"
  COMMENT "ghc: building expander2-shell (profile=${EXPANDER2_PROFILE})"
  VERBATIM)

add_custom_target(shell DEPENDS "${BIN_DIR}/expander2-shell${EXE_SUFFIX}")

add_custom_target(info
  COMMAND "${CMAKE_COMMAND}" -E echo "profile=${EXPANDER2_PROFILE} llvm=${EXPANDER2_LLVM} jobs=${_jobs} out=${BIN_DIR}"
  COMMAND "${CMAKE_COMMAND}" -E echo "flags=${GHC_FLAGS}"
  COMMAND "${GHC_EXECUTABLE}" --version
  VERBATIM)

add_custom_target(clean-all
  COMMAND "${CMAKE_COMMAND}" -E remove_directory "${OBJ_MAIN}"
  COMMAND "${CMAKE_COMMAND}" -E remove_directory "${OBJ_SHELL}"
  VERBATIM)

foreach(_tgt expander2-hs expander2-shell)
  install(PROGRAMS "${BIN_DIR}/${_tgt}${EXE_SUFFIX}" DESTINATION bin)
endforeach()
