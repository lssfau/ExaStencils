package exastencils.waLBerla.ir.util

import exastencils.config.Knowledge
import exastencils.config.Platform

object IR_WaLBerlaPreprocessorDirectives {

  val headerTop : String = """#ifdef __GNUC__
                             |#define RESTRICT __restrict__
                             |#elif _MSC_VER
                             |#define RESTRICT __restrict
                             |#else
                             |#define RESTRICT
                             |#endif
                             |
                             |#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                             |#   pragma GCC diagnostic push
                             |#   pragma GCC diagnostic ignored "-Wunused-parameter"
                             |#   pragma GCC diagnostic ignored "-Wreorder"
                             |#endif
                             |""".stripMargin

  val headerBottom : String = """#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                                |#   pragma GCC diagnostic pop
                                |#endif
                                |""".stripMargin

  val sourceTop : String = s"""
                              | ${if (Knowledge.cuda_enabled) "#define FUNC_PREFIX __global__" else if (Platform.targetHardware == "CPU") "#define FUNC_PREFIX"}
                              |
                              |#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                              |#   pragma GCC diagnostic push
                              |#   pragma GCC diagnostic ignored "-Wfloat-equal"
                              |#   pragma GCC diagnostic ignored "-Wshadow"
                              |#   pragma GCC diagnostic ignored "-Wconversion"
                              |#   pragma GCC diagnostic ignored "-Wunused-variable"
                              |#endif
                              |
                              |#if ( defined WALBERLA_CXX_COMPILER_IS_INTEL )
                              |#pragma warning push
                              |#pragma warning( disable :  1599 )
                              |#endif
                              |
                              |""".stripMargin

  val sourceBottom : String = """#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                                |#   pragma GCC diagnostic pop
                                |#endif
                                |
                                |#if ( defined WALBERLA_CXX_COMPILER_IS_INTEL )
                                |#pragma warning pop
                                |#endif
                                |""".stripMargin
}
