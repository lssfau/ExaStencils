CXX = g++

CXXFLAGS = -O3 -DNDEBUG -std=c++11 -fopenmp -I.
LDFLAGS = -fopenmp
LDLIBS =

ALL_OBJ = Communication/Communication_exchbath_all_a_a_a_a_3.o Communication/Communication_exchcetaLower0_all_a_a_a_a_3.o Communication/Communication_exchcetaUpper0_all_a_a_a_a_3.o Communication/Communication_exchcuLower0_all_a_a_a_a_3.o Communication/Communication_exchcuTildeLower0_all_a_a_a_a_3.o Communication/Communication_exchcuTildeUpper0_all_a_a_a_a_3.o Communication/Communication_exchcuUpper0_all_a_a_a_a_3.o Communication/Communication_exchcvLower0_all_a_a_a_a_3.o Communication/Communication_exchcvTildeLower0_all_a_a_a_a_3.o Communication/Communication_exchcvTildeUpper0_all_a_a_a_a_3.o Communication/Communication_exchcvUpper0_all_a_a_a_a_3.o Communication/Communication_exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3.o Communication/Communication_waitForFlag.o Domain/Domain_initDomain.o Domain/Domain_initGeometry.o Global/Global_declarations.o Global/Global_destroyGlobals.o Global/Global_initGlobals.o Global/Global_readParameterFile.o Global/Global_setupBuffers.o User/User_AdvanceRK2_3.o User/User_ApplyAllBc_3.o User/User_CheckGrid_3.o User/User_CommunicateAll_3.o User/User_InitBath_3.o User/User_InitFieldsAnalytical_3.o User/User_InitTriInfo_3.o User/User_MinDepth_3.o User/User_PrintDebugFields_3.o User/User_SetupPhase_3.o User/User_SolvePhase_3.o User/User_UpdateCTilde_3.o User/User_UpdateUnknowns_3.o User/User_initFieldsWithZero.o User/User_main.o Util/Timer_getTotalTime.o Util/Timer_printAllTimers.o Util/Timer_printAllTimersToFile.o Util/Timer_reduceTimers.o Util/Util_declarations.o Util/Util_readLine.o

BINARY = exastencils

.PHONY: all
all: ${BINARY}

.PHONY: clean
clean:
	rm -f ${BINARY} ${ALL_OBJ}

${BINARY}: ${ALL_OBJ}
	${CXX} -o ${BINARY} ${LDFLAGS} ${ALL_OBJ} ${LDLIBS}


# begin: C++ targets
Communication/Communication_exchbath_all_a_a_a_a_3.o : Communication/Communication_exchbath_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcetaLower0_all_a_a_a_a_3.o : Communication/Communication_exchcetaLower0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcetaUpper0_all_a_a_a_a_3.o : Communication/Communication_exchcetaUpper0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcuLower0_all_a_a_a_a_3.o : Communication/Communication_exchcuLower0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcuTildeLower0_all_a_a_a_a_3.o : Communication/Communication_exchcuTildeLower0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcuTildeUpper0_all_a_a_a_a_3.o : Communication/Communication_exchcuTildeUpper0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcuUpper0_all_a_a_a_a_3.o : Communication/Communication_exchcuUpper0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcvLower0_all_a_a_a_a_3.o : Communication/Communication_exchcvLower0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcvTildeLower0_all_a_a_a_a_3.o : Communication/Communication_exchcvTildeLower0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcvTildeUpper0_all_a_a_a_a_3.o : Communication/Communication_exchcvTildeUpper0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchcvUpper0_all_a_a_a_a_3.o : Communication/Communication_exchcvUpper0_all_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3.o : Communication/Communication_exchvf_nodePosition_ghost_a_a_a_a_a_a_a_a_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Communication/Communication_waitForFlag.o : Communication/Communication_waitForFlag.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Domain/Domain_initDomain.o : Domain/Domain_initDomain.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Domain/Domain_initGeometry.o : Domain/Domain_initGeometry.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Global/Global_declarations.o : Global/Global_declarations.cpp Global/Global.h Util/Stopwatch.h Util/Util.h
Global/Global_destroyGlobals.o : Global/Global_destroyGlobals.cpp Global/Global.h Util/Stopwatch.h Util/Util.h
Global/Global_initGlobals.o : Global/Global_initGlobals.cpp Global/Global.h Util/Stopwatch.h Util/Util.h
Global/Global_readParameterFile.o : Global/Global_readParameterFile.cpp Global/Global.h Util/Stopwatch.h Util/Util.h
Global/Global_setupBuffers.o : Global/Global_setupBuffers.cpp Global/Global.h Util/Stopwatch.h Util/Util.h
User/User_AdvanceRK2_3.o : User/User_AdvanceRK2_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_ApplyAllBc_3.o : User/User_ApplyAllBc_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_CheckGrid_3.o : User/User_CheckGrid_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_CommunicateAll_3.o : User/User_CommunicateAll_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_InitBath_3.o : User/User_InitBath_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_InitFieldsAnalytical_3.o : User/User_InitFieldsAnalytical_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_InitTriInfo_3.o : User/User_InitTriInfo_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_MinDepth_3.o : User/User_MinDepth_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_PrintDebugFields_3.o : User/User_PrintDebugFields_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_SetupPhase_3.o : User/User_SetupPhase_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_SolvePhase_3.o : User/User_SolvePhase_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_UpdateCTilde_3.o : User/User_UpdateCTilde_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_UpdateUnknowns_3.o : User/User_UpdateUnknowns_3.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_initFieldsWithZero.o : User/User_initFieldsWithZero.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
User/User_main.o : User/User_main.cpp Communication/Communication.h Domain/Domain.h Global/Global.h User/User.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Util/Timer_getTotalTime.o : Util/Timer_getTotalTime.cpp Global/Global.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Util/Timer_printAllTimers.o : Util/Timer_printAllTimers.cpp Global/Global.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Util/Timer_printAllTimersToFile.o : Util/Timer_printAllTimersToFile.cpp Global/Global.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Util/Timer_reduceTimers.o : Util/Timer_reduceTimers.cpp Global/Global.h Util/Stopwatch.h Util/Timer.h Util/Util.h
Util/Util_declarations.o : Util/Util_declarations.cpp Util/Util.h
Util/Util_readLine.o : Util/Util_readLine.cpp Util/Util.h
# end: C++ targets

# begin: additionalFiles targets
# end: additionalFiles targets

