class Configuration:
    baseName = "SISC_3D_VC"

    dimensionality = 3

    num_points_per_dim = 1024
    max_num_blocks_per_dim = 1024
    max_num_frags_per_block_per_dim = 64
    max_fragment_length_per_dim = 64

    def get_num_nodes(self):
        return (self.get_num_mpi() * self.get_num_omp()) / 64

    def get_rpn(self):
        return 64 / self.get_num_omp()

    def get_num_mpi(self):
        return self.get_value("mpi_numThreads", 1)

    def get_num_omp(self):
        return self.get_value("omp_numThreads", 1)

    def get_value(self, key, def_val):
        if key in self.constParameters:
            return self.constParameters[key]
        elif key in self.derivedParameters:
            return self.derivedParameters[key]
        elif key in self.chosenRangedParameters:
            return self.chosenRangedParameters[key]
        else:
            return self.chosenListedParameters.get(key, def_val)

    def set_value(self, key, new_val):
        if key in self.derivedParameters:
            self.derivedParameters[key] = new_val
        if key in self.chosenRangedParameters:
            self.chosenRangedParameters[key] = new_val
        if key in self.chosenListedParameters:
            self.chosenListedParameters[key] = new_val

    def update(self):
        self.set_value("domain_rect_numBlocks_x",
                       (self.num_points_per_dim /
                        (self.get_value("domain_fragmentLength_x", 1) * pow(2, self.get_value("maxLevel", 1))))
                       / self.get_value("domain_rect_numFragsPerBlock_x", 1))
        self.set_value("domain_rect_numBlocks_y",
                       (self.num_points_per_dim /
                        (self.get_value("domain_fragmentLength_y", 1) * pow(2, self.get_value("maxLevel", 1))))
                       / self.get_value("domain_rect_numFragsPerBlock_y", 1))
        self.set_value("domain_rect_numBlocks_z",
                       (self.num_points_per_dim /
                        (self.get_value("domain_fragmentLength_z", 1) * pow(2, self.get_value("maxLevel", 1))))
                       / self.get_value("domain_rect_numFragsPerBlock_z", 1))

        num_frags_per_block_total = \
            self.get_value("domain_rect_numFragsPerBlock_x", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_y", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_z", 1)
        frag_volume = \
            self.get_value("domain_fragmentLength_x", 1) \
            * self.get_value("domain_fragmentLength_y", 1) \
            * self.get_value("domain_fragmentLength_z", 1)
        num_blocks_total = \
            self.get_value("domain_rect_numBlocks_x", 1) \
            * self.get_value("domain_rect_numBlocks_y", 1) \
            * self.get_value("domain_rect_numBlocks_z", 1)

        self.set_value("domain_numBlocks", num_blocks_total)
        self.set_value("domain_numFragmentsPerBlock", num_frags_per_block_total)
        # self.set_value("mpi_enabled", "true")  # always true for this test case
        self.set_value("mpi_numThreads", num_blocks_total)
        if 1 == num_frags_per_block_total and 1 == frag_volume:
            self.set_value("omp_enabled", "false")
            self.set_value("omp_numThreads", 1)
            self.set_value("omp_parallelizeLoopOverFragments", "false")
            self.set_value("omp_parallelizeLoopOverDimensions", "false")
        elif num_frags_per_block_total > frag_volume:
            self.set_value("omp_enabled", "true")
            self.set_value("omp_numThreads", num_frags_per_block_total)
            self.set_value("omp_parallelizeLoopOverFragments", "true")
            self.set_value("omp_parallelizeLoopOverDimensions", "false")
        else:
            self.set_value("omp_enabled", "true")
            self.set_value("omp_numThreads", frag_volume)
            self.set_value("omp_parallelizeLoopOverFragments", "false")
            self.set_value("omp_parallelizeLoopOverDimensions", "true")

    def is_valid(self):
        num_unit_frags_x = \
            self.get_value("domain_rect_numBlocks_x", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_x", 1) \
            * self.get_value("domain_fragmentLength_x", 1)
        num_unit_frags_y = \
            self.get_value("domain_rect_numBlocks_y", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_y", 1) \
            * self.get_value("domain_fragmentLength_y", 1)
        num_unit_frags_z = \
            self.get_value("domain_rect_numBlocks_z", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_z", 1) \
            * self.get_value("domain_fragmentLength_z", 1)
        frag_volume = \
            self.get_value("domain_fragmentLength_x", 1) \
            * self.get_value("domain_fragmentLength_y", 1) \
            * self.get_value("domain_fragmentLength_z", 1)

        if not (self.get_num_nodes() > 8):
            # print("Not enough nodes in use")
            return False

        mem_per_node = (8 * 11 * 4 / 3 * pow(self.num_points_per_dim, self.dimensionality)) / self.get_num_nodes()
        if not (mem_per_node <= 12 * 1024 * 1024 * 1024):
            # print("Memory requirements for each node are too high")
            return False

        if not ((num_unit_frags_x == num_unit_frags_y) and (num_unit_frags_y == num_unit_frags_z)):
            # print("Not square")
            return False
        if not (self.get_value("domain_numBlocks", 1) >= 8):
            # print("Not enough blocks to distribute :%s" % num_blocks_total)
            return False
        if not (frag_volume <= 64 and self.get_value("domain_numFragmentsPerBlock", 1) <= 64):
            # print("Too many omp threads :%s" % self.get_value("omp_numThreads", 1))
            return False
        if not (1 == frag_volume or 1 == self.get_value("domain_numFragmentsPerBlock", 1)):
            # print("Two different omp parallelization strategies chosen concurrently")
            return False

        if not (self.get_value("l3tmp_numPre", 1) + self.get_value("l3tmp_numPost", 1) > 0):
            # print("Not enough smoothing steps")
            return False
        if not (self.get_value("l3tmp_numPre", 1) + self.get_value("l3tmp_numPost", 1) <= 12):
            # print("Too many smoothing steps")
            return False

        if not ("Jac" == self.get_value("l3tmp_smoother", "Jac")
                or "true" == self.get_value("l3tmp_useSlotsForJac", "true")):
            # print("Slotting is irrelevant if Jacobi is not used")
            return False

        if not ("Jac" != self.get_value("l3tmp_smoother", "Jac")
                or "true" == self.get_value("l3tmp_useSlotsForJac", "true")
                or (0 == self.get_value("l3tmp_numPre", 1) % 2 and 0 == self.get_value("l3tmp_numPost", 1) % 2)):
            # print("Constraint violation for un-slotted Jacobi")
            return False

        # print("Valid")
        return True

    rangedParameters = {  # variabilities to be tested with given ranges [parameterName, inclusiveBegin, inclusiveEnd]
        # "maxLevel": [4, 12, lambda x: x + 1],
        #
        # "domain_rect_numFragsPerBlock_x": [1, max_num_frags_per_block_per_dim, lambda x: 2 * x],
        # "domain_rect_numFragsPerBlock_y": [1, max_num_frags_per_block_per_dim, lambda x: 2 * x],
        # "domain_rect_numFragsPerBlock_z": [1, max_num_frags_per_block_per_dim, lambda x: 2 * x],
        # "domain_fragmentLength_x": [1, max_fragment_length_per_dim, lambda x: 2 * x],
        # "domain_fragmentLength_y": [1, max_fragment_length_per_dim, lambda x: 2 * x],
        # "domain_fragmentLength_z": [1, max_fragment_length_per_dim, lambda x: 2 * x],
        #
        # "l3tmp_numRecCycleCalls": [1, 2, lambda x: x + 1]
    }

    chosenRangedParameters = {  # to be filled later
    }

    listedParameters = {  # variabilities to be tested with given values from a predefined list [parameterName, [list]]
                          "l3tmp_smoother": ["\"Jac\"", "\"RBGS\""]  # ,

                          # "l3tmp_numPre": [0, 1, 2, 3, 4, 6, 9, 12],
                          # "l3tmp_numPost": [0, 1, 2, 3, 4, 6, 9, 12],
                          #
                          # "l3tmp_useSlotsForJac": ["true", "false"],
                          #
                          # "mpi_useCustomDatatypes": ["true", "false"]
    }

    chosenListedParameters = {  # to be filled later
    }

    derivedParameters = {  # derived with values to be updated based on the choices made
                           "domain_numBlocks": 2048,
                           "domain_numFragmentsPerBlock": 1,

                           "domain_rect_numBlocks_x": 1,
                           "domain_rect_numBlocks_y": 1,
                           "domain_rect_numBlocks_z": 1,

                           "mpi_numThreads": 2048,

                           "omp_enabled": "false",
                           "omp_numThreads": 1,
                           "omp_parallelizeLoopOverFragments": "false",
                           "omp_parallelizeLoopOverDimensions": "false"
    }

    constParameters = {  # parameters with values to be set directly
                         # temp consts
                         "maxLevel": 6,

                         "domain_rect_numFragsPerBlock_x": 1,
                         "domain_rect_numFragsPerBlock_y": 1,
                         "domain_rect_numFragsPerBlock_z": 1,
                         "domain_fragmentLength_x": 2,
                         "domain_fragmentLength_y": 2,
                         "domain_fragmentLength_z": 1,

                         "l3tmp_numRecCycleCalls": 1,

                         "l3tmp_numPre": 3,
                         "l3tmp_numPost": 3,

                         "l3tmp_useSlotsForJac": "true",

                         "mpi_useCustomDatatypes": "true",

                         # problem specific
                         "dimensionality": dimensionality,
                         "l3tmp_genStencilFields": "true",

                         # purely constant
                         "targetCompiler": "\"IBMBG\"",
                         "targetCompilerVersion": 12,
                         "targetCompilerVersionMinor": 1,

                         "useDblPrecision": "true",

                         "simd_instructionSet": "\"QPX\"",
                         "simd_avoidUnaligned": "true",

                         "timer_type": "\"MPI_TIME\"",

                         "domain_readFromFile": "false",
                         "domain_onlyRectangular": "true",
                         "domain_rect_generate": "true",

                         "ir_genSepLayoutsPerField": "true",

                         "comm_sepDataByFragment": "true",
                         "comm_sepDataByDomain": "false",
                         "comm_sepDataByField": "false",
                         "comm_sepDataByLevel": "false",
                         "comm_sepDataByNeighbor": "true",
                         "comm_useFragmentArrays": "true",
                         "comm_useDomainArrays": "true",
                         "comm_useFieldArrays": "false",
                         "comm_useLevelArrays": "false",
                         "comm_useNeighborArrays": "true",

                         "data_initAllFieldsWithZero": "true",
                         "data_useFieldNamesAsIdx": "false",

                         "mpi_defaultCommunicator": "\"MPI_COMM_WORLD\"",
                         "mpi_enabled": "true",
                         "mpi_useLoopsWherePossible": "true",

                         "omp_useCollapse": "false",

                         "l3tmp_generateL4": "true",

                         "l3tmp_cgs": "\"CG\"",

                         "l3tmp_useConditionsForRBGS": "true",
                         "l3tmp_useSlotVariables": "true",
                         "l3tmp_genHDepStencils": "true",

                         "l3tmp_genTimersPerFunction": "true",
                         "l3tmp_genTimersPerLevel": "false",
                         "l3tmp_genTimersForComm": "false",
                         "l3tmp_genCommTimersPerLevel": "false",

                         "l3tmp_printAllTimers": "false",
                         "l3tmp_printTimersToFile": "true",
                         "l3tmp_timerOuputFile": "\"timings.csv\"",

                         "l3tmp_exactSolution": "\"Kappa\"",
                         "l3tmp_genNonZeroRhs": "true",

                         "l3tmp_genExtFields": "false",
                         "l3tmp_genGlobalOmega": "false",
                         "l3tmp_genSetableStencil": "false",
                         "l3tmp_genVectorFields": "false",
                         "l3tmp_numVecDims": 1,
                         "l3tmp_genEmbeddedDomain": "false",
                         "l3tmp_useMaxNorm": "false",
                         "l3tmp_genCellBasedDiscr": "false",

                         "l3tmp_printFieldAtEnd": "false",
                         "l3tmp_initSolWithRand": "false",
                         "l3tmp_genForAutoTests": "false",  # TODO: TRUE
                         "l3tmp_printError": "true",
                         "l3tmp_useMaxNormForError": "false",  # TODO: TRUE

                         "l3tmp_sisc": "true",
                         "l3tmp_kelvin": "false",

                         "l3tmp_genStencilStencilConv": "false",
                         "l3tmp_genAsyncCommunication": "false",
                         "l3tmp_genFragLoops": "false",

                         "experimental_useLevelIndepFcts": "false",
                         "experimental_Neumann": "false",
                         "experimental_timerEnableCallStacks": "false",

                         "data_alignTmpBufferPointers": "false"
    }
