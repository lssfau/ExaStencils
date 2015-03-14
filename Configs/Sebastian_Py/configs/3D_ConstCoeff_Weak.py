class Configuration:
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
        elif key in self.chosenRangedParameters:
            return self.chosenRangedParameters[key]
        else:
            return self.chosenListedParameters.get(key, def_val)

    def set_value(self, key, new_val):
        if key in self.constParameters:
            self.constParameters[key] = new_val
        if key in self.chosenRangedParameters:
            self.chosenRangedParameters[key] = new_val
        if key in self.chosenListedParameters:
            self.chosenListedParameters[key] = new_val

    def update(self):
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
        self.set_value("mpi_enabled", "true")  # always true for this test case
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
        num_blocks_total = \
            self.get_value("domain_rect_numBlocks_x", 1) \
            * self.get_value("domain_rect_numBlocks_y", 1) \
            * self.get_value("domain_rect_numBlocks_z", 1)
        num_frags_per_block_total = \
            self.get_value("domain_rect_numFragsPerBlock_x", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_y", 1) \
            * self.get_value("domain_rect_numFragsPerBlock_z", 1)
        frag_volume = \
            self.get_value("domain_fragmentLength_x", 1) \
            * self.get_value("domain_fragmentLength_y", 1) \
            * self.get_value("domain_fragmentLength_z", 1)

        if not ((num_unit_frags_x == num_unit_frags_y) and (num_unit_frags_y == num_unit_frags_z)):
            # print("Not square")
            return False
        if not (16 == num_unit_frags_x):
            # print("Not the right size :%s" % num_unit_frags_x)
            return False
        if not (num_blocks_total >= 8):
            # print("Not enough blocks to distribute :%s" % num_blocks_total)
            return False
        if not (frag_volume <= 64 and num_frags_per_block_total <= 64):
            # print("Too many omp threads :%s" % self.get_value("omp_numThreads", 1))
            return False
        if not (1 == frag_volume or 1 == num_frags_per_block_total):
            # print("Two different omp parallelization strategies chosen concurrently")
            return False

        # print("Valid")
        return True

    baseName = "3D_CC_Weak"

    rangedParameters = {  # variabilities to be tested with given ranges [parameterName, inclusiveBegin, inclusiveEnd]
                          "domain_rect_numBlocks_x": [1, 16, lambda x: 2 * x],
                          "domain_rect_numBlocks_y": [1, 16, lambda x: 2 * x],
                          "domain_rect_numBlocks_z": [1, 16, lambda x: 2 * x],
                          "domain_rect_numFragsPerBlock_x": [1, 16, lambda x: 2 * x],
                          "domain_rect_numFragsPerBlock_y": [1, 16, lambda x: 2 * x],
                          "domain_rect_numFragsPerBlock_z": [1, 16, lambda x: 2 * x],
                          "domain_fragmentLength_x": [1, 16, lambda x: 2 * x],
                          "domain_fragmentLength_y": [1, 16, lambda x: 2 * x],
                          "domain_fragmentLength_z": [1, 16, lambda x: 2 * x],
    }

    chosenRangedParameters = {  # to be filled later
    }

    listedParameters = {  # variabilities to be tested with given values from a predefined list [parameterName, [list]]
                          "l3tmp_smoother": ["\"GS\"", "\"Jac\"", "\"RBGS\""]
    }

    chosenListedParameters = {  # to be filled later
    }

    constParameters = {  # parameters with values to be set directly
                         "targetCompiler": "\"IBMBG\"",
                         "simd_instructionSet": "\"QPX\"",
                         "timer_type": "\"MPI_TIME\"",
                         "dimensionality": 3,
                         "minLevel": 0,
                         "maxLevel": 6,
                         "comm_strategyFragment": 6,
                         "domain_onlyRectangular": "true",
                         "domain_numBlocks": 2048,
                         "domain_numFragmentsPerBlock": 1,
                         "domain_rect_generate": "true",
                         "omp_enabled": "false",
                         "omp_numThreads": 1,
                         "mpi_enabled": "true",
                         "mpi_numThreads": 2048,
                         "omp_parallelizeLoopOverFragments": "false",
                         "omp_parallelizeLoopOverDimensions": "false",
                         "l3tmp_generateL4": "true",
                         "l3tmp_sisc": "true",
                         "l3tmp_genStencilFields": "false",
                         "l3tmp_genHDepStencils": "true",
                         "l3tmp_genNonZeroRhs": "true",
                         "l3tmp_exactSolution": "\"Kappa\"",
                         "mpi_useCustomDatatypes": "true",
                         "poly_optLevel_fine": 3,
                         "opt_useAddressPrecalc": "true",
                         "opt_vectorize": "false",
                         "opt_unroll": 2,

                         "l3tmp_genForAutoTests": "true",
                         "l3tmp_printError": "false",
                         # "l3tmp_useMaxNormForError": "true",
                         "l3tmp_genTimersPerFunction": "true",
                         "l3tmp_genTimersForComm": "true",
                         "l3tmp_printTimersToFile": "true",
    }
