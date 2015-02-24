class Configuration:
    def get_num_nodes(self):
        return (self.get_num_mpi() * self.get_num_omp()) / 64

    def get_num_mpi(self):
        params = self.constParameters + self.chosenRangedParameters
        return next((x for x in params if "mpi_numThreads" == x[0]), ["", 1])[1]

    def get_num_omp(self):
        params = self.constParameters + self.chosenRangedParameters
        return next((x for x in params if "omp_numThreads" == x[0]), ["", 1])[1]

    def get_value(self, param, def_val):
        params = self.constParameters + self.chosenRangedParameters + self.chosenListedParameters
        return next((x for x in params if param == x[0]), ["", def_val])[1]

    def set_value(self, param, new_val):
        candidates = (x for x in self.constParameters + self.chosenRangedParameters + self.chosenListedParameters if
                      param == x[0])
        for params in candidates:
            params[1] = new_val

    def update(self):
        pass

    def is_valid(self):
        return True

    baseName = "Test"

    rangedParameters = [  # variabilities to be tested with given ranges [parameterName, inclusiveBegin, inclusiveEnd]
                          ["l3tmp_numPre", 2, 3, lambda x: x + 1],
                          ["l3tmp_numPost", 2, 3, lambda x: x + 1]
    ]

    chosenRangedParameters = [  # to be filled later
    ]

    listedParameters = [  # variabilities to be tested with given values from a predefined list [parameterName, [list]]
                          ["l3tmp_smoother", ["\"GS\"", "\"Jac\"", "\"RBGS\""]]
    ]

    chosenListedParameters = [  # to be filled later
    ]

    constParameters = [  # parameters with values to be set directly
                         ["targetCompiler", "\"IBMBG\""],
                         ["simd_instructionSet", "\"QPX\""],
                         ["dimensionality", 3],
                         ["minLevel", 0],
                         ["maxLevel", 6],
                         ["comm_strategyFragment", 6],
                         ["domain_onlyRectangular", "true"],
                         ["domain_numBlocks", 2048],
                         ["domain_numFragmentsPerBlock", 1],
                         ["domain_fragmentLength_x", 2],
                         ["domain_fragmentLength_y", 1],
                         ["domain_fragmentLength_z", 1],
                         ["domain_rect_generate", "true"],
                         ["domain_rect_numBlocks_x", 8],
                         ["domain_rect_numBlocks_y", 16],
                         ["domain_rect_numBlocks_z", 16],
                         ["domain_rect_numFragsPerBlock_x", 1],
                         ["domain_rect_numFragsPerBlock_y", 1],
                         ["domain_rect_numFragsPerBlock_z", 1],
                         ["omp_enabled", "false"],
                         ["omp_numThreads", 1],
                         ["mpi_enabled", "true"],
                         ["mpi_numThreads", 2048],
                         ["omp_parallelizeLoopOverFragments", "false"],
                         ["omp_parallelizeLoopOverDimensions", "false"],
                         ["l3tmp_generateL4", "true"],
                         ["l3tmp_genForAutoTests", "false"],
                         ["l3tmp_sisc", "true"],
                         ["l3tmp_genStencilFields", "false"],
                         ["l3tmp_genHDepStencils", "true"],
                         ["l3tmp_genNonZeroRhs", "true"],
                         ["l3tmp_exactSolution", "\"Kappa\""],
                         ["mpi_useCustomDatatypes", "true"],
                         ["poly_optLevel_fine", 3],
                         ["opt_useAddressPrecalc", "true"],
                         ["opt_vectorize", "false"],
                         ["opt_unroll", 2],
                         ["l3tmp_printError", "true"],
                         ["l3tmp_useMaxNormForError", "true"]
    ]
