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
        pass

    def is_valid(self):
        return True

    baseName = "Test"

    rangedParameters = {  # variabilities to be tested with given ranges [parameterName, inclusiveBegin, inclusiveEnd]
                          "l3tmp_numPre": [2, 3, lambda x: x + 1],
                          "l3tmp_numPost": [2, 3, lambda x: x + 1]
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
                         "dimensionality": 3,
                         "minLevel": 0,
                         "maxLevel": 6,
                         "comm_strategyFragment": 6,
                         "domain_onlyRectangular": "true",
                         "domain_numBlocks": 2048,
                         "domain_numFragmentsPerBlock": 1,
                         "domain_fragmentLength_x": 2,
                         "domain_fragmentLength_y": 1,
                         "domain_fragmentLength_z": 1,
                         "domain_rect_generate": "true",
                         "domain_rect_numBlocks_x": 8,
                         "domain_rect_numBlocks_y": 16,
                         "domain_rect_numBlocks_z": 16,
                         "domain_rect_numFragsPerBlock_x": 1,
                         "domain_rect_numFragsPerBlock_y": 1,
                         "domain_rect_numFragsPerBlock_z": 1,
                         "omp_enabled": "false",
                         "omp_numThreads": 1,
                         "mpi_enabled": "true",
                         "mpi_numThreads": 2048,
                         "omp_parallelizeLoopOverFragments": "false",
                         "omp_parallelizeLoopOverDimensions": "false",
                         "l3tmp_generateL4": "true",
                         "l3tmp_genForAutoTests": "false",
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
                         "l3tmp_printError": "true",
                         "l3tmp_useMaxNormForError": "true"
    }
