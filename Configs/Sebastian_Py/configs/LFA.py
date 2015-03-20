class Configuration:
    baseName = "SISC_LFA"

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
        pass

    def is_valid(self):
        if 2 == self.get_value("dimensionality", 1):
            if not self.get_value("numCellsPerDimCoarsest", 1) < 32 * 1024:
                # print("To many points on the coarse grid")
                return False
        if 3 == self.get_value("dimensionality", 1):
            if not self.get_value("numCellsPerDimCoarsest", 1) < 1024:
                # print("To many points on the coarse grid")
                return False

        if not (self.get_value("l3tmp_numPre", 1) + self.get_value("l3tmp_numPost", 1) > 0):
            # print("Not enough smoothing steps")
            return False
        if not (self.get_value("l3tmp_numPre", 1) + self.get_value("l3tmp_numPost", 1) <= 12):
            # print("Too many smoothing steps")
            return False

        # print("Valid")
        return True

    rangedParameters = {  # variabilities to be tested with given ranges [parameterName, inclusiveBegin, inclusiveEnd]
                          "dimensionality": [2, 3, lambda x: x + 1],

                          "numCellsPerDimCoarsest": [2, 32 * 1024, lambda x: 2 * x],

                          "l3tmp_numRecCycleCalls": [1, 2, lambda x: x + 1],

                          "l3tmp_numPre": [0, 4, lambda x: x + 1],
                          "l3tmp_numPost": [0, 4, lambda x: x + 1],
    }

    chosenRangedParameters = {  # to be filled later
    }

    listedParameters = {  # variabilities to be tested with given values from a predefined list [parameterName, [list]]
                          "l3tmp_genStencilFields": ["true", "false"],

                          "l3tmp_smoother": ["\"Jac\"", "\"RBGS\""],
    }

    chosenListedParameters = {  # to be filled later
    }

    derivedParameters = {  # derived with values to be updated based on the choices made
    }

    constParameters = {  # parameters with values to be set directly
    }
