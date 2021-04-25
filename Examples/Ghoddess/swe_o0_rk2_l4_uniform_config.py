from swe.configMasterAnalytical import ConfigMasterAnalytical


class Config(ConfigMasterAnalytical):
    def __init__(self):
        super().__init__()

        # exit criterion
        self.default_dt = 0.5  # in seconds
        self.adapt_time_step = 0  # 0: always use default_dt, 1: compute step size once in the beginning, 2: recompute step size every time step

        self.use_time_steps = False  # only if adapt_time_step = 0, if True, use fixed number of time steps (timeSteps); otherwise use a maximum time (tMax)
        self.time_steps = 3000
        self.t_max = 1500  # in seconds

        # printing
        self.print_interval = 100  # in seconds

        # number of degrees of freedom; constant: d = 1, linear: d = 3, quadratic: d = 6, cubic: d = 10
        self.adapt_local_order = False
        self.adaptivity_nlock = 10
        self.adaptivity_criterion = f"eta"  # no_adaption: no adaption; eta: eta values being outside of certain range; element_grad: element gradients exceeding threshold (Kubatko et al. 2009);
        # neigh_grad: gradients to neighbor exceeding threshold; diff_to_lower_order: L^2 norm of the difference between the approximation of the water depth of order o and o - 1 (Esskilsson 2010)
        # eta_jumps: sum of jumps in eta (Remacle2003); diff_to_lower_order_and_eta_jumps: combination of diff_to_lower_order and eta_jumps

        self.patchwise_adaptivity = True

        # thresholds for criterion eta
        if self.patchwise_adaptivity:
            self.eta_min_val = 270
            self.eta_max_val = 320
        else:
            self.eta_min_val = 2
            self.eta_max_val = 2.6
        # thresholds for criterion element_grad and neigh_grad
        self.adaptivity_threshold_eta_grad = 0.005
        self.adaptivity_threshold_u_grad = 0.0005
        self.adaptivity_threshold_v_grad = 0.0005
        # thresholds for criterion diff_to_lower_order
        self.eta_diff_threshold_refine = 0.007
        self.eta_diff_threshold_coarse = 0.001
        # thresholds for criterion eta_jumps
        self.adaptivity_threshold_eta_jump = 10  # 10 for o0-01; 0.06 for 01-o2

        self.min_order = 0
        self.max_order = 1

        self.set_bc_ids_analdirichlet = True

        self.print_vtk = True

        self.print_fields_to_file = False
        self.print_single_terms_to_file = False
        self.print_quantities_to_file = False
        self.print_node_quantities_to_file = False
        self.print_exact_to_file = False
        self.print_error_to_file = False
        self.print_lambda_to_file = False

        self.print_field_precision = -1

        # grid and bathymetry
        self.assume_uniform_grid = True

        # # only used if assume_uniform_grid is enabled
        # self.grid_gen_frag_scale_x = 1
        # self.grid_gen_frag_scale_y = 1
        # self.grid_gen_num_frag_per_block_x = 2
        # self.grid_gen_num_frag_per_block_y = 2
        # self.grid_gen_num_block_x = 1
        # self.grid_gen_num_block_y = 1

        self.read_grid_from_file = False

        self.minLevel = 4
        self.maxLevel = self.minLevel  # 2**maxLevel is the number of cells per dimension

        self.grid_folder = ""
        self.n_blocks = 1
        self.n_frags_per_block = 1

        self.truncate = False  # only for d > 1
        if self.truncate:
            self.max_degree_tri = 2 * self.order
            self.max_degree_edge = 2 * self.order + 1

        # time stepping scheme: "RK1" or "RK2" or "RK3"
        self.time_integration = "RK2"

        # control generation aspects

        self.store_normals = not self.assume_uniform_grid
        self.store_edge_lengths = not self.assume_uniform_grid

        self.store_b = False

        self.store_det_b = False
        self.store_det_b_inv = False
        self.store_det_b_sqrt = False
        self.store_det_b_sqrt_inv = False
        
        # nodal reduction
        self.swe_nodalReductionPrint = True

        # parallelization

        self.omp_enabled = True
        self.omp_num_threads = 8

        self.check_input()
        self.update_tag()
        self.update_filenames()


config = Config()
