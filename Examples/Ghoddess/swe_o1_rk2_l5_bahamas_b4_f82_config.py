import sympy as sp

from ghoddess.util import deg_to_rad
from swe.configMasterReal import ConfigMasterReal


class Config(ConfigMasterReal):
    def __init__(self):
        super().__init__()

        # exit criterion

        self.default_dt = 1  # in seconds
        self.adapt_time_step = 0  # 0: always use default_dt, 1: compute step size once in the beginning, 2: recompute step size every time step

        self.use_time_steps = False  # only if adapt_time_step = 0, if True, use fixed number of time steps (timeSteps); otherwise use a maximum time (tMax)
        self.time_steps = 6000
        self.t_max = 1500

        # printing

        self.print_interval = 100

        # number of degrees of freedom; constant: d = 1, linear: d = 3, quadratic: d = 6, cubic: d = 10
        self.adapt_local_order = False
        self.adaptivity_criterion = f"eta"  # no_adaption: no adaption; eta: eta values being outside of certain range; element_grad: element gradients exceeding threshold (Kubatko et al. 2009);
        # neigh_grad: gradients to neighbor exceeding threshold; diff_to_lower_order: L^2 norm of the difference between the approximation of the water depth of order o and o - 1 (Esskilsson 2010)
        # eta_jumps: sum of jumps in eta (Remacle2003); diff_to_lower_order_and_eta_jumps: combination of diff_to_lower_order and eta_jumps

        # thresholds for criterion eta
        self.eta_min_val = -0.5
        self.eta_max_val = 0.5
        # thresholds for criterion element_grad and neigh_grad
        self.adaptivity_threshold_eta_grad = 0.0001
        self.adaptivity_threshold_u_grad = 0
        self.adaptivity_threshold_v_grad = 0

        self.adaptivity_nlock = 10

        self.min_order = 0
        self.max_order = 1
        self.order = self.max_order

        self.print_vtk = True

        self.print_fields_to_file = False
        self.print_single_terms_to_file = False
        self.print_quantities_to_file = False
        self.print_node_quantities_to_file = False
        self.print_exact_to_file = False
        self.print_error_to_file = False
        self.print_lambda_to_file = False

        self.print_field_precision = -1

        # stations

        self.stations = [[38666.66, 49333.32],
                         [56097.79, 9612.94],
                         [41262.60, 29775.73],
                         [59594.66, 41149.62]]

        self.print_stations_eta = False
        self.print_stations_u = False
        self.print_stations_v = False
        if not self.original_formulation:
            self.print_stations_u_tilde = False
            self.print_stations_v_tilde = False

        # grid and bathymetry

        self.minLevel = 5
        self.maxLevel = self.minLevel  # 2**maxLevel is the number of cells per dimension

        scenario = "bahamas_b4_f82"

        if "bahamas_b1_f82" == scenario:  # Bahamas with 82 fragments distributed across 1 block
            self.grid_folder = "../grids/BSG_bahamas_b1_f82"
            self.n_blocks = 1
            self.n_frags_per_block = 82
        elif "bahamas_b4_f82" == scenario:  # Bahamas with 82 fragments distributed across 4 blocks
            self.grid_folder = "../grids/BSG_bahamas_b4_f82"
            self.n_blocks = 4
            self.n_frags_per_block = 21
        elif "bahamas_b82_f82" == scenario:  # Bahamas with 82 fragments/ blocks
            self.grid_folder = "../grids/BSG_bahamas_b82_f82"
            self.n_blocks = 82
            self.n_frags_per_block = 1
        elif "bahamas_b1_f32" == scenario:  # Bahamas with 32 fragments distributed across 1 block
            self.grid_folder = "../grids/BSG_bahamas_b1_f32"
            self.n_blocks = 1
            self.n_frags_per_block = 32
        else:
            raise ValueError("Unknown scenario chosen")

        # parallelization

        self.omp_enabled = True
        if 1 == self.n_blocks:
            self.omp_num_threads = 8
        else:
            self.omp_num_threads = max(8, self.n_frags_per_block)

        # parameters for open sea boundaries #
        self.nope = 1  # number of open sea boundaries forcing segments
        self.nbfr = 5  # total number of forcing frequencies on open sea boundaries

        # ramp function option
        # 0: no ramp function is used for forcing functions
        # 1: a hyperbolic tangent ramp function is specified and applied to the surface elevation specified boundary conditions,
        #    the tidal potential forcing function as well as the wind and atmospheric pressure forcing functions
        self.n_ramp = 1
        self.d_ramp = 2  # duration of ramp function (in days)

        # forcing frequencies
        self.amig = [sp.S(0.000067597751162),
                     sp.S(0.000072921165921),
                     sp.S(0.000137879713787),
                     sp.S(0.000140518917083),
                     sp.S(0.000145444119418)]

        # nodal factor
        self.ff = [sp.S(1), sp.S(1), sp.S(1), sp.S(1), sp.S(1)]

        # equilibrium argument in degrees for tidal forcing on open sea boundaries
        self.face = [deg_to_rad(0), deg_to_rad(0), deg_to_rad(0), deg_to_rad(0), deg_to_rad(0)]

        self.variable_emo_efa = False

        # amplitude of the harmonic forcing function at the open sea boundaries for frequency
        self.emo = [sp.S(0.075),
                    sp.S(0.095),
                    sp.S(0.1),
                    sp.S(0.395),
                    sp.S(0.06)]

        # phase (in degrees) of the harmonic forcing function at the open sea boundaries for frequency
        self.efa = [deg_to_rad(sp.S(194.806)),
                    deg_to_rad(sp.S(206.265)),
                    deg_to_rad(sp.S(340)),
                    deg_to_rad(sp.S(0)),
                    deg_to_rad(sp.S(42.9718))]

        # projection in LONG/LAT #
        # center of cpp projection in degrees LONG/LAT
        self.slam_0 = deg_to_rad(sp.S(282.5))
        self.sfea_0 = deg_to_rad(sp.S(24))

        # coriolis force #
        # coriolis option parameter 0: spatially constant, 1: spatially variable
        self.ncor = 0
        # constant coriolis coefficient
        self.cori = sp.S(0.0000319)

        # friction #
        # friction option parameter 0: linear, 1: nonlinear
        self.noli_bf = 1
        # linearized friction coefficient
        self.tau = sp.S(0.0001)
        # nonlinear bottom friction coefficient, can possibly be dependent on node position
        self.cf = sp.S(0.009)

        # tidal potential #
        # number of tidal potential constituents being forced
        self.ntif = 0
        self.tpk = sp.zeros(self.ntif)  # tidal potential amplitude
        self.amigt = sp.zeros(self.ntif)  # tidal potential frequency earth tide
        self.etrf = sp.zeros(self.ntif)  # potential reduction factor (generally taken 0.69) for all constitutes (hendershott) but for more precise calculations can take slightly
        # different values (e.g. see Wahr, 1981)
        self.fft = sp.zeros(self.ntif)  # nodal factor
        self.facet = sp.zeros(self.ntif)  # equilibrium argument in degrees

        self.check_input()
        self.update_tag()
        self.update_filenames()


config = Config()
