package isl;

import com.sun.jna.Library;
import com.sun.jna.Native;

public class Options {

	public static boolean setTileScaleTileLoops(boolean val) {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_set_tile_scale_tile_loops(ctx.getPtr(), val ? 1 : 0);
		}
		return result;
	}

	public static boolean getTileScaleTileLoops() {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_get_tile_scale_tile_loops(ctx.getPtr());
		}
		return result;
	}

	public static boolean setTileShiftPointLoops(boolean val) {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_set_tile_shift_point_loops(ctx.getPtr(), val ? 1 : 0);
		}
		return result;
	}

	public static boolean getTileShiftPointLoops() {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_get_tile_shift_point_loops(ctx.getPtr());
		}
		return result;
	}

	public static final int SCHEDULE_FUSE_MAX = 0;
	public static final int SCHEDULE_FUSE_MIN = 1;

	public static int setScheduleFuse(int val) {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_set_schedule_fuse(ctx.getPtr(), val);
		}
		return result;
	}

	public static int getScheduleFuse() {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_get_schedule_fuse(ctx.getPtr());
		}
		return result;
	}

	public static boolean setScheduleMaximizeBandDepth(boolean val) {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_set_schedule_maximize_band_depth(ctx.getPtr(), val ? 1 : 0);
		}
		return result;
	}

	public static boolean getScheduleMaximizeBandDepth() {
		Context ctx = Context.getDefaultInstance();
		boolean result;
		synchronized (ctx) {
			result = 0 != OptionsImpl.isl.isl_options_get_schedule_maximize_band_depth(ctx.getPtr());
		}
		return result;
	}

	public static int setScheduleMaxConstantTerm(int val) {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_set_schedule_max_constant_term(ctx.getPtr(), val);
		}
		return result;
	}

	public static int getScheduleMaxConstantTerm() {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_get_schedule_max_constant_term(ctx.getPtr());
		}
		return result;
	}

	public static int setScheduleMaxCoefficient(int val) {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_set_schedule_max_coefficient(ctx.getPtr(), val);
		}
		return result;
	}

	public static int getScheduleMaxCoefficient() {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_get_schedule_max_coefficient(ctx.getPtr());
		}
		return result;
	}

	public static final int SCHEDULE_ALGORITHM_ISL = 0;
	public static final int SCHEDULE_ALGORITHM_FEAUTRIER = 1;

	public static int setScheduleAlgorithm(int val) {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_set_schedule_algorithm(ctx.getPtr(), val);
		}
		return result;
	}

	public static int getScheduleAlgorithm() {
		Context ctx = Context.getDefaultInstance();
		int result;
		synchronized (ctx) {
			result = OptionsImpl.isl.isl_options_get_schedule_algorithm(ctx.getPtr());
		}
		return result;
	}
}

interface OptionsImpl extends Library {
	static OptionsImpl isl = (OptionsImpl) Native.loadLibrary("isl", OptionsImpl.class);

	int isl_options_set_tile_scale_tile_loops(Context.Ptr ctx, int val);
	int isl_options_get_tile_scale_tile_loops(Context.Ptr ctx);
	int isl_options_set_tile_shift_point_loops(Context.Ptr ctx, int val);
	int isl_options_get_tile_shift_point_loops(Context.Ptr ctx);

	int isl_options_set_schedule_fuse(Context.Ptr ctx, int val);
	int isl_options_get_schedule_fuse(Context.Ptr ctx);
	int isl_options_set_schedule_maximize_band_depth(Context.Ptr ctx, int val);
	int isl_options_get_schedule_maximize_band_depth(Context.Ptr ctx);
	int isl_options_set_schedule_max_constant_term(Context.Ptr ctx, int val);
	int isl_options_get_schedule_max_constant_term(Context.Ptr ctx);
	int isl_options_set_schedule_max_coefficient(Context.Ptr ctx, int val);
	int isl_options_get_schedule_max_coefficient(Context.Ptr ctx);

	int isl_options_set_schedule_algorithm(Context.Ptr ctx, int val);
	int isl_options_get_schedule_algorithm(Context.Ptr ctx);
}
