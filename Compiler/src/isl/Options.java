package isl;

import com.sun.jna.Library;
import com.sun.jna.Native;

public class Options {

	public static int FOO = 42;

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
}

interface OptionsImpl extends Library {
	static OptionsImpl isl = (OptionsImpl) Native.loadLibrary("isl", OptionsImpl.class);

	int isl_options_set_tile_scale_tile_loops(Context.Ptr ctx, int val);
	int isl_options_get_tile_scale_tile_loops(Context.Ptr ctx);
	int isl_options_set_tile_shift_point_loops(Context.Ptr ctx, int val);
	int isl_options_get_tile_shift_point_loops(Context.Ptr ctx);
}
