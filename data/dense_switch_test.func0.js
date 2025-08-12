function global() {
	globalThis.denseSwitchTest = undefined;
	globalThis.largeSwitchTest = undefined;
	globalThis.switchWithBreak = undefined;
	globalThis.switchWithFallthrough = undefined;
	globalThis.nestedSwitch = undefined;
	globalThis.mixedBreakReturn = undefined;
	const var2_b = function denseSwitchTest(arg0) {
		/* Nested function - body not yet decompiled */;
	};
	const var0_a = globalThis;
	var0.denseSwitchTest = var2;
	const var2 = function largeSwitchTest(arg0) {
		/* Nested function - body not yet decompiled */;
	};
	var0.largeSwitchTest = var2;
	const var2_a = function switchWithBreak(arg0) {
		/* Nested function - body not yet decompiled */;
	};
	var0.switchWithBreak = var2;
	const var2_d = function switchWithFallthrough(arg0) {
		/* Nested function - body not yet decompiled */;
	};
	var0.switchWithFallthrough = var2;
	const var2_e = function nestedSwitch(arg0, arg1) {
		/* Nested function - body not yet decompiled */;
	};
	var0.nestedSwitch = var2;
	const var1_t = function mixedBreakReturn(arg0) {
		/* Nested function - body not yet decompiled */;
	};
	var0.mixedBreakReturn = __env_1;
	const var3_a = var0.denseSwitchTest;
	const var2_c = undefined;
	let var1_k = 15;
	const var1 = var3_a.call(var2_c, var1_k);
	const var3 = var0.largeSwitchTest;
	let var1_v = 7;
	const var1_m = var3.call(var2_c, var1_v);
	const var1_o = var0.switchWithBreak;
	let var5 = 3;
	const var1_e = var1_o.call(var2_c, var5);
	const var1_u = var0.switchWithFallthrough;
	let var4 = 0;
	const var1_i = var1_u.call(var2_c, var4);
	const var1_b = var0.switchWithFallthrough;
	const var1_c = var1_b.call(var2_c, var5);
	const var3_b = var0.switchWithFallthrough;
	let var1_p = 6;
	const var1_j = var3_b.call(var2_c, var1_p);
	const var1_q = var0.nestedSwitch;
	let var3_c = 1;
	const var1_f = var1_q.call(var2_c, var4, var3_c);
	const var1_n = var0.nestedSwitch;
	const var1_g = var1_n.call(var2_c, var3_c, var5);
	const var1_s = var0.nestedSwitch;
	const var1_a = var1_s.call(var2_c, var5, var4);
	const var1_x = var0.mixedBreakReturn;
	const var1_w = var1_x.call(var2_c, var3_c);
	const var3_d = var0.mixedBreakReturn;
	let var1_l = 2;
	const var1_r = var3_d.call(var2_c, var1_l);
	const var1_h = var0.mixedBreakReturn;
	let var0 = 4;
	const var0_b = var1_h.call(var2_c, var0);
	return var0;
}
