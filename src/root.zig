pub const version_string = build_options.version;
pub const version = std.SemanticVersion.parse(version_string) catch unreachable;
pub const tokenizer = @import("tokenizer.zig");
pub const binary = @import("binary.zig");
pub const mem = @import("mem.zig");
pub const Parse = @import("Parse.zig");
pub const Ast = @import("Ast.zig");
pub const Sema = @import("Sema.zig");
pub const Ir = @import("Ir.zig");

pub const Value = extern struct {
    tag: Tag,
    /// int/bool: value, range: lo
    x: i64,
    /// range: hi
    y: i64,

    pub fn int(value: i64) Value {
        return .{ .tag = .int, .x = value, .y = 0 };
    }

    pub fn boolean(value: bool) Value {
        return .{ .tag = .boolean, .x = if (value) 1 else 0, .y = 0 };
    }

    pub fn range(lo: i64, hi: i64) Value {
        return .{ .tag = .range, .x = lo, .y = hi };
    }

    pub fn asInt(value: Value) i64 {
        return switch (value.tag) {
            .int, .boolean => value.x,
            .range => value.x,
        };
    }

    pub const Tag = enum(u8) {
        int,
        boolean,
        range,
    };
};

pub const State = struct {
    regs: []Value,
    globals: []Value,

    pub fn init(allocator: std.mem.Allocator, program: *const Ir.Program) std.mem.Allocator.Error!State {
        var regs = try allocator.alloc(Value, program.regs);
        for (regs) |*reg| {
            reg.* = .int(0);
        }
        var globals = try allocator.alloc(Value, program.vars);
        for (globals) |*global| {
            global.* = .int(0);
        }
        const ops = program.code.items(.op);
        const dsts = program.code.items(.dst);
        const as = program.code.items(.a);
        const bs = program.code.items(.b);
        const aux = program.code.items(.aux);

        var ip: usize = 0;
        while (ip < ops.len) : (ip += 1) {
            const op = ops[ip];
            const dst = dsts[ip];
            const a = as[ip];
            const b = bs[ip];
            const ax = aux[ip];

            switch (op) {
                .iconst => {
                    regs[dst] = .int(program.constants.items[ax]);
                },
                .load => {
                    regs[dst] = globals[ax];
                },
                .store => {
                    globals[ax] = regs[a];
                },
                .mov => {
                    regs[dst] = regs[a];
                },
                .add => regs[dst] = .int(regs[a].asInt() + regs[b].asInt()),
                .sub => regs[dst] = .int(regs[a].asInt() - regs[b].asInt()),
                .mul => regs[dst] = .int(regs[a].asInt() * regs[b].asInt()),
                .div => {
                    const d = regs[b].asInt();
                    regs[dst] = .int(if (d == 0) 0 else @divTrunc(regs[a].asInt(), d));
                },
                .mod => {
                    const d = regs[b].asInt();
                    regs[dst] = .int(if (d == 0) 0 else @mod(regs[a].asInt(), d));
                },
                .less => regs[dst] = .boolean(regs[a].asInt() < regs[b].asInt()),
                .less_equal => regs[dst] = .boolean(regs[a].asInt() <= regs[b].asInt()),
                .greater => regs[dst] = .boolean(regs[a].asInt() > regs[b].asInt()),
                .greater_equal => regs[dst] = .boolean(regs[a].asInt() >= regs[b].asInt()),
                .equal_equal => regs[dst] = .boolean(regs[a].asInt() == regs[b].asInt()),
                .not_equal => regs[dst] = .boolean(regs[a].asInt() != regs[b].asInt()),

                .@"and" => regs[dst] = .int(regs[a].asInt() & regs[b].asInt()),
                .@"or" => regs[dst] = .int(regs[a].asInt() | regs[b].asInt()),
                .pipe, .pipe_greater, .less_pipe => {
                    regs[dst] = .int(regs[a].asInt() | regs[b].asInt());
                },
                .range => regs[dst] = .range(regs[a].asInt(), regs[b].asInt()),
            }
        }
        return .{ .regs = regs, .globals = globals };
    }

    pub fn deinit(state: *State, allocator: std.mem.Allocator) void {
        allocator.free(state.regs);
        allocator.free(state.globals);
        state.* = undefined;
    }
};

pub fn dumpValue(label: []const u8, v: Value) void {
    switch (v.tag) {
        .int => std.debug.print("{s} = int({d})\n", .{ label, v.x }),
        .boolean => std.debug.print("{s} = bool({s}) ; raw={d}\n", .{ label, if (v.x != 0) "true" else "false", v.x }),
        .range => std.debug.print("{s} = range({d}..{d})\n", .{ label, v.x, v.y }),
    }
}

pub fn dumpAll(globals: []const Value) void {
    for (globals, 0..) |v, i| {
        var buf: [32]u8 = undefined;
        const label = std.fmt.bufPrint(&buf, "g[{d}]", .{i}) catch "g[?]";
        dumpValue(label, v);
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");
const build_options = @import("build_options");
