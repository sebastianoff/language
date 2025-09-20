ast: *const Ast,
source: []const u8,
m: *Module,
constants: std.AutoHashMapUnmanaged(i64, u32) = .empty,
gvn: std.AutoHashMapUnmanaged(Key, u32) = .empty,
memver: std.AutoHashMapUnmanaged(u32, u32) = .empty,

const Key = packed struct {
    op: operations.Op,
    a: u32,
    b: u32,
    aux: u32,
    /// memory version (0 for ops that don't read memory)
    mem: u32,
};

pub fn init(ast: *const Ast, source: []const u8, m: *Module) Ir {
    return .{ .ast = ast, .source = source, .m = m };
}

pub fn deinit(ir: *Ir, allocator: std.mem.Allocator) void {
    ir.constants.deinit(allocator);
    ir.gvn.deinit(allocator);
    ir.memver.deinit(allocator);
}

pub fn internConst(ir: *Ir, allocator: std.mem.Allocator, value: i64) std.mem.Allocator.Error!u32 {
    if (ir.constants.get(value)) |index| {
        return index;
    }
    const index: u32 = @intCast(ir.m.constants.items.len);
    try ir.m.constants.append(allocator, value);
    try ir.constants.put(allocator, value, index);
    return index;
}

pub fn emitRaw(
    ir: *Ir,
    allocator: std.mem.Allocator,
    op: operations.Op,
    a: u32,
    b: u32,
    aux: u32,
) std.mem.Allocator.Error!u32 {
    const index: u32 = @intCast(ir.m.instructions.len);
    try ir.m.instructions.append(allocator, .{ .op = op, .a = a, .b = b, .aux = aux });
    return index;
}

fn canonicalize(op: operations.Op, a: u32, b: u32) struct { u32, u32 } {
    if (operations.isCommutative(op) and b < a) {
        return .{ b, a };
    }
    return .{ a, b };
}

fn emit(
    ir: *Ir,
    allocator: std.mem.Allocator,
    op: operations.Op,
    a: u32,
    b: u32,
    aux: u32,
) std.mem.Allocator.Error!u32 {
    if (operations.isPure(op)) {
        const ab = canonicalize(op, a, b);
        // include memory version for load, 0 for all other pure ops
        const mem: u32 = switch (op) {
            .load => ir.memVersionForSlot(aux),
            else => 0,
        };
        const key: Key = .{ .op = op, .a = ab[0], .b = ab[1], .aux = aux, .mem = mem };
        if (ir.gvn.get(key)) |hit| {
            return hit;
        }
        const id = try ir.emitRaw(allocator, op, ab[0], ab[1], aux);
        try ir.gvn.put(allocator, key, id);
        return id;
    }
    // impure
    const id = try ir.emitRaw(allocator, op, a, b, aux);
    // stores mutate memory for a specific slot, bump its version so subsequent loads
    // get different GVN keys and won't be merged with pre-store loads
    if (op == .store) {
        try ir.bumpMemVersion(allocator, aux);
    }
    return id;
}

fn isConst(ir: *Ir, id: u32) ?i64 {
    const ops = ir.m.instructions.items(.op);
    if (id >= ops.len) {
        return null;
    }
    if (ops[id] != .iconst) {
        return null;
    }
    const aux = ir.m.instructions.items(.aux)[id];
    return ir.m.constants.items[aux];
}

fn slotFor(ir: *Ir, allocator: std.mem.Allocator, name: []const u8) std.mem.Allocator.Error!u32 {
    const gop = try ir.m.name_to_slot.getOrPut(allocator, name);
    if (!gop.found_existing) {
        gop.value_ptr.* = ir.m.slots;
        ir.m.slots += 1;
    }
    return gop.value_ptr.*;
}

fn memVersionForSlot(ir: *Ir, slot: u32) u32 {
    return ir.memver.get(slot) orelse 0;
}

fn bumpMemVersion(ir: *Ir, allocator: std.mem.Allocator, slot: u32) std.mem.Allocator.Error!void {
    const cur = ir.memver.get(slot) orelse 0;
    try ir.memver.put(allocator, slot, cur + 1);
}

fn lowerAssign(ir: *Ir, allocator: std.mem.Allocator, index: u32) std.mem.Allocator.Error!void {
    const n = ir.ast.nodes.items[index];
    std.debug.assert(n.tag == .assign);
    const lhs_index = n.data.binary_op.lhs;
    const rhs_index = n.data.binary_op.rhs;
    const lhs = ir.ast.nodes.items[lhs_index];
    const value = try ir.lowerExpression(allocator, rhs_index);
    if (lhs.tag != .identifier) {
        return; // malformed
    }
    const name = ir.ast.tokenSlice(lhs.token_start, ir.source);
    const slot = try ir.slotFor(allocator, name);
    _ = try ir.emit(allocator, .store, value, 0, slot);
}

fn lowerRoot(ir: *Ir, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    if (ir.ast.nodes.items.len == 0) {
        return;
    }
    const nodes = ir.ast.nodes.items;
    const root = nodes[0];
    std.debug.assert(root.tag == .root);
    var i = root.data.list.start;
    const end = i + root.data.list.len;
    while (i < end) : (i += 1) {
        const node = nodes[i];
        if (node.tag == .assign) {
            try ir.lowerAssign(allocator, i);
        } else if (node.tag != .root and node.tag != .@"error") {
            _ = try ir.lowerExpression(allocator, i);
        }
    }
}

fn lowerExpression(ir: *Ir, allocator: std.mem.Allocator, index: u32) std.mem.Allocator.Error!u32 {
    const n = ir.ast.nodes.items[index];
    return switch (n.tag) {
        .number_literal => {
            const s = ir.ast.tokenSlice(n.token_start, ir.source);
            const v = std.fmt.parseInt(i64, s, 0) catch 0;
            const k = try ir.internConst(allocator, v);
            return try ir.emit(allocator, .iconst, 0, 0, k);
        },
        .identifier => {
            const name = ir.ast.tokenSlice(n.token_start, ir.source);
            const slot = try ir.slotFor(allocator, name);
            return try ir.emit(allocator, .load, 0, 0, slot);
        },
        .add, .sub, .mul, .div, .mod, .less, .less_equal, .greater, .greater_equal, .equal_equal, .not_equal, .@"and", .@"or", .pipe, .pipe_greater, .less_pipe, .range => {
            const lhs = try ir.lowerExpression(allocator, n.data.binary_op.lhs);
            const rhs = try ir.lowerExpression(allocator, n.data.binary_op.rhs);
            // fold if both const
            if (ir.isConst(lhs)) |left| if (ir.isConst(rhs)) |right| {
                const folded = operations.tryFold(nodeToOperation(n.tag), left, right);
                if (folded) |value| {
                    const k = try ir.internConst(allocator, value);
                    return try ir.emit(allocator, .iconst, 0, 0, k);
                }
            };
            return try ir.emit(allocator, nodeToOperation(n.tag), lhs, rhs, 0);
        },
        .@"error" => err: {
            const k = try ir.internConst(allocator, 0);
            break :err try ir.emit(allocator, .iconst, 0, 0, k);
        },
        .assign, .root => unreachable,
    };
}

fn nodeToOperation(tag: Ast.Node.Tag) operations.Op {
    return switch (tag) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .mod => .mod,

        .less => .less,
        .less_equal => .less_equal,
        .greater => .greater,
        .greater_equal => .greater_equal,

        .equal_equal => .equal_equal,
        .not_equal => .not_equal,

        .@"and" => .@"and",
        .@"or" => .@"or",

        .pipe => .pipe,
        .pipe_greater => .pipe_greater,
        .less_pipe => .less_pipe,

        .range => .range,
        else => unreachable,
    };
}

pub const Module = struct {
    instructions: std.MultiArrayList(operations.IrInstruction) = .empty,
    constants: std.ArrayList(i64) = .empty,
    name_to_slot: std.StringArrayHashMapUnmanaged(u32) = .empty,
    slots: u32 = 0,

    pub const empty: Module = .{};

    pub fn init(allocator: std.mem.Allocator, ast: *const Ast, source: []const u8) std.mem.Allocator.Error!Module {
        var m: Module = .empty;
        errdefer m.deinit(allocator);
        var ir: Ir = .init(ast, source, &m);
        defer ir.deinit(allocator);
        try ir.lowerRoot(allocator);
        return m;
    }

    pub fn deinit(module: *Module, allocator: std.mem.Allocator) void {
        module.instructions.deinit(allocator);
        module.constants.deinit(allocator);
        module.name_to_slot.deinit(allocator);
        module.* = undefined;
    }

    pub fn disassemble(module: *const Module, w: *std.Io.Writer) std.Io.Writer.Error!void {
        const ops = module.instructions.items(.op);
        const as_ = module.instructions.items(.a);
        const bs = module.instructions.items(.b);
        const aux = module.instructions.items(.aux);

        try w.writeAll("ir:\n");
        var i: usize = 0;
        while (i < ops.len) : (i += 1) {
            const op = ops[i];
            switch (op) {
                .iconst => try w.print("{d}: {s} c{d}  ; {d}\n", .{
                    i,
                    operations.mnemonic(op),
                    aux[i],
                    module.constants.items[aux[i]],
                }),
                .load => try w.print("{d}: {s} r{d} <- slot[{d}]\n", .{
                    i,
                    operations.mnemonic(op),
                    i,
                    aux[i],
                }),
                .store => try w.print("{d}: {s} slot[{d}] <- r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    aux[i],
                    as_[i],
                }),
                .mov => try w.print("{d}: {s} r{d} <- r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    i,
                    as_[i],
                }),
                else => try w.print("{d}: {s} r{d} <- r{d}, r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    i,
                    as_[i],
                    bs[i],
                }),
            }
        }
    }
};

pub const Program = struct {
    code: std.MultiArrayList(operations.BytecodeInstruction) = .empty,
    constants: std.ArrayList(i64) = .empty,
    vars: u32 = 0,
    regs: u32 = 0,

    pub const empty: Program = .{};

    pub fn init(allocator: std.mem.Allocator, ir: *const Module) std.mem.Allocator.Error!Program {
        var p: Program = .empty;
        errdefer p.deinit(allocator);
        // copy const pool
        try p.constants.ensureTotalCapacity(allocator, ir.constants.items.len);
        for (ir.constants.items) |v| p.constants.appendAssumeCapacity(v);

        p.vars = ir.slots;
        p.regs = @intCast(ir.instructions.len);

        try p.code.ensureTotalCapacity(allocator, ir.instructions.len);

        const ir_ops = ir.instructions.items(.op);
        const ir_as = ir.instructions.items(.a);
        const ir_bs = ir.instructions.items(.b);
        const ir_aux = ir.instructions.items(.aux);

        var i: u32 = 0;
        while (i < ir_ops.len) : (i += 1) {
            p.code.appendAssumeCapacity(.{
                .op = ir_ops[i],
                .dst = i,
                .a = ir_as[i],
                .b = ir_bs[i],
                .aux = ir_aux[i],
            });
        }
        return p;
    }

    pub fn deinit(program: *Program, allocator: std.mem.Allocator) void {
        program.code.deinit(allocator);
        program.constants.deinit(allocator);
        program.* = undefined;
    }

    pub fn disassemble(program: *const Program, w: *std.Io.Writer) std.Io.Writer.Error!void {
        const ops = program.code.items(.op);
        const dsts = program.code.items(.dst);
        const as = program.code.items(.a);
        const bs = program.code.items(.b);
        const aux = program.code.items(.aux);

        try w.writeAll("bytecode:\n");
        var i: usize = 0;
        while (i < ops.len) : (i += 1) {
            const op = ops[i];
            switch (op) {
                .iconst => try w.print("{d}: {s} r{d} <- c{d} ; {d}\n", .{
                    i,
                    operations.mnemonic(op),
                    dsts[i],
                    aux[i],
                    program.constants.items[aux[i]],
                }),
                .load => try w.print("{d}: {s} r{d} <- slot[{d}]\n", .{
                    i,
                    operations.mnemonic(op),
                    dsts[i],
                    aux[i],
                }),
                .store => try w.print("{d}: {s} slot[{d}] <- r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    aux[i],
                    as[i],
                }),
                .mov => try w.print("{d}: {s} r{d} <- r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    dsts[i],
                    as[i],
                }),
                else => try w.print("{d}: {s} r{d} <- r{d}, r{d}\n", .{
                    i,
                    operations.mnemonic(op),
                    dsts[i],
                    as[i],
                    bs[i],
                }),
            }
        }
        try w.print("vars={d}, regs={d}\n", .{ program.vars, program.regs });
    }
};

test "constant interning and iconst GVN dedupe" {
    const allocator = std.testing.allocator;

    var m: Module = .empty;
    defer m.deinit(allocator);

    var dummy_ast: Ast = undefined;
    var ir: Ir = .init(&dummy_ast, "", &m);
    defer ir.deinit(allocator);

    const k0 = try ir.internConst(allocator, 42);
    const id0 = try emit(&ir, allocator, .iconst, 0, 0, k0);

    const k1 = try ir.internConst(allocator, 42);
    const id1 = try emit(&ir, allocator, .iconst, 0, 0, k1);
    // same const value → same const index
    try std.testing.expectEqual(k0, k1);
    // pure op + same (op, a, b, aux) → GVN hit, same instruction id
    try std.testing.expectEqual(id0, id1);
    // only one const in the pool, one instruction emitted
    try std.testing.expectEqual(@as(usize, 1), m.constants.items.len);
    try std.testing.expectEqual(@as(usize, 1), m.instructions.len);
    // make sure they really refer to the same const slot
    const aux = m.instructions.items(.aux);
    try std.testing.expectEqual(k0, aux[id0]);
}

test "commutative canonicalization + GVN hit for add" {
    const allocator = std.testing.allocator;

    var m: Module = .empty;
    defer m.deinit(allocator);

    var dummy_ast: Ast = undefined;
    var ir: Ir = .init(&dummy_ast, "", &m);
    defer ir.deinit(allocator);

    // r0 <- 2
    const k2 = try ir.internConst(allocator, 2);
    const r0 = try emit(&ir, allocator, .iconst, 0, 0, k2);

    // r1 <- 7
    const k7 = try ir.internConst(allocator, 7);
    const r1 = try emit(&ir, allocator, .iconst, 0, 0, k7);
    // add with reversed operands, should canonicalize and GVN to the same id
    const add_a_b = try emit(&ir, allocator, .add, r0, r1, 0);
    const add_b_a = try emit(&ir, allocator, .add, r1, r0, 0);

    try std.testing.expectEqual(add_a_b, add_b_a);
    // 2 iconsts + 1 add
    try std.testing.expectEqual(@as(usize, 3), m.instructions.len);
    // the stored operands should be ordered (canonicalized a <= b)
    const as = m.instructions.items(.a);
    const bs = m.instructions.items(.b);
    try std.testing.expect(as[add_a_b] <= bs[add_a_b]);
}

test "store is impure and never GVN'd" {
    const allocator = std.testing.allocator;

    var m: Module = .empty;
    defer m.deinit(allocator);

    var dummy_ast: Ast = undefined;
    var ir: Ir = .init(&dummy_ast, "", &m);
    defer ir.deinit(allocator);

    const k1 = try ir.internConst(allocator, 1);
    const v = try emit(&ir, allocator, .iconst, 0, 0, k1);

    // store is impure -> no GVN, each emit produces a new id
    const s0 = try emit(&ir, allocator, .store, v, 0, 0);
    const s1 = try emit(&ir, allocator, .store, v, 0, 0);

    try std.testing.expect(s0 != s1);
    // iconst + 2 stores
    try std.testing.expectEqual(@as(usize, 3), m.instructions.len);
}

test "code emission, dst mapping and runtime evaluation" {
    const allocator = std.testing.allocator;

    var m: Module = .empty;
    defer m.deinit(allocator);

    var dummy_ast: Ast = undefined;
    var ir: Ir = .init(&dummy_ast, "", &m);
    defer ir.deinit(allocator);

    // const pool
    const k5 = try ir.internConst(allocator, 5);
    const k3 = try ir.internConst(allocator, 3);

    // r0 <- 5
    const r0 = try emitRaw(&ir, allocator, .iconst, 0, 0, k5);
    // r1 <- 3
    const r1 = try emitRaw(&ir, allocator, .iconst, 0, 0, k3);
    // r2 <- r0 + r1 = 8
    const r2 = try emitRaw(&ir, allocator, .add, r0, r1, 0);
    // r3 <- r2 * r1 = 24
    const r3 = try emitRaw(&ir, allocator, .mul, r2, r1, 0);

    // globals[0] <- r3
    _ = try emitRaw(&ir, allocator, .store, r3, 0, 0);
    m.slots = 1;

    // r5 <- globals[0] (=24)
    const r5 = try emitRaw(&ir, allocator, .load, 0, 0, 0);

    // r6 <- r0 / r1 (= 1)
    const r6 = try emitRaw(&ir, allocator, .div, r0, r1, 0);
    // r7 <- r0 % r1 (= 2)
    const r7 = try emitRaw(&ir, allocator, .mod, r0, r1, 0);

    // r8 <- r0 > r1 (= true -> 1)
    const r8 = try emitRaw(&ir, allocator, .greater, r0, r1, 0);
    // r9 <- r0 == r0 (= 1)
    const r9 = try emitRaw(&ir, allocator, .equal_equal, r0, r0, 0);

    // r10 <- r0 & r1 (= 1)
    const r10 = try emitRaw(&ir, allocator, .@"and", r0, r1, 0);
    // r11 <- r0 | r1 (= 7)
    const r11 = try emitRaw(&ir, allocator, .@"or", r0, r1, 0);
    // r12 <- pipe (alias OR) (= 7)
    const r12 = try emitRaw(&ir, allocator, .pipe, r0, r1, 0);
    // r13 <- |> (alias OR) (= 7)
    const r13 = try emitRaw(&ir, allocator, .pipe_greater, r0, r1, 0);
    // r14 <- <| (alias OR) (= 7)
    const r14 = try emitRaw(&ir, allocator, .less_pipe, r0, r1, 0);
    // r15 <- range(r0, r1) (= [5..3])
    const r15 = try emitRaw(&ir, allocator, .range, r0, r1, 0);
    // sanity on reg ids and instruction count
    try std.testing.expectEqual(@as(u32, 0), r0);
    try std.testing.expectEqual(@as(u32, 1), r1);
    try std.testing.expectEqual(@as(u32, 2), r2);
    try std.testing.expectEqual(@as(u32, 3), r3);
    try std.testing.expectEqual(@as(usize, 16), m.instructions.len);
    var p: Program = try .init(allocator, &m);
    defer p.deinit(allocator);
    // dst should map 1:1 to IR index; regs should equal instruction count
    try std.testing.expectEqual(@as(u32, @intCast(m.instructions.len)), p.regs);
    const dsts = p.code.items(.dst);
    for (0..dsts.len) |i| {
        try std.testing.expectEqual(@as(u32, @intCast(i)), dsts[i]);
    }

    var st: libn.State = try .init(allocator, &p);
    defer st.deinit(allocator);

    try std.testing.expectEqual(libn.Value.int(8), st.regs[r2]);
    try std.testing.expectEqual(libn.Value.int(24), st.regs[r3]);
    try std.testing.expectEqual(libn.Value.int(24), st.regs[r5]);
    try std.testing.expectEqual(libn.Value.int(1), st.regs[r6]);
    try std.testing.expectEqual(libn.Value.int(2), st.regs[r7]);
    try std.testing.expectEqual(libn.Value.boolean(true), st.regs[r8]);
    try std.testing.expectEqual(libn.Value.boolean(true), st.regs[r9]);
    try std.testing.expectEqual(libn.Value.int(1), st.regs[r10]);
    try std.testing.expectEqual(libn.Value.int(7), st.regs[r11]);
    try std.testing.expectEqual(libn.Value.int(7), st.regs[r12]);
    try std.testing.expectEqual(libn.Value.int(7), st.regs[r13]);
    try std.testing.expectEqual(libn.Value.int(7), st.regs[r14]);
    try std.testing.expectEqual(libn.Value.range(5, 3), st.regs[r15]);
}

test "div and mod by zero produce 0" {
    const allocator = std.testing.allocator;

    var m: Module = .empty;
    defer m.deinit(allocator);

    var dummy_ast: Ast = undefined;
    var ir: Ir = .init(&dummy_ast, "", &m);
    defer ir.deinit(allocator);

    const k5 = try ir.internConst(allocator, 5);
    const k0 = try ir.internConst(allocator, 0);

    const r0 = try emitRaw(&ir, allocator, .iconst, 0, 0, k5);
    const r1 = try emitRaw(&ir, allocator, .iconst, 0, 0, k0);

    const rd = try emitRaw(&ir, allocator, .div, r0, r1, 0);
    const rm = try emitRaw(&ir, allocator, .mod, r0, r1, 0);

    var p = try Program.init(allocator, &m);
    defer p.deinit(allocator);

    var st: libn.State = try .init(allocator, &p);
    defer st.deinit(allocator);

    try std.testing.expectEqual(libn.Value.int(0), st.regs[rd]);
    try std.testing.expectEqual(libn.Value.int(0), st.regs[rm]);
}

const std = @import("std");
const operations = @import("operations.zig");
const Ast = @import("Ast.zig");
const libn = @import("root.zig");
const Ir = @This();
