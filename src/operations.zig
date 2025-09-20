pub const Op = enum(u8) {
    // data movement
    iconst, // dst <- consts[aux]
    load, // dst <- globals[aux]
    store, // globals[aux] <- a
    mov, // dst <- a
    // arithmetic
    add,
    sub,
    mul,
    div,
    mod,
    // comparisons (int -> bool as 0/1)
    less,
    less_equal,
    greater,
    greater_equal,
    equal_equal,
    not_equal,
    // boolean/pipeline family
    @"and",
    @"or",
    pipe,
    pipe_greater,
    less_pipe,
    // constructs a runtime range value
    range,
};

pub const IrInstruction = extern struct {
    op: Op,
    a: u32,
    b: u32,
    aux: u32,
};

pub const BytecodeInstruction = extern struct {
    op: Op,
    dst: u32,
    a: u32,
    b: u32,
    aux: u32,
};

pub inline fn isCommutative(op: Op) bool {
    return switch (op) {
        .add, .mul, .equal_equal, .not_equal, .@"and", .@"or", .pipe => true,
        else => false,
    };
}

pub inline fn isPure(op: Op) bool {
    return switch (op) {
        .store => false,
        else => true,
    };
}

pub inline fn mnemonic(op: Op) []const u8 {
    return switch (op) {
        .iconst => "iconst",
        .load => "load",
        .store => "store",
        .mov => "mov",
        .add => "add",
        .sub => "sub",
        .mul => "mul",
        .div => "div",
        .mod => "mod",
        .less => "lt",
        .less_equal => "le",
        .greater => "gt",
        .greater_equal => "ge",
        .equal_equal => "eq",
        .not_equal => "ne",
        .@"and" => "and",
        .@"or" => "or",
        .pipe => "pipe",
        .pipe_greater => "|>",
        .less_pipe => "<|",
        .range => "range",
    };
}

pub inline fn tryFold(op: Op, lhs: i64, rhs: i64) ?i64 {
    return switch (op) {
        .add => lhs + rhs,
        .sub => lhs - rhs,
        .mul => lhs * rhs,
        .div => if (rhs == 0) null else @divTrunc(lhs, rhs),
        .mod => if (rhs == 0) null else @mod(lhs, rhs),

        .less => if (lhs < rhs) 1 else 0,
        .less_equal => if (lhs <= rhs) 1 else 0,
        .greater => if (lhs > rhs) 1 else 0,
        .greater_equal => if (lhs >= rhs) 1 else 0,
        .equal_equal => if (lhs == rhs) 1 else 0,
        .not_equal => if (lhs != rhs) 1 else 0,

        .@"and" => lhs & rhs,
        .@"or" => lhs | rhs,
        .pipe => lhs | rhs,
        .pipe_greater => lhs | rhs,
        .less_pipe => lhs | rhs,

        .range, .iconst, .load, .store, .mov => null,
    };
}

const std = @import("std");
