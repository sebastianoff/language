ast: *const Ast,
source: []const u8,
tokens: std.MultiArrayList(tokenizer.Token).Slice,
/// name -> first-declaration token index (slice into `source`)
declarations: std.StringArrayHashMapUnmanaged(u32) = .empty,
diagnostics: std.ArrayList(Diagnostic) = .empty,

pub fn init(ast: *const Ast, source: []const u8) Sema {
    return .{ .ast = ast, .source = source, .tokens = ast.tokens.slice() };
}

pub fn deinit(sema: *Sema, allocator: std.mem.Allocator) void {
    sema.diagnostics.deinit(allocator);
    sema.declarations.deinit(allocator);
    sema.* = undefined;
}

pub fn walk(sema: *Sema, allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
    if (sema.ast.nodes.items.len == 0) {
        return;
    }
    const nodes = sema.ast.nodes.items;
    const root = nodes[0];
    std.debug.assert(root.tag == .root);
    // reserve since we don't want to rehash during the loop
    var assign: usize = 0;
    {
        var i = root.data.list.start;
        const end = i + root.data.list.len;
        while (i < end) : (i += 1) {
            if (nodes[i].tag == .assign) {
                assign += 1;
            }
        }
    }
    _ = try sema.declarations.ensureTotalCapacity(allocator, assign);
    try sema.diagnostics.ensureTotalCapacity(allocator, @max(assign, 8));
    var i = root.data.list.start;
    const end = i + root.data.list.len;
    while (i < end) : (i += 1) {
        const node = nodes[i];
        if (node.tag != .assign) {
            continue;
        }
        const lhs = node.data.binary_op.lhs;
        const rhs = node.data.binary_op.rhs;
        // lhs declares or redeclares
        try sema.declare(allocator, lhs);
        // rhs uses
        try sema.checkExpressionUses(allocator, rhs);
    }
}

/// declare a name from an identifier node, record redeclarations
fn declare(sema: *Sema, allocator: std.mem.Allocator, lhs: Ast.Node.Index) std.mem.Allocator.Error!void {
    const node = sema.ast.nodes.items[lhs];
    if (node.tag != .identifier) {
        return; // malformed lhs
    }
    const index = node.token_start;
    const name = sema.tokenSlice(index);
    const gop = try sema.declarations.getOrPut(allocator, name);
    if (!gop.found_existing) {
        gop.value_ptr.* = index;
    } else {
        try sema.diagnostics.append(allocator, .{
            .tag = .sema_redeclaration,
            .index = index,
            .got = sema.tokens.items(.tag)[index],
        });
    }
}

/// check expression subtree for identifier uses; lhs of an assignment is not considered a use
fn checkExpressionUses(sema: *Sema, allocator: std.mem.Allocator, index: Ast.Node.Index) std.mem.Allocator.Error!void {
    const node = sema.ast.nodes.items[index];
    switch (node.tag) {
        .number_literal, .@"error" => return,
        .identifier => {
            const token = node.token_start;
            const name = sema.tokenSlice(token);
            if (sema.declarations.get(name) == null) {
                try sema.diagnostics.append(
                    allocator,
                    .{
                        .tag = .sema_undeclared_identifier,
                        .index = token,
                        .got = sema.tokens.items(.tag)[token],
                    },
                );
            }
        },
        .assign => {
            // only the rhs is a use
            try sema.checkExpressionUses(allocator, node.data.binary_op.rhs);
        },
        .root => {}, // not expected inside expressions
    }
}

fn tokenSlice(sema: *Sema, index: u32) []const u8 {
    return sema.tokens.items(.span)[index].slice(sema.source);
}

test "empty input -> no diagnostics" {
    const allocator = std.testing.allocator;

    var ast = try Ast.initRoot(allocator, "");
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, "");
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "single assignment (x=1;) -> ok" {
    const allocator = std.testing.allocator;
    const source = "x=1;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "identifiers on RHS - undeclared (x=y;)" {
    const allocator = std.testing.allocator;
    const source = "x=y;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 1), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_undeclared_identifier, sema.diagnostics.items[0].tag);
    // 0:'x', 1:'=', 2:'y', 3:';'
    try std.testing.expectEqual(@as(u32, 2), sema.diagnostics.items[0].index);
}

test "declare then use later (y=1; x=y;) -> ok" {
    const allocator = std.testing.allocator;
    const source = "y=1; x=y;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "self-reference allowed within same assignment (y=y;)" {
    const allocator = std.testing.allocator;
    const source = "y=y;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "use-before-declare across statements (x=y; y=1;) -> one undeclared" {
    const allocator = std.testing.allocator;
    const source = "x=y; y=1;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 1), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_undeclared_identifier, sema.diagnostics.items[0].tag);
    // 0:'x',1:'=',2:'y',3:';', 4:'y',5:'=',6:'1',7:';'
    try std.testing.expectEqual(@as(u32, 2), sema.diagnostics.items[0].index);
}

test "redeclaration simple (x=1; x=2;)" {
    const allocator = std.testing.allocator;
    const source = "x=1; x=2;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 1), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_redeclaration, sema.diagnostics.items[0].tag);
    //  0:'x',1:'=',2:'1',3:';', 4:'x',5:'=',6:'2',7:';'
    try std.testing.expectEqual(@as(u32, 4), sema.diagnostics.items[0].index);
}

test "redeclaration multiple (x=1; x=2; x=3;)" {
    const allocator = std.testing.allocator;
    const source = "x=1; x=2; x=3;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 2), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_redeclaration, sema.diagnostics.items[0].tag);
    try std.testing.expectEqual(Diagnostic.Tag.sema_redeclaration, sema.diagnostics.items[1].tag);
}

test "redeclaration after other decls (x=1; y=2; x=3;)" {
    const allocator = std.testing.allocator;
    const source = "x=1; y=2; x=3;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 1), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_redeclaration, sema.diagnostics.items[0].tag);
}

test "redeclaration with comment between tokens (x//c\\n=1; x=2;)" {
    const allocator = std.testing.allocator;
    const source = "x//c\n=1; x=2;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);

    try std.testing.expectEqual(@as(usize, 1), sema.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.sema_redeclaration, sema.diagnostics.items[0].tag);
    // 0:'x',1:comment,2:'=',3:'1',4:';', 5:'x',...
    try std.testing.expectEqual(@as(u32, 5), sema.diagnostics.items[0].index);
}

test "case sensitivity (x vs X are different names)" {
    const allocator = std.testing.allocator;
    const source = "x=1; X=2;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "non-assignment statements ignored (42;)" {
    const allocator = std.testing.allocator;
    const source = "42;";

    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
    try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
}

test "parser error cases on RHS do not produce sema errors" {
    const allocator = std.testing.allocator;

    // missing RHS expression
    {
        const source = "x=;";
        var ast: Ast = try .initRoot(allocator, source);
        defer ast.deinit(allocator);
        var sema: Sema = .init(&ast, source);
        defer sema.deinit(allocator);
        try sema.walk(allocator);
        try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
    }
    // invalid character
    {
        const source = "x=&;";
        var ast: Ast = try .initRoot(allocator, source);
        defer ast.deinit(allocator);
        var sema: Sema = .init(&ast, source);
        defer sema.deinit(allocator);
        try sema.walk(allocator);
        try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
    }
    // invalid number tail
    {
        const source = "x=1a;";
        var ast: Ast = try .initRoot(allocator, source);
        defer ast.deinit(allocator);
        var sema: Sema = .init(&ast, source);
        defer sema.deinit(allocator);
        try sema.walk(allocator);
        try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
    }
    // missing semicolon at EOF
    {
        const source = "x=1";
        var ast: Ast = try .initRoot(allocator, source);
        defer ast.deinit(allocator);
        var sema: Sema = .init(&ast, source);
        defer sema.deinit(allocator);
        try sema.walk(allocator);
        try std.testing.expectEqual(@as(usize, 0), sema.diagnostics.items.len);
    }
}

fn allocSafeSemaParsed(allocator: std.mem.Allocator, source: []const u8) !void {
    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);

    var sema: Sema = .init(&ast, source);
    defer sema.deinit(allocator);

    try sema.walk(allocator);
}

test "allocation failure (parsed, redeclaration path)" {
    const allocator = std.testing.allocator;
    const source = "x=1; x=2; y=3; z=4;";
    try std.testing.checkAllAllocationFailures(allocator, allocSafeSemaParsed, .{source});
}

test "allocation failure (parsed, undeclared identifier path)" {
    const allocator = std.testing.allocator;
    const source = "x=y; a=b; c=1;";
    try std.testing.checkAllAllocationFailures(allocator, allocSafeSemaParsed, .{source});
}

const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Diagnostic = @import("Diagnostic.zig");
const Ast = @import("Ast.zig");
const Sema = @This();
