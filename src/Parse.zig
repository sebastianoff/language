//! n frontend. Parse.
ast: *Ast,
cursor: u32 = 0,
tokens: std.MultiArrayList(tokenizer.Token).Slice,
diagnostics: std.ArrayList(Diagnostic) = .empty,

pub fn init(ast: *Ast) Parse {
    return .{ .ast = ast, .tokens = ast.tokens.slice() };
}

pub fn deinit(parse: *Parse, allocator: std.mem.Allocator) void {
    parse.diagnostics.deinit(allocator);
}

pub const TokenInfo = struct {
    tag: tokenizer.Token.Tag,
    index: u32,
    /// Inserted/missing token.
    synthetic: bool = false,
};

pub const sets = struct {
    pub const statement_stop = &.{ .semicolon, .invalid_character, .identifier };
    pub const rhs_stop = &.{ .semicolon, .invalid_character, .identifier };

    pub fn inSet(comptime set: []const tokenizer.Token.Tag, tag: tokenizer.Token.Tag) bool {
        inline for (set) |t| {
            if (t == tag) return true;
        }
        return false;
    }
};

pub fn appendError(parse: *Parse, allocator: std.mem.Allocator, tag: Diagnostic.Tag, at: TokenInfo) void {
    if (parse.diagnostics.items.len >= 64) {
        // force advance to eof to stop.
        parse.cursor = @intCast(parse.tokens.len);
        return;
    }
    parse.diagnostics.append(allocator, .{ .tag = tag, .index = at.index, .got = at.tag }) catch return;
}

pub fn appendNode(parse: *Parse, allocator: std.mem.Allocator, node: Ast.Node) std.mem.Allocator.Error!Ast.Node.Index {
    const index: u32 = @intCast(parse.ast.nodes.items.len);
    try parse.ast.nodes.append(allocator, node);
    return index;
}

/// scan forward to any token in `stop`.
fn recoverUntil(parse: *Parse, comptime stop: []const tokenizer.Token.Tag, mode: enum { keep, consume }) TokenInfo {
    var index = parse.cursor;
    const tags = parse.tokens.items(.tag);
    while (index < tags.len) : (index += 1) {
        const tag = tags[index];
        if (tag != .comment and sets.inSet(stop, tag)) {
            break;
        }
    }
    // position the cursor and return the token we stopped at, or eof
    if (mode == .consume and index < tags.len) {
        parse.cursor = index + 1;
    } else {
        parse.cursor = index;
    }
    if (index >= tags.len) {
        return .{ .tag = .invalid_character, .index = @intCast(tags.len), .synthetic = false };
    }
    return .{ .tag = tags[index], .index = index, .synthetic = false };
}

/// expect a token; on failure either insert it or panic to sync set.
fn expect(
    parse: *Parse,
    allocator: std.mem.Allocator,
    want: tokenizer.Token.Tag,
    comptime sync: []const tokenizer.Token.Tag,
    mode: enum { insert, panic },
) TokenInfo {
    const token = parse.peek(.{});
    if (token.tag == want) {
        parse.cursor = token.index + 1;
        return token;
    }
    // record
    const tag: Diagnostic.Tag = switch (want) {
        .identifier => .parse_expected_identifier,
        .equal => .parse_expected_equal,
        .semicolon => .parse_expected_semicolon,
        else => .parse_expected_expression,
    };
    parse.appendError(allocator, tag, token);
    if (mode == .insert) {
        // do not advance
        return .{ .tag = want, .index = token.index, .synthetic = true };
    }
    // scan forward to sync, but don't consume the sync token itself
    const stop = parse.recoverUntil(sync, .keep);
    return .{ .tag = want, .index = stop.index, .synthetic = true };
}

pub fn statement(parse: *Parse, allocator: std.mem.Allocator) std.mem.Allocator.Error!Ast.Node.Index {
    if (parse.peek(.{}).tag == .identifier and parse.peek(.{ .offset = 1 }).tag == .equal) {
        return parse.assignmentStatement(allocator);
    }
    // error
    const at = parse.peek(.{});
    parse.appendError(allocator, .parse_expected_statement, at);
    _ = parse.recoverUntil(sets.statement_stop, .consume);
    return parse.appendNode(allocator, .{
        .tag = .@"error",
        .token_start = at.index,
        .token_len = 0,
        .data = .{ .none = {} },
    });
}
pub fn atEnd(parse: *Parse) bool {
    return parse.peek(.{}).tag == .invalid_character;
}

fn assignmentStatement(parse: *Parse, allocator: std.mem.Allocator) !Ast.Node.Index {
    const start = parse.peek(.{}).index;
    // lhs
    const lhs_token = parse.expect(allocator, .identifier, sets.statement_stop, .panic);
    const lhs_node = if (lhs_token.synthetic)
        try parse.appendNode(allocator, .{
            .tag = .@"error",
            .token_start = lhs_token.index,
            .token_len = 0,
            .data = .{ .none = {} },
        })
    else
        try parse.appendNode(allocator, .{
            .tag = .identifier,
            .token_start = lhs_token.index,
            .token_len = 1,
            .data = .{ .none = {} },
        });

    // '='
    _ = parse.expect(allocator, .equal, sets.statement_stop, .insert);
    // rhs
    const rhs_node = try parse.primaryExpression(allocator, sets.rhs_stop);
    // ';'
    _ = parse.expect(allocator, .semicolon, sets.statement_stop, .insert);
    // if nothing advanced, consume one sync token
    if (parse.cursor == start and !parse.atEnd()) {
        _ = parse.recoverUntil(sets.statement_stop, .consume);
    }
    const end = parse.cursor;
    return try parse.appendNode(
        allocator,
        .{
            .tag = .assign,
            .token_start = start,
            .token_len = @intCast(end - start),
            .data = .{ .binary_op = .{ .lhs = lhs_node, .rhs = rhs_node } },
        },
    );
}

fn primaryExpression(
    parse: *Parse,
    allocator: std.mem.Allocator,
    comptime sync: []const tokenizer.Token.Tag,
) std.mem.Allocator.Error!Ast.Node.Index {
    const before = parse.peek(.{});
    if (parse.match(&.{ .number, .identifier })) |token| {
        const tag: Ast.Node.Tag = switch (token.tag) {
            .number => .number_literal,
            .identifier => .identifier,
            else => unreachable,
        };
        return parse.appendNode(allocator, .{
            .tag = tag,
            .token_start = token.index,
            .token_len = 1,
            .data = .{ .none = {} },
        });
    }

    parse.appendError(allocator, .parse_expected_expression, before);
    if (before.tag != .semicolon) {
        _ = parse.recoverUntil(sync, .consume);
    }
    return try parse.appendNode(
        allocator,
        .{ .tag = .@"error", .token_start = before.index, .token_len = 0, .data = .{ .none = {} } },
    );
}

fn peek(parse: *Parse, options: struct { offset: u32 = 0 }) TokenInfo {
    const tags = parse.tokens.items(.tag);
    var index = parse.cursor;
    var i: u32 = 0;
    while (i <= options.offset) {
        // skip comments
        while (index < tags.len and tags[index] == .comment) {
            index += 1;
        }
        if (i == options.offset) break;
        // if we are at the end after skipping comments, we can't advance further.
        if (index >= tags.len) break;

        index += 1;
        i += 1;
    }

    if (index >= tags.len) {
        return .{ .tag = .invalid_character, .index = @intCast(tags.len) };
    }
    return .{ .tag = tags[index], .index = index };
}

fn match(parse: *Parse, tags: []const tokenizer.Token.Tag) ?TokenInfo {
    const token = parse.peek(.{});
    for (tags) |tag| {
        if (token.tag == tag) {
            parse.cursor = token.index + 1;
            return token;
        }
    }
    return null;
}

fn expectNodeBasic(n: Ast.Node, tag: Ast.Node.Tag, token_start: u32, token_len: u16) !void {
    try std.testing.expectEqual(tag, n.tag);
    try std.testing.expectEqual(token_start, n.token_start);
    try std.testing.expectEqual(token_len, n.token_len);
}

test "simple assignment x=1;" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=1;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1); // token 0: identifier x
    try expectNodeBasic(num, .number_literal, 2, 1); // token 2: number 1
    try expectNodeBasic(asn, .assign, 0, 4); // spanning tokens [0..4)

    try std.testing.expectEqual(@as(Ast.Node.Index, 0), asn.data.binary_op.lhs);
    try std.testing.expectEqual(@as(Ast.Node.Index, 1), asn.data.binary_op.rhs);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "assignment with comment between identifier and = (x//c\\n=1;)" {
    const allocator = std.testing.allocator;

    // Tokens: 0:id(x), 1:comment, 2:'=', 3:'1', 4:';'
    const tokens = try tokenizer.tokenize(allocator, "x//c\n=1;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(num, .number_literal, 3, 1);
    try expectNodeBasic(asn, .assign, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "assignment missing semicolon (x=1)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=1");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(num, .number_literal, 2, 1);
    try expectNodeBasic(asn, .assign, 0, 3); // no semicolon, end at tokens.len

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_semicolon, parse.diagnostics.items[0].tag);
}

test "missing RHS (x = ;)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const err = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(err, .@"error", 2, 0); // error at semicolon
    try expectNodeBasic(asn, .assign, 0, 3);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_expression, parse.diagnostics.items[0].tag);
}

test "invalid RHS token (x = &;)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=&;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const err = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(err, .@"error", 2, 0); // error at invalid_character '&'
    try expectNodeBasic(asn, .assign, 0, 4);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_expression, parse.diagnostics.items[0].tag);
}

test "statement starting with a number" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "42;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 1), ast.nodes.items.len);

    const err = ast.nodes.items[0];
    try expectNodeBasic(err, .@"error", 0, 0);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_statement, parse.diagnostics.items[0].tag);
}

const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Diagnostic = @import("Diagnostic.zig");
const Ast = @import("Ast.zig");
const Parse = @This();
