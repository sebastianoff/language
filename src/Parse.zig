//! n frontend. Parse.
ast: *Ast,
cursor: u32 = 0,
tokens: std.MultiArrayList(tokenizer.Token).Slice,

pub fn init(ast: *Ast) Parse {
    return .{ .ast = ast, .tokens = ast.tokens.slice() };
}

pub const TokenInfo = struct {
    tag: tokenizer.Token.Tag,
    index: u32,
};

pub fn appendNode(parse: *Parse, allocator: std.mem.Allocator, node: Ast.Node) std.mem.Allocator.Error!Ast.Node.Index {
    const index: u32 = @intCast(parse.ast.nodes.items.len);
    try parse.ast.nodes.append(allocator, node);
    return index;
}

pub fn statement(parse: *Parse, allocator: std.mem.Allocator) !Ast.Node.Index {
    // look ahead to see if we have an assignment.
    if (parse.peek(.{}).tag == .identifier and parse.peek(.{ .offset = 1 }).tag == .equal) {
        return parse.assignmentStatement(allocator);
    }
    return error.ExpectedStatement;
}

pub fn atEnd(parse: *Parse) bool {
    return parse.peek(.{}).tag == .invalid_character;
}

fn assignmentStatement(parse: *Parse, allocator: std.mem.Allocator) !Ast.Node.Index {
    const start_token_index = parse.cursor;
    // lhs
    const lhs_token = try parse.expect(.identifier);
    const lhs_node = try parse.appendNode(allocator, .{ .tag = .identifier, .token_start = lhs_token.index, .token_len = 1, .data = .{ .none = {} } });
    // '='
    _ = try parse.expect(.equal);
    // rhs
    const rhs_node = try parse.primaryExpression(allocator);
    // ';'
    _ = try parse.expect(.semicolon);
    const end_token_index = parse.cursor;
    return parse.appendNode(
        allocator,
        .{
            .tag = .assign,
            .token_start = start_token_index,
            .token_len = @intCast(end_token_index - start_token_index),
            .data = .{ .binary_op = .{ .lhs = lhs_node, .rhs = rhs_node } },
        },
    );
}

fn primaryExpression(parse: *Parse, allocator: std.mem.Allocator) !Ast.Node.Index {
    if (parse.match(&.{.number})) |token| {
        return parse.appendNode(
            allocator,
            .{ .tag = .number_literal, .token_start = token.index, .token_len = 1, .data = .{ .none = {} } },
        );
    }
    return error.ExpectedExpression;
}

fn peek(parse: *Parse, options: struct { offset: u32 = 0 }) TokenInfo {
    var index = parse.cursor;
    var i: u32 = 0;
    while (i <= options.offset) {
        // skip comments
        while (index < parse.tokens.len and parse.tokens.items(.tag)[index] == .comment) {
            index += 1;
        }
        if (i == options.offset) break;
        // if we are at the end after skipping comments, we can't advance further.
        if (index >= parse.tokens.len) break;

        index += 1;
        i += 1;
    }

    if (index >= parse.tokens.len) {
        return .{ .tag = .invalid_character, .index = @intCast(parse.tokens.len) };
    }
    return .{ .tag = parse.tokens.items(.tag)[index], .index = index };
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

fn expect(parse: *Parse, tag: tokenizer.Token.Tag) !TokenInfo {
    const token = parse.peek(.{});
    if (token.tag == tag) {
        parse.cursor = token.index + 1;
        return token;
    }
    return switch (tag) {
        .identifier => error.ExpectedIdentifier,
        .equal => error.ExpectedEqualSign,
        .semicolon => error.ExpectedSemicolon,
        else => error.UnexpectedToken,
    };
}

const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Ast = @import("Ast.zig");
const Parse = @This();
