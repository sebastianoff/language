//! n frontend. Abstract Syntax Tree.
tokens: std.MultiArrayList(tokenizer.Token) = .empty,
nodes: std.ArrayList(Node) = .empty,

pub fn init(tokens: std.MultiArrayList(tokenizer.Token)) Ast {
    return .{ .tokens = tokens };
}

pub fn initRoot(allocator: std.mem.Allocator, source: []const u8) !Ast {
    // tokenize
    const tokens = try tokenizer.tokenize(allocator, source);
    // parse
    var ast: Ast = .init(tokens);
    errdefer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    // root @ 0.
    const root_index = try parse.appendNode(allocator, .{
        .tag = .root,
        .token_start = 0,
        .token_len = 0, // will be updated
        .data = .{ .list = .{ .start = 0, .len = 0 } }, // will be updated
    });
    std.debug.assert(root_index == 0);

    const list_start_index: u32 = @intCast(parse.ast.nodes.items.len);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }
    const list_end_index: u32 = @intCast(parse.ast.nodes.items.len);

    parse.ast.nodes.items[0].data.list.start = list_start_index;
    parse.ast.nodes.items[0].data.list.len = @intCast(list_end_index - list_start_index);
    parse.ast.nodes.items[0].token_len = @intCast(parse.tokens.len);

    return ast;
}

pub fn deinit(ast: *Ast, allocator: std.mem.Allocator) void {
    ast.tokens.deinit(allocator);
    ast.nodes.deinit(allocator);
    ast.* = undefined;
}

pub const Node = struct {
    /// the data payload of the node
    data: Data,
    /// the index of the first token this node represents.
    token_start: u32,
    /// the number of tokens in this node's span.
    token_len: u16,
    tag: Tag,
    /// a handle to a node in the `Ast.nodes`.
    pub const Index = u32;

    pub const Tag = enum(u8) {
        root,
        assign,
        identifier,
        number_literal,
    };

    pub const Data = extern union {
        /// for nodes that represent a list of other nodes (e.g., `root`).
        list: extern struct {
            start: Index,
            len: u32,
        },
        /// for binary operations like assignment.
        binary_op: extern struct {
            lhs: Index,
            rhs: Index,
        },
        none: void,
    };

    comptime {
        if (@sizeOf(Node) != 16) {
            @compileLog(.{ .expected = 16, .actual = @sizeOf(Node) }); // size mismatch
        }
    }
};

const tokenizer = @import("tokenizer.zig");
const std = @import("std");
const Parse = @import("Parse.zig");
const Ast = @This();
