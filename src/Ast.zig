//! n frontend. Abstract Syntax Tree.
tokens: std.MultiArrayList(tokenizer.Token) = .empty,
nodes: std.ArrayList(Node) = .empty,

pub const init: Ast = .{};

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
const Ast = @This();
