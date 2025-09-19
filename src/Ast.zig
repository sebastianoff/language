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
    defer parse.deinit(allocator);
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

    if (!builtin.is_test) {
        var map: Diagnostic.Map = try .init(allocator, source);
        defer map.deinit(allocator);

        const spans = ast.tokens.items(.span);
        const tok_len = ast.tokens.len;

        for (parse.diagnostics.items) |diagnostic| {
            const src_index: u32 = if (diagnostic.index < tok_len)
                spans[diagnostic.index].start
            else
                @intCast(source.len); // EOF

            const loc = map.lookup(src_index);
            std.log.err("{[line]d}:{[col]d}: error: {[diagnostic]s}", .{
                .line = loc.line,
                .col = loc.col,
                .diagnostic = @tagName(diagnostic.tag),
            });
        }
    }

    return ast;
}

pub fn deinit(ast: *Ast, allocator: std.mem.Allocator) void {
    ast.tokens.deinit(allocator);
    ast.nodes.deinit(allocator);
    ast.* = undefined;
}

pub fn render(ast: *const Ast, source: []const u8, w: *std.Io.Writer, options: Writer.Options) std.Io.Writer.Error!void {
    if (ast.nodes.items.len == 0) return;

    var writer: Writer = .{
        .ast = ast,
        .source = source,
        .w = w,
        .options = options,
        .indent = 0,
    };
    try writer.writeRoot();
}

pub const RenderError = std.Io.Writer.Error || std.mem.Allocator.Error;

pub fn renderToOwnedSlice(
    ast: *const Ast,
    allocator: std.mem.Allocator,
    source: []const u8,
    options: Writer.Options,
) RenderError![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try render(ast, source, &aw.writer, options);
    return try aw.toOwnedSlice();
}

pub const Writer = struct {
    ast: *const Ast,
    source: []const u8,
    w: *std.Io.Writer,
    options: Options,
    indent: usize,

    pub const Options = struct {
        // indentation
        indent_char: u8 = ' ',
        indent_width: u8 = 4,
        // newlines
        newline: []const u8 = "\n",
    };

    fn writeRoot(writer: *Writer) std.Io.Writer.Error!void {
        const nodes = writer.ast.nodes.items;
        const root = nodes[0];
        std.debug.assert(root.tag == .root);
        var i = root.data.list.start;
        const end = i + root.data.list.len;
        var first = true;
        while (i < end) : (i += 1) {
            const node = writer.ast.nodes.items[i];
            if (!isTopLevel(node.tag)) {
                continue;
            }
            if (!first) {
                // since we already wrote a statement we should start new lines between
                // statements
                try writer.w.writeAll(writer.options.newline);
            }
            first = false;

            try writer.writeStatement(i);
        }
        try writer.w.writeAll(writer.options.newline);
    }

    fn writeStatement(writer: *Writer, index: Ast.Node.Index) std.Io.Writer.Error!void {
        try writer.writeIndent();
        try writer.writeInline(index);
    }

    fn writeInline(writer: *Writer, index: Ast.Node.Index) std.Io.Writer.Error!void {
        const node = writer.ast.nodes.items[index];
        switch (node.tag) {
            .assign => {
                const lhs = node.data.binary_op.lhs;
                const rhs = node.data.binary_op.rhs;
                // lhs
                try writer.writeInline(lhs);
                // " = "
                try writer.w.writeAll("=");
                // rhs
                try writer.writeInline(rhs);
            },
            .identifier, .number_literal => {
                try writer.writeNodeSpan(node);
            },
            .root => {
                var i = node.data.list.start;
                const end = i + node.data.list.len;
                var first = true;
                while (i < end) : (i += 1) {
                    const child = writer.ast.nodes.items[i];
                    if (!isTopLevel(child.tag)) continue;
                    if (!first) {
                        try writer.w.writeAll(writer.options.newline);
                        try writer.writeIndent();
                    }
                    first = false;
                    try writer.writeInline(i);
                }
            },
            .@"error" => {
                try writer.w.writeAll("<error>");
            },
        }
    }

    fn writeIndent(writer: *Writer) std.Io.Writer.Error!void {
        if (writer.indent == 0) return;
        const count: usize = writer.indent * writer.options.indent_width;
        if (count == 0) return;
        try writer.w.splatByteAll(writer.options.indent_char, count);
    }

    fn writeNodeSpan(writer: *Writer, node: Ast.Node) std.Io.Writer.Error!void {
        // render the byte span covered by this node from the original source so we get the
        // original lexeme
        //
        // we treat the node as a contiguous range from the first token's start
        // to the last token's end
        const start_tok = node.token_start;
        const end_tok = start_tok + node.token_len - 1;
        const start_off = getTokenStart(writer.ast, start_tok);
        const end_off = getTokenEnd(writer.ast, end_tok);
        if (start_off <= end_off and end_off <= writer.source.len) {
            try writer.w.writeAll(writer.source[start_off..end_off]);
        }
    }

    fn isTopLevel(tag: Ast.Node.Tag) bool {
        return switch (tag) {
            .assign => true,
            .identifier, .number_literal => false,
            .root => false,
            .@"error" => false,
        };
    }
};

fn getTokenStart(ast: *const Ast, index: u32) usize {
    return ast.tokens.items(.span)[index].start;
}

fn getTokenEnd(ast: *const Ast, index: u32) usize {
    const span = ast.tokens.items(.span)[index];
    return span.start + span.len;
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
        @"error",
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

fn allocSafeInitRoot(allocator: std.mem.Allocator, source: []const u8) !void {
    var ast: Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);
}

test "allocation failure" {
    const allocator = std.testing.allocator;
    const source =
        "x=1; // ok\n" ++
        "y=&; // invalid rhs\n" ++
        "z=2";

    try std.testing.checkAllAllocationFailures(
        allocator,
        allocSafeInitRoot,
        .{source},
    );
}

const tokenizer = @import("tokenizer.zig");
const std = @import("std");
const Parse = @import("Parse.zig");
const Diagnostic = @import("Diagnostic.zig");
const builtin = @import("builtin");
const Ast = @This();
