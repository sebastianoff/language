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
            else => {
                // expressions
                try writer.writeExpr(index, 0);
            },
        }
    }

    fn writeExpr(writer: *Writer, index: Ast.Node.Index, parent_prec: u8) std.Io.Writer.Error!void {
        const node = writer.ast.nodes.items[index];
        // leaves and error
        switch (node.tag) {
            .identifier, .number_literal => {
                try writer.writeNodeSpan(node);
                return;
            },
            .@"error" => {
                try writer.w.writeAll("<error>");
                return;
            },
            else => {},
        }
        // binary
        if (isBinaryTag(node.tag)) {
            const this_prec = binaryPrecedence(node.tag);
            const need_paren = this_prec < parent_prec;

            if (need_paren) try writer.w.writeAll("(");

            const lhs = node.data.binary_op.lhs;
            const rhs = node.data.binary_op.rhs;
            // lhs
            const lhs_tag = writer.ast.nodes.items[lhs].tag;
            if (isBinaryTag(lhs_tag) and binaryPrecedence(lhs_tag) < this_prec) {
                try writer.w.writeAll("(");
                try writer.writeExpr(lhs, 0);
                try writer.w.writeAll(")");
            } else {
                try writer.writeExpr(lhs, this_prec);
            }
            // lexeme
            try writer.w.writeAll(opMnemonic(node.tag));
            // rhs
            const rhs_tag = writer.ast.nodes.items[rhs].tag;
            if (isBinaryTag(rhs_tag) and binaryPrecedence(rhs_tag) < this_prec) {
                try writer.w.writeAll("(");
                try writer.writeExpr(rhs, 0);
                try writer.w.writeAll(")");
            } else {
                try writer.writeExpr(rhs, this_prec);
            }

            if (need_paren) try writer.w.writeAll(")");
            return;
        }
        try writer.writeNodeSpan(node);
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

    inline fn isTopLevel(tag: Ast.Node.Tag) bool {
        return switch (tag) {
            .assign => true,
            .identifier, .number_literal => false,
            .root => false,
            .@"error" => false,
            .add, .sub, .mul, .div, .mod, .less, .less_equal, .greater, .greater_equal, .equal_equal, .not_equal, .@"and", .@"or", .pipe, .pipe_greater, .less_pipe, .range => false,
        };
    }
    inline fn isBinaryTag(tag: Ast.Node.Tag) bool {
        return switch (tag) {
            .assign => true,
            .add, .sub, .mul, .div, .mod => true,
            .less, .less_equal, .greater, .greater_equal => true,
            .equal_equal, .not_equal => true,
            .@"and", .@"or" => true,
            .pipe, .pipe_greater, .less_pipe => true,
            .range => true,
            else => false,
        };
    }

    inline fn opMnemonic(tag: Ast.Node.Tag) []const u8 {
        return switch (tag) {
            .assign => "=",
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",

            .less => "<",
            .less_equal => "<=",
            .greater => ">",
            .greater_equal => ">=",

            .equal_equal => "==",
            .not_equal => "!=",

            .@"and" => "&&",
            .@"or" => "||",

            .pipe => "|",
            .pipe_greater => "|>",
            .less_pipe => "<|",

            .range => "..",

            else => "?",
        };
    }
};

pub fn tokenSlice(ast: *const Ast, index: u32, source: []const u8) []const u8 {
    return ast.tokens.items(.span)[index].slice(source);
}

pub inline fn binaryPrecedence(tag: Ast.Node.Tag) u8 {
    return switch (tag) {
        .mul, .div, .mod => 50,
        .add, .sub => 40,
        .less, .less_equal, .greater, .greater_equal => 30,
        .equal_equal, .not_equal => 25,
        .range => 22,
        .pipe_greater, .less_pipe => 21,
        .pipe => 20,
        .@"and" => 15,
        .@"or" => 14,
        .assign => 10,
        else => 0,
    };
}

pub inline fn binaryTag(tag: tokenizer.Token.Tag) ?Ast.Node.Tag {
    return switch (tag) {
        .plus => .add,
        .minus => .sub,
        .star => .mul,
        .slash => .div,
        .percent => .mod,

        .less => .less,
        .less_equal => .less_equal,
        .greater => .greater,
        .greater_equal => .greater_equal,

        .equal_equal => .equal_equal,
        .not_equal => .not_equal,

        .amp_amp => .@"and",
        .pipe_pipe => .@"or",
        .pipe => .pipe,
        .pipe_greater => .pipe_greater,
        .less_pipe => .less_pipe,

        .dot_dot => .range,
        else => null,
    };
}

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
        // binary operators
        add,
        sub,
        mul,
        div,
        mod,
        less,
        less_equal,
        greater,
        greater_equal,
        equal_equal,
        not_equal,
        @"and",
        @"or",
        pipe,
        pipe_greater,
        less_pipe,
        range,
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
