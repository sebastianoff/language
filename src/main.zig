pub fn main() !void {
    std.debug.print("{[version]s} (Zig {[zig_version]s}; {[mode]s})\n", .{
        .version = libn.version_string,
        .zig_version = builtin.zig_version_string,
        .mode = @tagName(builtin.mode),
    });
    const source = "hello_123 = 456; // an unknown token type";
    const allocator = std.heap.page_allocator;

    std.debug.print("============ source: =============\n{[source]s}\n", .{ .source = source });
    std.debug.print("============ tokenize: =============\n", .{});

    var tokens = libn.tokenizer.tokenize(allocator, source) catch |err| {
        std.debug.print("tokenization failed: {any}\n", .{err});
        return;
    };
    defer tokens.deinit(allocator);

    std.debug.print("tokens found: {d}\n", .{tokens.len});

    const tags = tokens.items(.tag);
    const spans = tokens.items(.span);

    for (0..tokens.len) |i| {
        const token_tag = tags[i];
        const token_span = spans[i];
        const token_slice = token_span.slice(source);

        std.debug.print("  - {[tag]s:12} `{[slice]s}`\n", .{
            .tag = @tagName(token_tag),
            .slice = token_slice,
        });
    }

    std.debug.print("============ ast: =============\n", .{});
    var ast: libn.Ast = try .initRoot(allocator, source);
    defer ast.deinit(allocator);
    std.debug.print("{[ast]s}", .{ .ast = try ast.renderToOwnedSlice(allocator, source, .{}) });
}

const libn = @import("libn");
const builtin = @import("builtin");
const std = @import("std");
