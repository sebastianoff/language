pub fn main() !void {
    std.debug.print("{[version]s} (Zig {[zig_version]s}; {[mode]s})\n", .{
        .version = libn.version_string,
        .zig_version = builtin.zig_version_string,
        .mode = @tagName(builtin.mode),
    });
    const source = "hello_123 = 456_000;";
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

    var mod: libn.Ir.Module = try .init(allocator, &ast, source);
    defer mod.deinit(allocator);

    std.debug.print("\n============ IR: =============\n", .{});
    const stderr = std.debug.lockStderrWriter(&.{});
    defer std.debug.unlockStderrWriter();
    try mod.disassemble(stderr);

    var prog: libn.Ir.Program = try .init(allocator, &mod);
    defer prog.deinit(allocator);

    std.debug.print("\n============ bytecode: =============\n", .{});
    try prog.disassemble(stderr);

    var state: libn.State = try .init(allocator, &prog);
    defer state.deinit(allocator);

    std.debug.print("\n============ eval: globals =============\n", .{});
    libn.dumpAll(state.globals);

    const ops = prog.code.items(.op);
    const dsts = prog.code.items(.dst);
    var last_res: ?u32 = null;
    var i: usize = ops.len;
    while (i > 0) : (i -= 1) {
        if (ops[i - 1] != .store) {
            last_res = dsts[i - 1];
            break;
        }
    }
    if (last_res) |r| {
        std.debug.print("\n============ eval: last register =============\n", .{});
        libn.dumpValue("last", state.regs[r]);
    }
}

const libn = @import("libn");
const builtin = @import("builtin");
const std = @import("std");
