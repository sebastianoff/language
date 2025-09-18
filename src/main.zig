pub fn main() void {
    std.debug.print("{[version]s} (Zig {[zig_version]s}; {[mode]s})\n", .{
        .version = libn.version_string,
        .zig_version = builtin.zig_version_string,
        .mode = @tagName(builtin.mode),
    });
}

const libn = @import("libn");
const builtin = @import("builtin");
const std = @import("std");
