pub const version_string = build_options.version;
pub const version = std.SemanticVersion.parse(version_string) catch unreachable;
pub const tokenizer = @import("tokenizer.zig");

const std = @import("std");
const build_options = @import("build_options");
