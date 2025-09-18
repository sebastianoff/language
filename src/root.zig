pub const version_string = build_options.version;
pub const version = std.SemanticVersion.parse(version_string) catch unreachable;

const std = @import("std");
const build_options = @import("build_options");
