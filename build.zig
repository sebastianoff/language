pub fn build(b: *std.Build) void {
    // Steps.
    const fmt = b.step("fmt", "Format source files");
    const fmt_check = b.step("check-fmt", "Check source formatting");
    const unit_test = b.step("test", "Run unit tests");
    const run_step = b.step("run", "Run the application");
    // Options.
    const options = b.addOptions();
    const version = b.option(
        []const u8,
        "version",
        "CLI version. Defaults to `version` from build.zig.zon",
    ) orelse zon.version;
    _ = std.SemanticVersion.parse(version) catch
        std.debug.panic("expected a valid semantic version for 'version' option", .{});
    options.addOption([]const u8, "version", version);
    // Standard options.
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseFast,
    });
    // libn.
    const lib_module = b.addModule("libn", .{
        .root_source_file = b.path("src/root.zig"),
        .optimize = optimize,
        .target = target,
        .imports = &.{
            .{ .name = "build_options", .module = options.createModule() },
        },
    });
    const lib = b.addLibrary(.{
        .name = "n",
        .root_module = lib_module,
    });
    b.installArtifact(lib);
    // Command line interface.
    const cli_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .optimize = optimize,
        .target = target,
        .imports = &.{
            .{ .name = "libn", .module = lib_module },
        },
    });
    const cli_executable = b.addExecutable(.{
        .name = "n",
        .root_module = cli_module,
    });
    b.installArtifact(cli_executable);
    // `zig build run`
    const run_cmd = b.addRunArtifact(cli_executable);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    run_step.dependOn(&run_cmd.step);
    b.default_step = run_step;
    // `zig build fmt`.
    const paths: []const []const u8 = &.{ "src", "build.zig" };
    fmt.dependOn(&b.addFmt(
        .{ .paths = paths },
    ).step);
    // `zig build fmt-check`
    fmt_check.dependOn(&b.addFmt(
        .{ .paths = paths, .check = true },
    ).step);
    // `zig build test`
    const test_lib = b.addTest(.{
        .root_module = lib_module,
    });
    unit_test.dependOn(&b.addRunArtifact(test_lib).step);
}

const std = @import("std");
const zon = @import("build.zig.zon");
