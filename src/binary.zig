pub const native_endian = builtin.cpu.arch.endian();

pub const magic = "nb1";
pub const version: u16 = 1;

pub const Header = extern struct {
    magic: [3]u8 = magic.*,
    version: u16 = version,
    /// 0 - little, 1 - big
    endian: u8 = if (native_endian == .little) 0 else 1,
    reserved: u8 = 0,
    // counts
    constants_len: u32 = 0,
    code_len: u32 = 0,
    // vm
    vars: u32 = 0,
    regs: u32 = 0,
    names_count: u32 = 0,
    strings_len: u32 = 0,
};

pub const Options = struct {
    /// format-wide endianess
    endian: std.builtin.Endian = native_endian,
};

/// compute the exact file size that `writeProgram`
pub fn sizeOfProgram(p: *const Ir.Program) usize {
    const hdr = @sizeOf(Header);

    const constants_bytes = @as(usize, p.constants.items.len) * @sizeOf(i64);
    const ops_bytes = @as(usize, p.code.len) * @sizeOf(u8);
    const u32_vec_bytes = @as(usize, p.code.len) * @sizeOf(u32);

    var total: usize = 0;
    total += hdr;
    total = std.mem.alignForward(usize, total, 8); // constants alignment
    total += constants_bytes;

    total += ops_bytes; // u8 needs no alignment
    total = std.mem.alignForward(usize, total, 4); // u32 vectors alignment
    total += u32_vec_bytes; // dst
    total += u32_vec_bytes; // a
    total += u32_vec_bytes; // b
    total += u32_vec_bytes; // aux
    return total;
}

/// ```
///   Header
///   pad to 8
///   constants: i64[constants_len]
///   ops: u8[code_len]
///   pad to 4
///   dst: u32[code_len]
///   a:   u32[code_len]
///   b:   u32[code_len]
///   aux: u32[code_len]
/// ```
pub fn writeProgram(w: *std.Io.Writer, p: *const Ir.Program, opts: Options) !void {
    var cursor: usize = 0;
    const hdr: Header = .{
        .constants_len = @intCast(p.constants.items.len),
        .code_len = @intCast(p.code.len),
        .vars = p.vars,
        .regs = p.regs,
        .endian = if (opts.endian == .little) 0 else 1,
    };
    try w.writeStruct(hdr, opts.endian);
    cursor += @sizeOf(Header);
    // align to 8, then constants (i64[])
    try padToAlign(w, &cursor, 8);
    if (p.constants.items.len != 0) {
        try w.writeSliceEndian(i64, p.constants.items, opts.endian);
        cursor += @as(usize, p.constants.items.len) * @sizeOf(i64);
    }
    // ops (u8[])
    if (p.code.len != 0) {
        const ops = p.code.items(.op);
        // op is enum(u8)
        for (ops) |op| {
            try w.writeByte(@intFromEnum(op));
        }
        cursor += ops.len * @sizeOf(u8);
    }
    // align to 4, then u32 vectors: dst, a, b, aux
    try padToAlign(w, &cursor, 4);
    if (p.code.len != 0) {
        try w.writeSliceEndian(u32, p.code.items(.dst), opts.endian);
        cursor += p.code.len * @sizeOf(u32);

        try w.writeSliceEndian(u32, p.code.items(.a), opts.endian);
        cursor += p.code.len * @sizeOf(u32);

        try w.writeSliceEndian(u32, p.code.items(.b), opts.endian);
        cursor += p.code.len * @sizeOf(u32);

        try w.writeSliceEndian(u32, p.code.items(.aux), opts.endian);
        cursor += p.code.len * @sizeOf(u32);
    }
}

pub fn writeProgramToFile(file: std.fs.File, p: *const Ir.Program, opts: Options) !void {
    var buf: [16 * 1024]u8 = undefined;
    var fw: std.fs.File.Writer = .init(file, &buf);
    try writeProgram(&fw.interface, p, opts);
    try fw.interface.flush();
}

fn padToAlign(w: *std.Io.Writer, cursor: *usize, alignment: usize) std.Io.Writer.Error!void {
    const padded = std.mem.alignForward(usize, cursor.*, alignment);
    const pad_n = padded - cursor.*;
    if (pad_n != 0) {
        try w.splatByteAll(0, pad_n);
        cursor.* = padded;
    }
}

fn readIntAt(comptime T: type, buf: []const u8, off: usize, endian: std.builtin.Endian) T {
    const n = @divExact(@typeInfo(T).int.bits, 8);
    const p: *const [n]u8 = @ptrCast(buf.ptr + off);
    return std.mem.readInt(T, p, endian);
}

fn readU16(buf: []const u8, endian: std.builtin.Endian) u16 {
    return readIntAt(u16, buf, 0, endian);
}

fn readU32(buf: []const u8, endian: std.builtin.Endian) u32 {
    return readIntAt(u32, buf, 0, endian);
}

fn readI64(buf: []const u8, endian: std.builtin.Endian) i64 {
    return readIntAt(i64, buf, 0, endian);
}

fn isZeroed(buf: []const u8) ReadError!void {
    for (buf) |b| if (b != 0) return error.BadPadding;
}

fn initEmpty(allocator: std.mem.Allocator) !Ir.Program {
    var m: Ir.Module = .empty;
    defer m.deinit(allocator);
    return try .init(allocator, &m);
}

fn buildEmpty(allocator: std.mem.Allocator) !Ir.Program {
    //   r0 <- iconst 5
    //   r1 <- iconst 3
    //   r2 <- r0 + r1
    //   store slot[0] <- r2
    //   r4 <- load slot[0]
    var m: Ir.Module = .empty;
    errdefer m.deinit(allocator);

    try m.constants.append(allocator, 5);
    try m.constants.append(allocator, 3);

    try m.instructions.append(allocator, .{ .op = .iconst, .a = 0, .b = 0, .aux = 0 }); // r0
    try m.instructions.append(allocator, .{ .op = .iconst, .a = 0, .b = 0, .aux = 1 }); // r1
    try m.instructions.append(allocator, .{ .op = .add, .a = 0, .b = 1, .aux = 0 }); // r2
    try m.instructions.append(allocator, .{ .op = .store, .a = 2, .b = 0, .aux = 0 }); // store r2 -> slot0
    try m.instructions.append(allocator, .{ .op = .load, .a = 0, .b = 0, .aux = 0 }); // r4

    m.slots = 1;

    const p: Ir.Program = try .init(allocator, &m);
    m.deinit(allocator);
    return p;
}

fn sectionOffsets(hdr_size: usize, constants_len: usize, code_len: usize) struct {
    constants_off: usize,
    ops_off: usize,
    dst_off: usize,
    a_off: usize,
    b_off: usize,
    aux_off: usize,
    file_size: usize,
} {
    var off: usize = hdr_size;
    // constants aligned to 8
    off = std.mem.alignForward(usize, off, 8);
    const constants_off = off;
    off += constants_len * @sizeOf(i64);
    // ops (u8)
    const ops_off = off;
    off += code_len * @sizeOf(u8);
    // u32 vectors aligned to 4
    off = std.mem.alignForward(usize, off, 4);
    const dst_off = off;
    off += code_len * @sizeOf(u32);
    const a_off = off;
    off += code_len * @sizeOf(u32);
    const b_off = off;
    off += code_len * @sizeOf(u32);
    const aux_off = off;
    off += code_len * @sizeOf(u32);

    return .{
        .constants_off = constants_off,
        .ops_off = ops_off,
        .dst_off = dst_off,
        .a_off = a_off,
        .b_off = b_off,
        .aux_off = aux_off,
        .file_size = off,
    };
}

pub const ReadError = error{
    InvalidMagic,
    BadEndian,
    UnsupportedVersion,
    Truncated,
    OutOfBounds,
    BadPadding,
} || mem.MapError || std.fs.File.OpenError || std.fs.File.StatError;

pub const ReaderOpenFlags = struct {
    strict_size: bool = false,
    verify_padding: bool = false,
    advise: ?mem.Advice = null,
    lock: bool = false,
};

pub const View = struct {
    map: mem.Mapping,
    bytes: []const u8,

    hdr_version: u16,
    file_endian: std.builtin.Endian,
    constants_len: usize,
    code_len: usize,
    vars: u32,
    regs: u32,
    names_count: u32,
    strings_len: u32,

    constants_off: usize,
    ops_off: usize,
    dst_off: usize,
    a_off: usize,
    b_off: usize,
    aux_off: usize,
    computed_file_size: usize,

    constants_bytes: []const u8,
    ops_bytes: []const u8,
    dst_bytes: []const u8,
    a_bytes: []const u8,
    b_bytes: []const u8,
    aux_bytes: []const u8,

    typed_constants: ?[]align(8) const i64 = null,
    typed_dst: ?[]align(4) const u32 = null,
    typed_a: ?[]align(4) const u32 = null,
    typed_b: ?[]align(4) const u32 = null,
    typed_aux: ?[]align(4) const u32 = null,

    pub fn init(file: std.fs.File, flags: ReaderOpenFlags) ReadError!View {
        const st = try file.stat();
        if (st.size < @sizeOf(Header)) return error.Truncated;

        var m = try mem.map(file, .{
            .length = @intCast(st.size),
            .offset = 0,
            .protect = .{ .read = true, .write = false, .exec = false },
            .shared = true,
            .populate = flags.advise != null,
            .lock = flags.lock,
        });
        errdefer m.unmap();

        var view: View = .{
            .map = m,
            .bytes = m.slice(),

            .hdr_version = 0,
            .file_endian = .little,
            .constants_len = 0,
            .code_len = 0,
            .vars = 0,
            .regs = 0,
            .names_count = 0,
            .strings_len = 0,
            .constants_off = 0,
            .ops_off = 0,
            .dst_off = 0,
            .a_off = 0,
            .b_off = 0,
            .aux_off = 0,
            .computed_file_size = 0,

            .constants_bytes = &.{},
            .ops_bytes = &.{},
            .dst_bytes = &.{},
            .a_bytes = &.{},
            .b_bytes = &.{},
            .aux_bytes = &.{},
            .typed_constants = null,
            .typed_dst = null,
            .typed_a = null,
            .typed_b = null,
            .typed_aux = null,
        };

        const parsed = try parseHeaderHost(view.bytes);

        view.hdr_version = parsed.hdr_version;
        view.file_endian = parsed.endian;
        view.constants_len = @intCast(parsed.constants_len);
        view.code_len = @intCast(parsed.code_len);
        view.vars = parsed.vars;
        view.regs = parsed.regs;
        view.names_count = parsed.names_count;
        view.strings_len = parsed.strings_len;

        const views = try computeViews(view.bytes, view.file_endian, .{
            .constants_len = view.constants_len,
            .code_len = view.code_len,
        });

        view.constants_off = views.constants_off;
        view.ops_off = views.ops_off;
        view.dst_off = views.dst_off;
        view.a_off = views.a_off;
        view.b_off = views.b_off;
        view.aux_off = views.aux_off;
        view.computed_file_size = views.file_size;

        view.constants_bytes = views.constants_bytes;
        view.ops_bytes = views.ops_bytes;
        view.dst_bytes = views.dst_bytes;
        view.a_bytes = views.a_bytes;
        view.b_bytes = views.b_bytes;
        view.aux_bytes = views.aux_bytes;

        view.typed_constants = views.typed_constants;
        view.typed_dst = views.typed_dst;
        view.typed_a = views.typed_a;
        view.typed_b = views.typed_b;
        view.typed_aux = views.typed_aux;

        if (flags.strict_size and view.bytes.len != view.computed_file_size) {
            return error.Truncated;
        }
        if (flags.verify_padding) {
            // between header and constants (if any)
            if (view.constants_off > @sizeOf(Header)) {
                try isZeroed(view.bytes[@sizeOf(Header)..view.constants_off]);
            }
            // between ops and dst (if any)
            const ops_end = view.ops_off + view.code_len;
            if (view.dst_off > ops_end) {
                try isZeroed(view.bytes[ops_end..view.dst_off]);
            }
        }

        if (flags.advise) |a| {
            view.map.advise(0, view.map.len, a);
        }

        return view;
    }

    pub fn deinit(view: *View) void {
        view.map.unmap();
        view.* = undefined;
    }

    // lengths
    pub fn constantsLen(view: *const View) usize {
        return view.constants_len;
    }

    pub fn opsLen(view: *const View) usize {
        return view.code_len;
    }

    // constants
    pub fn constantsSliceHost(view: *const View) ?[]const i64 {
        if (view.typed_constants) |s| return s else return null;
    }

    pub fn constantAt(view: *const View, idx: usize) i64 {
        if (idx >= view.constants_len) unreachable;
        if (view.typed_constants) |s| return s[idx];
        const off = idx * 8;
        return readIntAt(i64, view.constants_bytes, off, view.file_endian);
    }

    pub fn tryConstantAt(view: *const View, idx: usize) ReadError!i64 {
        if (idx >= view.constants_len) return error.OutOfBounds;
        return view.constantAt(idx);
    }

    // ops
    pub fn opsBytes(view: *const View) []const u8 {
        return view.ops_bytes;
    }
    pub fn opByteAt(view: *const View, idx: usize) u8 {
        if (idx >= view.code_len) unreachable;
        return view.ops_bytes[idx];
    }
    pub fn opAt(view: *const View, idx: usize) operations.Op {
        return @enumFromInt(view.opByteAt(idx));
    }
    pub fn tryOpAt(view: *const View, idx: usize) ReadError!operations.Op {
        if (idx >= view.code_len) return error.OutOfBounds;
        return view.opAt(idx);
    }

    // u32 vectors (dst/a/b/aux)
    pub fn dstSliceHost(view: *const View) ?[]const u32 {
        return view.typed_dst;
    }

    pub fn aSliceHost(view: *const View) ?[]const u32 {
        return view.typed_a;
    }

    pub fn bSliceHost(view: *const View) ?[]const u32 {
        return view.typed_b;
    }

    pub fn auxSliceHost(view: *const View) ?[]const u32 {
        return view.typed_aux;
    }

    pub fn dstAt(view: *const View, idx: usize) u32 {
        if (idx >= view.code_len) unreachable;
        if (view.typed_dst) |s| return s[idx];
        const off = idx * 4;
        return readIntAt(u32, view.dst_bytes, off, view.file_endian);
    }

    pub fn aAt(view: *const View, idx: usize) u32 {
        if (idx >= view.code_len) unreachable;
        if (view.typed_a) |s| return s[idx];
        const off = idx * 4;
        return readIntAt(u32, view.a_bytes, off, view.file_endian);
    }

    pub fn bAt(view: *const View, idx: usize) u32 {
        if (idx >= view.code_len) unreachable;
        if (view.typed_b) |s| return s[idx];
        const off = idx * 4;
        return readIntAt(u32, view.b_bytes, off, view.file_endian);
    }

    pub fn auxAt(view: *const View, idx: usize) u32 {
        if (idx >= view.code_len) unreachable;
        if (view.typed_aux) |s| return s[idx];
        const off = idx * 4;
        return readIntAt(u32, view.aux_bytes, off, view.file_endian);
    }

    pub fn tryDstAt(view: *const View, idx: usize) ReadError!u32 {
        if (idx >= view.code_len) return error.OutOfBounds;
        return view.dstAt(idx);
    }

    pub fn tryAAt(view: *const View, idx: usize) ReadError!u32 {
        if (idx >= view.code_len) return error.OutOfBounds;
        return view.aAt(idx);
    }

    pub fn tryBAt(view: *const View, idx: usize) ReadError!u32 {
        if (idx >= view.code_len) return error.OutOfBounds;
        return view.bAt(idx);
    }

    pub fn tryAuxAt(view: *const View, idx: usize) ReadError!u32 {
        if (idx >= view.code_len) return error.OutOfBounds;
        return view.auxAt(idx);
    }

    pub fn materializeConstants(view: *const View, allocator: std.mem.Allocator) ![]i64 {
        const n = view.constants_len;
        var out = try allocator.alloc(i64, n);
        errdefer allocator.free(out);
        if (view.typed_constants) |s| {
            @memcpy(out, s);
            return out;
        }
        var i: usize = 0;
        var base: usize = 0;
        while (i < n) : (i += 1) {
            out[i] = readIntAt(i64, view.constants_bytes, base, view.file_endian);
            base += 8;
        }
        return out;
    }

    pub fn materializeU32(view: *const View, allocator: std.mem.Allocator, which: enum { dst, a, b, aux }) ![]u32 {
        const n = view.code_len;
        var out = try allocator.alloc(u32, n);
        errdefer allocator.free(out);
        const fe = view.file_endian;
        switch (which) {
            .dst => if (view.typed_dst) |s| {
                @memcpy(out, s);
                return out;
            } else {
                var i: usize = 0;
                var base: usize = 0;
                while (i < n) : (i += 1) {
                    out[i] = readIntAt(u32, view.dst_bytes, base, fe);
                    base += 4;
                }
                return out;
            },
            .a => if (view.typed_a) |s| {
                @memcpy(out, s);
                return out;
            } else {
                var i: usize = 0;
                var base: usize = 0;
                while (i < n) : (i += 1) {
                    out[i] = readIntAt(u32, view.a_bytes, base, fe);
                    base += 4;
                }
                return out;
            },
            .b => if (view.typed_b) |s| {
                @memcpy(out, s);
                return out;
            } else {
                var i: usize = 0;
                var base: usize = 0;
                while (i < n) : (i += 1) {
                    out[i] = readIntAt(u32, view.b_bytes, base, fe);
                    base += 4;
                }
                return out;
            },
            .aux => if (view.typed_aux) |s| {
                @memcpy(out, s);
                return out;
            } else {
                var i: usize = 0;
                var base: usize = 0;
                while (i < n) : (i += 1) {
                    out[i] = readIntAt(u32, view.aux_bytes, base, fe);
                    base += 4;
                }
                return out;
            },
        }
    }
};

fn parseHeaderHost(buf: []const u8) ReadError!struct {
    endian: std.builtin.Endian,
    hdr_version: u16,
    constants_len: u32,
    code_len: u32,
    vars: u32,
    regs: u32,
    names_count: u32,
    strings_len: u32,
} {
    if (buf.len < @sizeOf(Header)) return error.Truncated;

    if (!std.mem.eql(u8, buf[0..magic.len], magic)) return error.InvalidMagic;

    const endian_byte = buf[@offsetOf(Header, "endian")];
    const file_endian: std.builtin.Endian = switch (endian_byte) {
        0 => .little,
        1 => .big,
        else => return error.BadEndian,
    };

    const off_version = @offsetOf(Header, "version");
    const off_constants_len = @offsetOf(Header, "constants_len");
    const off_code_len = @offsetOf(Header, "code_len");
    const off_vars = @offsetOf(Header, "vars");
    const off_regs = @offsetOf(Header, "regs");
    const off_names_count = @offsetOf(Header, "names_count");
    const off_strings_len = @offsetOf(Header, "strings_len");

    const hdr_version = std.mem.readInt(u16, buf[off_version .. off_version + 2], file_endian);
    if (hdr_version != version) return error.UnsupportedVersion;

    return .{
        .endian = file_endian,
        .hdr_version = hdr_version,
        .constants_len = std.mem.readInt(u32, buf[off_constants_len .. off_constants_len + 4], file_endian),
        .code_len = std.mem.readInt(u32, buf[off_code_len .. off_code_len + 4], file_endian),
        .vars = std.mem.readInt(u32, buf[off_vars .. off_vars + 4], file_endian),
        .regs = std.mem.readInt(u32, buf[off_regs .. off_regs + 4], file_endian),
        .names_count = std.mem.readInt(u32, buf[off_names_count .. off_names_count + 4], file_endian),
        .strings_len = std.mem.readInt(u32, buf[off_strings_len .. off_strings_len + 4], file_endian),
    };
}

fn computeViews(bytes: []const u8, file_endian: std.builtin.Endian, counts: struct {
    constants_len: usize,
    code_len: usize,
}) ReadError!struct {
    constants_off: usize,
    ops_off: usize,
    dst_off: usize,
    a_off: usize,
    b_off: usize,
    aux_off: usize,
    file_size: usize,

    constants_bytes: []const u8,
    ops_bytes: []const u8,
    dst_bytes: []const u8,
    a_bytes: []const u8,
    b_bytes: []const u8,
    aux_bytes: []const u8,
    // typed views when same endian
    typed_constants: ?[]align(8) const i64,
    typed_dst: ?[]align(4) const u32,
    typed_a: ?[]align(4) const u32,
    typed_b: ?[]align(4) const u32,
    typed_aux: ?[]align(4) const u32,
} {
    const offs = sectionOffsets(@sizeOf(Header), counts.constants_len, counts.code_len);
    if (bytes.len < offs.file_size) return error.Truncated;
    // slice out sections
    const constants_bytes = bytes[offs.constants_off .. offs.constants_off + counts.constants_len * 8];
    const ops_bytes = bytes[offs.ops_off .. offs.ops_off + counts.code_len];
    const dst_bytes = bytes[offs.dst_off .. offs.dst_off + counts.code_len * 4];
    const a_bytes = bytes[offs.a_off .. offs.a_off + counts.code_len * 4];
    const b_bytes = bytes[offs.b_off .. offs.b_off + counts.code_len * 4];
    const aux_bytes = bytes[offs.aux_off .. offs.aux_off + counts.code_len * 4];

    var typed_constants: ?[]align(8) const i64 = null;
    var typed_dst: ?[]align(4) const u32 = null;
    var typed_a: ?[]align(4) const u32 = null;
    var typed_b: ?[]align(4) const u32 = null;
    var typed_aux: ?[]align(4) const u32 = null;

    if (counts.constants_len > 0) {
        // constants aligned to 8 by writer
        if (file_endian == native_endian) {
            const p: [*]align(8) const i64 = @ptrCast(@alignCast(constants_bytes.ptr));
            typed_constants = p[0..counts.constants_len];
        }
    }
    if (counts.code_len > 0) {
        if (file_endian == native_endian) {
            const p_dst: [*]align(4) const u32 = @ptrCast(@alignCast(dst_bytes.ptr));
            const p_a: [*]align(4) const u32 = @ptrCast(@alignCast(a_bytes.ptr));
            const p_b: [*]align(4) const u32 = @ptrCast(@alignCast(b_bytes.ptr));
            const p_aux: [*]align(4) const u32 = @ptrCast(@alignCast(aux_bytes.ptr));
            typed_dst = p_dst[0..counts.code_len];
            typed_a = p_a[0..counts.code_len];
            typed_b = p_b[0..counts.code_len];
            typed_aux = p_aux[0..counts.code_len];
        }
    }

    return .{
        .constants_off = offs.constants_off,
        .ops_off = offs.ops_off,
        .dst_off = offs.dst_off,
        .a_off = offs.a_off,
        .b_off = offs.b_off,
        .aux_off = offs.aux_off,
        .file_size = offs.file_size,

        .constants_bytes = constants_bytes,
        .ops_bytes = ops_bytes,
        .dst_bytes = dst_bytes,
        .a_bytes = a_bytes,
        .b_bytes = b_bytes,
        .aux_bytes = aux_bytes,

        .typed_constants = typed_constants,
        .typed_dst = typed_dst,
        .typed_a = typed_a,
        .typed_b = typed_b,
        .typed_aux = typed_aux,
    };
}

fn writeHeaderOnlyFile(td: *std.testing.TmpDir, name: []const u8, endian: std.builtin.Endian) !std.fs.File {
    const allocator = std.testing.allocator;
    var p = try initEmpty(allocator);
    errdefer p.deinit(allocator);

    var f = try td.dir.createFile(name, .{ .read = true, .truncate = true, .mode = 0o644 });
    errdefer f.close();

    try writeProgramToFile(f, &p, .{ .endian = endian });
    return f;
}

fn writeProgramFile(td: *std.testing.TmpDir, name: []const u8, endian: std.builtin.Endian) !struct {
    file: std.fs.File,
    program: Ir.Program,
} {
    const allocator = std.testing.allocator;
    var p = try buildEmpty(allocator);
    errdefer p.deinit(allocator);

    var f = try td.dir.createFile(name, .{ .read = true, .truncate = true, .mode = 0o644 });
    errdefer f.close();

    try writeProgramToFile(f, &p, .{ .endian = endian });

    return .{ .file = f, .program = p };
}

test "header-only" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const endian = builtin.cpu.arch.endian();
    var f = try writeHeaderOnlyFile(&td, "header_only.nb", endian);
    defer f.close();

    var view: View = try .init(f, .{});
    defer view.deinit();

    try std.testing.expectEqual(@as(usize, 0), view.constantsLen());
    try std.testing.expectEqual(@as(usize, 0), view.opsLen());
    try std.testing.expectEqual(@as(u32, 0), view.vars);
    try std.testing.expectEqual(@as(u32, 0), view.regs);
    try std.testing.expectEqual(@as(u32, 0), view.names_count);
    try std.testing.expectEqual(@as(u32, 0), view.strings_len);

    const st = try f.stat();
    try std.testing.expectEqual(@as(usize, @intCast(st.size)), view.bytes.len);
    try std.testing.expectEqual(@sizeOf(Header), view.computed_file_size);
    try std.testing.expectEqual(@sizeOf(Header), view.bytes.len);

    // strict_size should also accept (sizes equal)
    var view_strict: View = try .init(f, .{ .strict_size = true });
    defer view_strict.deinit();
}

fn checkSampleViewCommon(view: *const View, endian: std.builtin.Endian) !void {
    // header fields
    try std.testing.expectEqual(version, view.hdr_version);
    try std.testing.expectEqual(endian, view.file_endian);
    try std.testing.expectEqual(@as(usize, 2), view.constants_len);
    try std.testing.expectEqual(@as(usize, 5), view.code_len);
    try std.testing.expectEqual(@as(u32, 1), view.vars);
    try std.testing.expectEqual(@as(u32, 5), view.regs);

    // offsets + file size
    const offs = sectionOffsets(@sizeOf(Header), 2, 5);
    try std.testing.expectEqual(offs.constants_off, view.constants_off);
    try std.testing.expectEqual(offs.ops_off, view.ops_off);
    try std.testing.expectEqual(offs.dst_off, view.dst_off);
    try std.testing.expectEqual(offs.a_off, view.a_off);
    try std.testing.expectEqual(offs.b_off, view.b_off);
    try std.testing.expectEqual(offs.aux_off, view.aux_off);
    try std.testing.expectEqual(offs.file_size, view.computed_file_size);

    // ops bytes and decoded ops
    const expect_ops = [_]u8{
        @intFromEnum(operations.Op.iconst),
        @intFromEnum(operations.Op.iconst),
        @intFromEnum(operations.Op.add),
        @intFromEnum(operations.Op.store),
        @intFromEnum(operations.Op.load),
    };
    try std.testing.expectEqual(@as(usize, 5), view.ops_bytes.len);
    try std.testing.expectEqualSlices(u8, &expect_ops, view.ops_bytes);
    for (0..5) |i| {
        try std.testing.expectEqual(@as(operations.Op, @enumFromInt(expect_ops[i])), view.opAt(i));
    }

    // constants
    try std.testing.expectEqual(@as(i64, 5), view.constantAt(0));
    try std.testing.expectEqual(@as(i64, 3), view.constantAt(1));

    // dst: [0,1,2,3,4]
    for (0..5) |i| {
        try std.testing.expectEqual(@as(u32, @intCast(i)), view.dstAt(i));
    }
    // a: [0,0,0,2,0]
    const expect_a = [_]u32{ 0, 0, 0, 2, 0 };
    for (expect_a, 0..) |v, i| try std.testing.expectEqual(v, view.aAt(i));
    // b: [0,0,1,0,0]
    const expect_b = [_]u32{ 0, 0, 1, 0, 0 };
    for (expect_b, 0..) |v, i| try std.testing.expectEqual(v, view.bAt(i));
    // aux: [0,1,0,0,0]
    const expect_aux = [_]u32{ 0, 1, 0, 0, 0 };
    for (expect_aux, 0..) |v, i| try std.testing.expectEqual(v, view.auxAt(i));
}

fn expectTypedSlices(view: *const View, expect: bool) !void {
    try std.testing.expectEqual(expect, view.constantsSliceHost() != null);
    try std.testing.expectEqual(expect, view.dstSliceHost() != null);
    try std.testing.expectEqual(expect, view.aSliceHost() != null);
    try std.testing.expectEqual(expect, view.bSliceHost() != null);
    try std.testing.expectEqual(expect, view.auxSliceHost() != null);
}

test "(little-endian) values and typed-slice fast path when native_endian" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const endian = std.builtin.Endian.little;
    var w = try writeProgramFile(&td, "prog_le.nb", endian);
    defer {
        w.program.deinit(std.testing.allocator);
        w.file.close();
    }

    var view: View = try .init(w.file, .{});
    defer view.deinit();
    try checkSampleViewCommon(&view, endian);

    const expect_typed = (native_endian == endian);
    try expectTypedSlices(&view, expect_typed);

    if (expect_typed) {
        if (view.constantsSliceHost()) |s| {
            try std.testing.expectEqual(@as(usize, 0), @intFromPtr(s.ptr) % 8);
        }
        if (view.dstSliceHost()) |s| try std.testing.expectEqual(@as(usize, 0), @intFromPtr(s.ptr) % 4);
        if (view.aSliceHost()) |s| try std.testing.expectEqual(@as(usize, 0), @intFromPtr(s.ptr) % 4);
        if (view.bSliceHost()) |s| try std.testing.expectEqual(@as(usize, 0), @intFromPtr(s.ptr) % 4);
        if (view.auxSliceHost()) |s| try std.testing.expectEqual(@as(usize, 0), @intFromPtr(s.ptr) % 4);
    }
}

test "(big-endian) cross-endian accessors work and typed-slice disabled when not native_endian" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const endian = std.builtin.Endian.big;
    var w = try writeProgramFile(&td, "prog_be.nb", endian);
    defer {
        w.program.deinit(std.testing.allocator);
        w.file.close();
    }

    var view: View = try .init(w.file, .{});
    defer view.deinit();
    try checkSampleViewCommon(&view, endian);

    const expect_typed = (native_endian == endian);
    try expectTypedSlices(&view, expect_typed);
}

test "verify_padding and strict_size flags" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var w = try writeProgramFile(&td, "prog_pad.nb", native_endian);
    defer {
        w.program.deinit(std.testing.allocator);
        w.file.close();
    }
    // verify_padding should pass
    {
        var view: View = try .init(w.file, .{ .verify_padding = true });
        defer view.deinit();
    }

    // append trailing zeros, then mapping should:
    // - succeed when strict_size = false,
    // - fail when strict_size = true.
    {
        try w.file.seekFromEnd(0);
        try w.file.writeAll("\x00\x00\x00\x00\x00");
        // non-strict
        var view: View = try .init(w.file, .{ .verify_padding = true });
        defer view.deinit();
        // strict
        try std.testing.expectError(error.Truncated, View.init(w.file, .{ .strict_size = true }));
    }
}

test "invalid magic" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var f = try writeHeaderOnlyFile(&td, "bad_magic.nb", native_endian);
    defer f.close();

    try f.pwriteAll("ZZZ", 0);

    try std.testing.expectError(error.InvalidMagic, View.init(f, .{}));
}

test "bad endian flag" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var f = try writeHeaderOnlyFile(&td, "bad_endian.nb", native_endian);
    defer f.close();

    const off_endian = @offsetOf(Header, "endian");
    try f.pwriteAll(&[_]u8{2}, off_endian); // invalid value

    try std.testing.expectError(error.BadEndian, View.init(f, .{}));
}

test "unsupported version" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var f = try writeHeaderOnlyFile(&td, "bad_version.nb", native_endian);
    defer f.close();

    const off_version = @offsetOf(Header, "version");
    var buf: [2]u8 = undefined;
    std.mem.writeInt(u16, buf[0..2], version + 1, native_endian);
    try f.pwriteAll(&buf, off_version);

    try std.testing.expectError(error.UnsupportedVersion, View.init(f, .{}));
}

test "truncated file, too small for header and mid-file cutoff" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    // too small for header
    {
        var f = try td.dir.createFile("trunc_small.nb", .{ .read = true, .truncate = true, .mode = 0o644 });
        defer f.close();
        try f.writeAll("hi"); // 2 bytes
        try std.testing.expectError(error.Truncated, View.init(f, .{}));
    }

    // valid header/program but file shorter than computed_file_size
    {
        const endian = builtin.cpu.arch.endian();
        var w = try writeProgramFile(&td, "trunc_mid.nb", endian);
        defer {
            w.program.deinit(std.testing.allocator);
            w.file.close();
        }

        const offs = sectionOffsets(@sizeOf(Header), 2, 5);
        const cut = offs.file_size - 3;
        try w.file.setEndPos(cut);

        try std.testing.expectError(error.Truncated, View.init(w.file, .{}));
    }
}

test "out-of-bounds accessors return error.OutOfBounds" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var w = try writeProgramFile(&td, "oob.nb", native_endian);
    defer {
        w.program.deinit(std.testing.allocator);
        w.file.close();
    }

    var view: View = try .init(w.file, .{});
    defer view.deinit();

    try std.testing.expectError(error.OutOfBounds, view.tryConstantAt(view.constantsLen()));
    try std.testing.expectError(error.OutOfBounds, view.tryOpAt(view.opsLen()));
    try std.testing.expectError(error.OutOfBounds, view.tryDstAt(view.opsLen()));
    try std.testing.expectError(error.OutOfBounds, view.tryAAt(view.opsLen()));
    try std.testing.expectError(error.OutOfBounds, view.tryBAt(view.opsLen()));
    try std.testing.expectError(error.OutOfBounds, view.tryAuxAt(view.opsLen()));
}

fn testProgramEndianness(allocator: std.mem.Allocator, endian: std.builtin.Endian) !void {
    var p = try buildEmpty(allocator);
    defer p.deinit(allocator);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    const opts: Options = .{ .endian = endian };
    try writeProgram(&aw.writer, &p, opts);
    const blob = aw.written();

    const off_version = @offsetOf(Header, "version");
    const off_endian = @offsetOf(Header, "endian");
    const off_constants_len = @offsetOf(Header, "constants_len");
    const off_code_len = @offsetOf(Header, "code_len");
    const off_vars = @offsetOf(Header, "vars");
    const off_regs = @offsetOf(Header, "regs");

    try std.testing.expectEqualSlices(u8, magic, blob[0..magic.len]);
    try std.testing.expectEqual(@as(u16, version), readU16(blob[off_version .. off_version + 2], endian));
    try std.testing.expectEqual(@as(u8, if (endian == .little) 0 else 1), blob[off_endian]);
    try std.testing.expectEqual(@as(u32, 2), readU32(blob[off_constants_len..][0..4], endian));
    try std.testing.expectEqual(@as(u32, 5), readU32(blob[off_code_len..][0..4], endian));
    try std.testing.expectEqual(@as(u32, 1), readU32(blob[off_vars..][0..4], endian));
    try std.testing.expectEqual(@as(u32, 5), readU32(blob[off_regs..][0..4], endian));

    const offs = sectionOffsets(@sizeOf(Header), 2, 5);
    try std.testing.expectEqual(sizeOfProgram(&p), offs.file_size);
    try std.testing.expectEqual(offs.file_size, blob.len);

    if (offs.constants_off > @sizeOf(Header))
        try isZeroed(blob[@sizeOf(Header)..offs.constants_off]);
    if (offs.dst_off > offs.ops_off + 5)
        try isZeroed(blob[offs.ops_off + 5 .. offs.dst_off]);
    // constants: [5, 3]
    try std.testing.expectEqual(@as(i64, 5), readI64(blob[offs.constants_off + 0 ..][0..8], endian));
    try std.testing.expectEqual(@as(i64, 3), readI64(blob[offs.constants_off + 8 ..][0..8], endian));
    // ops
    const ops_bytes = blob[offs.ops_off .. offs.ops_off + 5];
    const expect_ops = [_]u8{
        @intFromEnum(operations.Op.iconst),
        @intFromEnum(operations.Op.iconst),
        @intFromEnum(operations.Op.add),
        @intFromEnum(operations.Op.store),
        @intFromEnum(operations.Op.load),
    };
    try std.testing.expectEqualSlices(u8, &expect_ops, ops_bytes);

    // dst: [0,1,2,3,4]
    for (0..5) |i| {
        const got = readU32(blob[offs.dst_off + i * 4 ..][0..4], endian);
        try std.testing.expectEqual(@as(u32, @intCast(i)), got);
    }

    // a: [0,0,0,2,0]
    const expect_a = [_]u32{ 0, 0, 0, 2, 0 };
    for (expect_a, 0..) |v, i| {
        const got = readU32(blob[offs.a_off + i * 4 ..][0..4], endian);
        try std.testing.expectEqual(v, got);
    }

    // b: [0,0,1,0,0]
    const expect_b = [_]u32{ 0, 0, 1, 0, 0 };
    for (expect_b, 0..) |v, i| {
        const got = readU32(blob[offs.b_off + i * 4 ..][0..4], endian);
        try std.testing.expectEqual(v, got);
    }

    // aux: [0,1,0,0,0]
    const expect_aux = [_]u32{ 0, 1, 0, 0, 0 };
    for (expect_aux, 0..) |v, i| {
        const got = readU32(blob[offs.aux_off + i * 4 ..][0..4], endian);
        try std.testing.expectEqual(v, got);
    }

    var aw2: std.Io.Writer.Allocating = .init(allocator);
    defer aw2.deinit();
    try writeProgram(&aw2.writer, &p, opts);
    try std.testing.expectEqualSlices(u8, blob, aw2.written());
}

test "program (little-endian)" {
    try testProgramEndianness(std.testing.allocator, .little);
}

test "program (big-endian)" {
    try testProgramEndianness(std.testing.allocator, .big);
}

const Ir = @import("Ir.zig");
const builtin = @import("builtin");
const operations = @import("operations.zig");
const mem = @import("mem.zig");
const std = @import("std");
