pub const MapError = error{
    AccessDenied,
    InvalidAddress,
    OutOfMemory,
    Invalid,
    NotAligned,
    FileTooSmall,
    NotSupported,
    SystemResources,
    PermissionDenied,
    Busy,
    Unexpected,
    InputOutput,
    FileTooBig,
    FileBusy,
    NonResizable,
} || std.posix.MMapError || std.posix.MSyncError;

pub const Protection = struct {
    read: bool = true,
    write: bool = false,
    exec: bool = false,
};

pub const Advice = enum {
    normal,
    sequential,
    random,
    willneed,
    dontneed,
};

pub const Options = struct {
    /// required for mapping functions
    length: usize,
    /// file offset (page/alignment adjusted automatically)
    offset: u64 = 0,
    /// R/W/X
    protect: Protection = .{},
    /// MAP_SHARED vs MAP_PRIVATE (Windows: shared vs copy-on-write)
    shared: bool = false,
    /// Linux: MAP_POPULATE | Windows: PrefetchVirtualMemory best-effort
    populate: bool = false,
    /// try to mlock/VirtualLock (best-effort)
    /// not supported on posix yet
    lock: bool = false,
    /// best-effort large/huge pages
    large_pages: bool = false,
    /// For file mapping
    grow_file: bool = false,
    /// anonymous: reserve only (commit later). Windows: MEM_RESERVE; POSIX: PROT_NONE.
    reserve_only: bool = false,
    /// if true, do not fallback when a best-effort feature fails
    strict: bool = false,
};

pub const Mapping = struct {
    /// slice of the user-visible mapping window [ptr .. ptr+len)
    ptr: [*]u8,
    len: usize,
    /// the actual view/base (aligned).
    view_base: [*]u8,
    view_len: usize,
    delta: usize,
    /// OS payload
    data: Data,

    const Data = if (builtin.os.tag == .windows) struct {
        /// null for anonymous VirtualAlloc mappings
        file_mapping: ?std.os.windows.HANDLE,
        /// not strictly required
        file_handle: ?std.os.windows.HANDLE,
        /// not strictly required
        is_anon: bool,
    } else struct {
        /// -1 for anonymous
        fd: std.posix.fd_t,
    };

    pub fn slice(self: *const Mapping) []u8 {
        return self.ptr[0..self.len];
    }

    pub fn unmap(self: *Mapping) void {
        if (builtin.os.tag == .windows) {
            self.winUnmap();
        } else {
            self.posixUnmap();
        }
        self.* = undefined;
    }

    pub fn protect(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        if (length == 0) return;
        if (builtin.os.tag == .windows) {
            try self.winProtect(offset, length, prot);
        } else {
            try self.posixProtect(offset, length, prot);
        }
    }

    pub const FlushMode = enum { sync, async };
    /// flush changes back to storage (no-op for anonymous)
    /// if `mode == sync`, use synchronous flush.
    pub fn flush(self: *Mapping, offset: usize, length: usize, mode: FlushMode) MapError!void {
        if (length == 0) return;
        if (builtin.os.tag == .windows) {
            try self.winFlush(offset, length, mode);
        } else {
            try self.posixFlush(offset, length, mode);
        }
    }

    /// best-effort advice
    pub fn advise(self: *Mapping, offset: usize, length: usize, advice: Advice) void {
        if (length == 0) return;
        if (builtin.os.tag == .windows) {
            self.winAdvise(offset, length, advice);
        } else {
            self.posixAdvise(offset, length, advice);
        }
    }

    /// commit or decommit only meaningful for some OS/types. On POSIX, decommit uses MADV_DONTNEED.
    pub fn commit(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        if (length == 0) return;
        if (builtin.os.tag == .windows) {
            try self.winCommit(offset, length, prot);
        } else {
            try self.posixCommit(offset, length, prot);
        }
    }

    pub fn decommit(self: *Mapping, offset: usize, length: usize) MapError!void {
        if (length == 0) return;
        if (builtin.os.tag == .windows) {
            try self.winDecommit(offset, length);
        } else {
            try self.posixDecommit(offset, length);
        }
    }

    fn winUnmap(self: *Mapping) void {
        const w = std.os.windows;
        if (self.data.is_anon) {
            if (@intFromPtr(self.view_base) != 0) {
                _ = w.VirtualFree(self.view_base, 0, w.MEM_RELEASE);
            }
        } else {
            if (@intFromPtr(self.view_base) != 0) {
                _ = UnmapViewOfFile(self.view_base);
            }
            if (self.data.file_mapping) |h| {
                _ = w.CloseHandle(h);
            }
        }
    }

    fn winFlush(self: *Mapping, offset: usize, length: usize, mode: FlushMode) MapError!void {
        _ = mode;
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const start_al = std.mem.alignBackward(usize, base, std.heap.pageSize());
        const len_al = std.mem.alignForward(usize, end - start_al, std.heap.pageSize());

        const ok: std.os.windows.BOOL = FlushViewOfFile(@ptrFromInt(start_al), len_al);
        if (ok == 0) return winErrToMapError(std.os.windows.kernel32.GetLastError());
    }

    fn winProtect(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        const w = std.os.windows;
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const start_al = std.mem.alignBackward(usize, base, std.heap.pageSize());
        const len_al = std.mem.alignForward(usize, end - start_al, std.heap.pageSize());

        var old: w.DWORD = 0;
        w.VirtualProtect(@ptrFromInt(start_al), len_al, protToWinProtect(prot), &old) catch |err| switch (err) {
            else => |e| return e,
        };
    }

    fn winCommit(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        if (!self.data.is_anon) return error.NotSupported;

        const w = std.os.windows;
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const start_al = std.mem.alignBackward(usize, base, std.heap.pageSize());
        const len_al = std.mem.alignForward(usize, end - start_al, std.heap.pageSize());

        _ = try w.VirtualAlloc(@ptrFromInt(start_al), len_al, w.MEM_COMMIT, protToWinProtect(prot));
    }

    fn winDecommit(self: *Mapping, offset: usize, length: usize) MapError!void {
        if (!self.data.is_anon) return error.NotSupported;
        const w = std.os.windows;
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const start_al = std.mem.alignBackward(usize, base, std.heap.pageSize());
        const len_al = std.mem.alignForward(usize, end - start_al, std.heap.pageSize());

        w.VirtualFree(@ptrFromInt(start_al), len_al, w.MEM_DECOMMIT);
    }

    fn winAdvise(self: *Mapping, offset: usize, length: usize, advice: Advice) void {
        if (advice != .willneed) return;
        const w = std.os.windows;

        const entry: WIN32_MEMORY_RANGE_ENTRY = .{
            .VirtualAddress = @ptrCast(self.ptr + offset),
            .NumberOfBytes = length,
        };
        // hProcess = current process (pseudo handle)
        _ = PrefetchVirtualMemory(w.GetCurrentProcess(), 1, &.{entry}, 0);
    }

    fn posixUnmap(self: *Mapping) void {
        const posix = std.posix;
        if (self.view_len == 0) return;
        const mem: []align(std.heap.page_size_min) const u8 =
            @alignCast(self.view_base[0..self.view_len]);
        _ = posix.munmap(mem);
    }

    fn posixFlush(self: *Mapping, offset: usize, length: usize, mode: FlushMode) MapError!void {
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const page = std.heap.pageSize();
        const start_al = std.mem.alignBackward(usize, base, page);
        const len_al = std.mem.alignForward(usize, end - start_al, page);
        if (len_al == 0) return;

        const mem_unaligned: []u8 = @as([*]u8, @ptrFromInt(start_al))[0..len_al];
        const mem: []align(std.heap.page_size_min) u8 = @alignCast(mem_unaligned);
        const flags: i32 = if (mode == .sync) std.posix.MSF.SYNC else std.posix.MSF.ASYNC;
        try std.posix.msync(mem, flags);
    }

    fn posixProtect(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const page = std.heap.pageSize();
        const start_al = std.mem.alignBackward(usize, base, page);
        const len_al = std.mem.alignForward(usize, end - start_al, page);
        if (len_al == 0) return;

        const mem_unaligned: []u8 = @as([*]u8, @ptrFromInt(start_al))[0..len_al];
        const mem: []align(std.heap.page_size_min) u8 = @alignCast(mem_unaligned);
        try std.posix.mprotect(mem, protToPosix(prot));
    }

    fn posixCommit(self: *Mapping, offset: usize, length: usize, prot: Protection) MapError!void {
        try self.posixProtect(offset, length, prot);
        self.posixAdvise(offset, length, .willneed);
    }

    fn posixDecommit(self: *Mapping, offset: usize, length: usize) MapError!void {
        const posix = std.posix;
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const start_al = std.mem.alignBackward(usize, base, std.heap.pageSize());
        const len_al = std.mem.alignForward(usize, end - start_al, std.heap.pageSize());
        if (len_al == 0) return;
        const ptr_al: [*]align(std.heap.page_size_min) u8 = @ptrFromInt(start_al);
        _ = posix.madvise(ptr_al, len_al, posix.MADV.DONTNEED) catch {};
    }

    fn posixAdvise(self: *Mapping, offset: usize, length: usize, advice: Advice) void {
        const posix = std.posix;
        const a: u32 = switch (advice) {
            .normal => posix.MADV.NORMAL,
            .sequential => posix.MADV.SEQUENTIAL,
            .random => posix.MADV.RANDOM,
            .willneed => posix.MADV.WILLNEED,
            .dontneed => posix.MADV.DONTNEED,
        };
        const base = @intFromPtr(self.ptr) + offset;
        const end = base + length;
        const page = std.heap.pageSize();
        const start_al = std.mem.alignBackward(usize, base, page);
        const len_al = std.mem.alignForward(usize, end - start_al, page);
        if (len_al == 0) return;
        const ptr_al: [*]align(std.heap.page_size_min) u8 = @ptrFromInt(start_al);
        _ = posix.madvise(ptr_al, len_al, a) catch {};
    }
};

pub fn mapAnonymous(opts: Options) MapError!Mapping {
    if (opts.length == 0) return error.Invalid;

    if (builtin.os.tag == .windows) {
        return winMapAnon(opts);
    } else {
        return posixMapAnon(opts);
    }
}

pub fn map(file: std.fs.File, opts: Options) MapError!Mapping {
    if (opts.length == 0) return error.Invalid;

    if (builtin.os.tag == .windows) {
        return winMapFile(file, opts);
    } else {
        return posixMapFile(file, opts);
    }
}

const WIN32_MEMORY_RANGE_ENTRY = extern struct {
    VirtualAddress: *anyopaque,
    NumberOfBytes: usize,
};

extern "kernel32" fn PrefetchVirtualMemory(
    hProcess: std.os.windows.HANDLE,
    NumberOfEntries: std.os.windows.ULONG_PTR,
    VirtualAddresses: [*]const WIN32_MEMORY_RANGE_ENTRY,
    Flags: std.os.windows.ULONG,
) callconv(.winapi) std.os.windows.BOOL;

extern "kernel32" fn CreateFileMappingW(
    hFile: std.os.windows.HANDLE,
    lpAttributes: ?*std.os.windows.SECURITY_ATTRIBUTES,
    flProtect: std.os.windows.DWORD,
    dwMaximumSizeHigh: std.os.windows.DWORD,
    dwMaximumSizeLow: std.os.windows.DWORD,
    lpName: ?std.os.windows.LPCWSTR,
) callconv(.winapi) ?std.os.windows.HANDLE;

extern "kernel32" fn MapViewOfFile(
    hFileMappingObject: std.os.windows.HANDLE,
    dwDesiredAccess: std.os.windows.DWORD,
    dwFileOffsetHigh: std.os.windows.DWORD,
    dwFileOffsetLow: std.os.windows.DWORD,
    dwNumberOfBytesToMap: usize,
) callconv(.winapi) ?*anyopaque;

extern "kernel32" fn UnmapViewOfFile(lpBaseAddress: *const anyopaque) callconv(.winapi) std.os.windows.BOOL;

extern "kernel32" fn FlushViewOfFile(lpBaseAddress: *const anyopaque, dwNumberOfBytesToFlush: usize) callconv(.winapi) std.os.windows.BOOL;

pub extern "kernel32" fn VirtualLock(lpAddress: std.os.windows.LPVOID, dwSize: std.os.windows.SIZE_T) callconv(.winapi) std.os.windows.BOOL;

fn winAllocGranularity() usize {
    var si: std.os.windows.SYSTEM_INFO = undefined;
    std.os.windows.kernel32.GetSystemInfo(&si);
    return @intCast(si.dwAllocationGranularity);
}

fn protToWinProtect(p: Protection) std.os.windows.DWORD {
    const w = std.os.windows;
    return if (p.exec)
        (if (p.write) w.PAGE_EXECUTE_READWRITE else if (p.read) w.PAGE_EXECUTE_READ else w.PAGE_EXECUTE)
    else
        (if (p.write) w.PAGE_READWRITE else if (p.read) w.PAGE_READONLY else w.PAGE_NOACCESS);
}

fn desiredAccessWin(p: Protection, shared: bool) std.os.windows.DWORD {
    // shared => FILE_MAP_READ / FILE_MAP_WRITE / FILE_MAP_EXECUTE
    // private (copy-on-write) => FILE_MAP_COPY (+ optional EXECUTE)
    const w = std.os.windows;
    if (shared) {
        var access: w.DWORD = 0;
        if (p.read) access |= 0x00000004; // FILE_MAP_READ
        if (p.write) access |= 0x00000002; // FILE_MAP_WRITE
        if (p.exec) access |= 0x00000020; // FILE_MAP_EXECUTE
        return access;
    } else {
        var access: w.DWORD = 0x00000001; // FILE_MAP_COPY
        if (p.exec) access |= 0x00000020; // FILE_MAP_EXECUTE
        return access;
    }
}

fn winErrToMapError(e: std.os.windows.Win32Error) MapError {
    return switch (e) {
        .ACCESS_DENIED => error.AccessDenied,
        .INVALID_PARAMETER => error.Invalid,
        .NOT_ENOUGH_MEMORY, .COMMITMENT_LIMIT => error.OutOfMemory,
        .SHARING_VIOLATION => error.PermissionDenied,
        .FILE_INVALID, .HANDLE_EOF => error.FileTooSmall,
        .NOT_SUPPORTED => error.NotSupported,
        else => error.Unexpected,
    };
}

fn winMapAnon(opts: Options) MapError!Mapping {
    const w = std.os.windows;
    const size = std.mem.alignForward(usize, opts.length, std.heap.pageSize());

    var flAlloc: w.DWORD = w.MEM_RESERVE;
    if (!opts.reserve_only) flAlloc |= w.MEM_COMMIT;
    if (opts.large_pages) flAlloc |= w.MEM_LARGE_PAGES;

    const flProtect = if (opts.reserve_only) w.PAGE_NOACCESS else protToWinProtect(opts.protect);

    const base = try w.VirtualAlloc(null, size, flAlloc, flProtect);

    var m: Mapping = .{
        .ptr = @ptrCast(@alignCast(base)),
        .len = opts.length,
        .view_base = @ptrCast(base),
        .view_len = size,
        .delta = 0,
        .data = .{ .file_mapping = null, .file_handle = null, .is_anon = true },
    };
    if (opts.lock) {
        _ = VirtualLock(m.view_base, m.view_len);
    }
    if (opts.populate and !opts.reserve_only) {
        m.winAdvise(0, m.len, .willneed);
    }
    return m;
}

fn winMapFile(file: std.fs.File, opts: Options) MapError!Mapping {
    const w = std.os.windows;

    var st = try file.stat();
    const needed_end: u64 = opts.offset + @as(u64, @intCast(opts.length));
    if (needed_end > st.size) {
        if (!opts.grow_file) return error.FileTooSmall;
        try file.setEndPos(needed_end);
        st = try file.stat(); // refresh
    }

    const gran = winAllocGranularity();
    const page = std.heap.pageSize();

    const aligned_off: u64 = opts.offset / gran * @as(u64, gran);
    const delta: usize = @intCast(opts.offset - aligned_off);
    // mapping object max size should fit file size
    const max_size = st.size;
    const max_hi: w.DWORD = @intCast((max_size >> 32) & 0xFFFF_FFFF);
    const max_lo: w.DWORD = @intCast(max_size & 0xFFFF_FFFF);

    var flProtect: w.DWORD = 0;
    if (opts.shared) {
        flProtect = protToWinProtect(opts.protect);
    } else {
        if (opts.protect.write) {
            flProtect = if (opts.protect.exec) 0x80 else 0x08; // PAGE_EXECUTE_WRITECOPY or PAGE_WRITECOPY
        } else {
            flProtect = if (opts.protect.exec) w.PAGE_EXECUTE_READ else w.PAGE_READONLY;
        }
    }
    if (opts.large_pages) flProtect |= 0x80000000; // SEC_LARGE_PAGES
    const hmap = CreateFileMappingW(file.handle, null, flProtect, max_hi, max_lo, null) orelse {
        return winErrToMapError(w.kernel32.GetLastError());
    };
    const desired = desiredAccessWin(opts.protect, opts.shared);
    const off_hi: w.DWORD = @intCast((aligned_off >> 32) & 0xFFFF_FFFF);
    const off_lo: w.DWORD = @intCast(aligned_off & 0xFFFF_FFFF);
    const view = MapViewOfFile(hmap, desired, off_hi, off_lo, 0) orelse {
        const err = winErrToMapError(w.kernel32.GetLastError());
        _ = w.CloseHandle(hmap);
        return err;
    };
    const req_view_len = std.mem.alignForward(usize, delta + opts.length, page);

    var m: Mapping = .{
        .ptr = @as([*]u8, @ptrFromInt(@intFromPtr(view) + delta)),
        .len = opts.length,
        .view_base = @ptrCast(view),
        .view_len = req_view_len,
        .delta = delta,
        .data = .{ .file_mapping = hmap, .file_handle = file.handle, .is_anon = false },
    };
    if (opts.lock) {
        _ = VirtualLock(m.view_base, m.view_len);
    }
    if (opts.populate) {
        m.winAdvise(0, m.len, .willneed);
    }

    return m;
}

fn protToPosix(p: Protection) u32 {
    var prot: u32 = 0;
    if (p.read) prot |= std.posix.PROT.READ;
    if (p.write) prot |= std.posix.PROT.WRITE;
    if (p.exec) prot |= std.posix.PROT.EXEC;
    return prot;
}

fn posixMapAnon(opts: Options) MapError!Mapping {
    const posix = std.posix;
    const page = std.heap.pageSize();
    const view_len = std.mem.alignForward(usize, opts.length, page);
    const prot = if (opts.reserve_only) std.posix.PROT.NONE else protToPosix(opts.protect);

    var flags: posix.MAP = .{ .ANONYMOUS = true, .TYPE = if (opts.shared) .SHARED else .PRIVATE };

    if (opts.populate and builtin.os.tag == .linux) flags.POPULATE = true;
    if (opts.large_pages and builtin.os.tag == .linux) flags.HUGETLB = true;

    const base = try posix.mmap(null, view_len, prot, flags, -1, 0);
    var m: Mapping = .{
        .ptr = @ptrCast(base.ptr),
        .len = opts.length,
        .view_base = @ptrCast(base.ptr),
        .view_len = view_len,
        .delta = 0,
        .data = .{ .fd = -1 },
    };

    if (!opts.reserve_only and opts.populate and builtin.os.tag != .linux) {
        m.posixAdvise(0, m.len, .willneed);
    }
    //if (opts.lock) {
    //    // TODO: posix.mlock once 0.16.0 releases
    //}
    return m;
}

fn posixMapFile(file: std.fs.File, opts: Options) MapError!Mapping {
    const posix = std.posix;
    var st = try file.stat();

    const needed_end: u64 = opts.offset + @as(u64, @intCast(opts.length));
    if (needed_end > st.size) {
        if (!opts.grow_file) return error.FileTooSmall;
        try file.setEndPos(needed_end);
        st = try file.stat();
    }

    const page = std.heap.pageSize();
    const aligned_off: u64 = opts.offset / @as(u64, page) * @as(u64, page);
    const delta: usize = @intCast(opts.offset - aligned_off);
    const view_len = std.mem.alignForward(usize, delta + opts.length, page);

    const prot = protToPosix(opts.protect);
    const flags: posix.MAP = .{ .TYPE = if (opts.shared) .SHARED else .PRIVATE };

    const base = try posix.mmap(null, view_len, prot, flags, file.handle, aligned_off);
    var m: Mapping = .{
        .ptr = @as([*]u8, @ptrFromInt(@intFromPtr(base.ptr) + delta)),
        .len = opts.length,
        .view_base = @ptrCast(base.ptr),
        .view_len = view_len,
        .delta = delta,
        .data = .{ .fd = file.handle },
    };

    if (opts.populate and builtin.os.tag != .linux) {
        m.posixAdvise(0, m.len, .willneed);
    }
    //if (opts.lock) {
    //    // TODO: posix.mlock once 0.16.0 releases
    //}
    return m;
}

test "mapAnonymous: length == 0 returns error.Invalid" {
    try std.testing.expectError(error.Invalid, mapAnonymous(.{ .length = 0 }));
}

test "mapFile: length == 0 returns error.Invalid" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var f = try td.dir.createFile("zmap.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    try std.testing.expectError(error.Invalid, map(f, .{ .length = 0 }));
}

test "protect/advise/slice/unmap" {
    const pg = std.heap.pageSize();

    var m = try mapAnonymous(.{
        .length = pg * 2,
        .protect = .{ .read = true, .write = false, .exec = false },
        .populate = true,
        .lock = true,
    });
    defer m.unmap();

    inline for (.{ Advice.normal, Advice.sequential, Advice.random, Advice.willneed, Advice.dontneed }) |a| {
        m.advise(0, m.len, a);
    }

    try m.protect(16, pg - 16, .{ .read = true, .write = true, .exec = false });

    var s = m.slice();
    s[16] = 0x42;
    try std.testing.expectEqual(@as(u8, 0x42), s[16]);
}

test "reserve_only -> commit/decommit/commit" {
    const pg = std.heap.pageSize();

    var m = try mapAnonymous(.{
        .length = pg,
        .reserve_only = true,
    });
    defer m.unmap();

    try m.commit(0, pg, .{ .read = true, .write = true, .exec = false });

    var s = m.slice();
    s[0] = 0x55;
    try std.testing.expectEqual(@as(u8, 0x55), s[0]);

    try m.decommit(0, pg);

    try m.commit(0, pg, .{ .read = true, .write = true, .exec = false });
    s[0] = 0x66;
    try std.testing.expectEqual(@as(u8, 0x66), s[0]);
}

fn makePattern(buf: []u8) void {
    for (buf, 0..) |_, i| {
        buf[i] = @as(u8, @intCast(i & 0xFF));
    }
}

test "read with non-aligned offset, delta and view_len checks" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();

    const extra: usize = 123;
    const file_len: usize = 3 * pg + extra;

    var f = try td.dir.createFile("filemap_a.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    var tmp = try allocator.alloc(u8, file_len);
    defer allocator.free(tmp);
    makePattern(tmp);
    try f.writeAll(tmp);

    const offset: u64 = @intCast(pg + 37);
    const map_len: usize = pg + 111;

    var m = try map(f, .{
        .length = map_len,
        .offset = offset,
        .protect = .{ .read = true, .write = false, .exec = false },
        .shared = true,
        .populate = true,
    });
    defer m.unmap();

    const gran = if (builtin.os.tag == .windows) winAllocGranularity() else pg;
    const aligned_off: u64 = offset / @as(u64, gran) * @as(u64, gran);
    const expected_delta: usize = @intCast(offset - aligned_off);
    const view_align = if (builtin.os.tag == .windows) std.heap.pageSize() else pg;
    const expected_view_len = std.mem.alignForward(usize, expected_delta + map_len, view_align);

    try std.testing.expectEqual(expected_delta, m.delta);
    try std.testing.expectEqual(expected_view_len, m.view_len);
    try std.testing.expectEqual(@as(usize, @intFromPtr(m.view_base)) + m.delta, @as(usize, @intFromPtr(m.ptr)));

    const slice = m.slice();
    try std.testing.expectEqual(slice.len, map_len);
    try std.testing.expectEqualSlices(u8, tmp[@intCast(offset)..@intCast(offset + map_len)], slice);

    m.advise(5, 17, .random);
}

test "changes persist after flush" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();
    const file_len: usize = 2 * pg;

    var f = try td.dir.createFile("filemap_shared.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    const buf = try allocator.alloc(u8, file_len);
    defer allocator.free(buf);
    @memset(buf, 0xAA);
    try f.writeAll(buf);

    var m = try map(f, .{
        .length = file_len,
        .offset = 0,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = true,
    });
    defer m.unmap();

    var s = m.slice();
    s[3] = 0x11;
    s[pg + 7] = 0x22;

    try m.flush(3, 1, .sync);
    try m.flush(pg + 7, 1, .sync);

    try f.seekTo(0);
    const back = try allocator.alloc(u8, file_len);
    defer allocator.free(back);
    const bytes_read = try f.readAll(back);
    try std.testing.expectEqual(back.len, bytes_read);

    try std.testing.expectEqual(@as(u8, 0x11), back[3]);
    try std.testing.expectEqual(@as(u8, 0x22), back[pg + 7]);
}

test "changes do NOT persist" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();
    const file_len: usize = 2 * pg;

    var f = try td.dir.createFile("filemap_private.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    const original = try allocator.alloc(u8, file_len);
    defer allocator.free(original);
    @memset(original, 0x5C);
    try f.writeAll(original);

    var m = try map(f, .{
        .length = file_len,
        .offset = 0,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = false, // private
    });
    defer m.unmap();

    var s = m.slice();
    s[0] = 0x99;
    s[pg - 1] = 0x77;
    s[pg] = 0x66;

    try f.seekTo(0);
    const back = try allocator.alloc(u8, file_len);
    defer allocator.free(back);
    const bytes_read = try f.readAll(back);
    try std.testing.expectEqual(back.len, bytes_read);

    try std.testing.expectEqualSlices(u8, original, back);
}

test "grow_file=false errors if length exceeds file size, grow_file=true extends file" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();

    var f = try td.dir.createFile("filemap_grow.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    try f.writeAll("hi");
    var st = try f.stat();
    try std.testing.expect(st.size <= 2);

    const offset: u64 = 0;
    const want_len: usize = pg + 10;
    const needed_end: u64 = offset + @as(u64, want_len);

    try std.testing.expectError(error.FileTooSmall, map(f, .{
        .length = want_len,
        .offset = offset,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = true,
        .grow_file = false,
    }));

    var m = try map(f, .{
        .length = want_len,
        .offset = offset,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = true,
        .grow_file = true,
    });
    defer m.unmap();

    var s = m.slice();
    s[0] = 0xE1;
    s[want_len - 1] = 0xE2;

    try m.flush(0, want_len, .sync);

    st = try f.stat();
    try std.testing.expectEqual(needed_end, st.size);

    try f.seekTo(0);
    const back = try allocator.alloc(u8, @intCast(st.size));
    defer allocator.free(back);
    const bytes_read = try f.readAll(back);
    try std.testing.expectEqual(back.len, bytes_read);

    try std.testing.expectEqual(@as(u8, 0xE1), back[0]);
    try std.testing.expectEqual(@as(u8, 0xE2), back[want_len - 1]);
}

test "slice returns correct view" {
    const pg = std.heap.pageSize();
    var m = try mapAnonymous(.{
        .length = pg + 7,
        .protect = .{ .read = true, .write = true, .exec = false },
    });
    defer m.unmap();

    const s = m.slice();
    try std.testing.expectEqual(m.len, s.len);
    try std.testing.expectEqual(@as(usize, @intFromPtr(m.ptr)), @intFromPtr(s.ptr));
}

test "flush accepts zero-length (no-op) and unaligned ranges" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();
    var f = try td.dir.createFile("filemap_flush.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    const buf = try allocator.alloc(u8, pg * 2);
    defer allocator.free(buf);
    @memset(buf, 0);
    try f.writeAll(buf);

    var m = try map(f, .{
        .length = buf.len,
        .offset = 0,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = true,
    });
    defer m.unmap();

    try m.flush(0, 0, .sync);
    try m.flush(0, 0, .async);

    var s = m.slice();
    s[5] = 0xAB;
    try m.flush(5, 3, .sync);

    try f.seekTo(0);
    const back = try allocator.alloc(u8, buf.len);
    defer allocator.free(back);
    const bytes_read = try f.readAll(back);
    try std.testing.expectEqual(back.len, bytes_read);

    try std.testing.expectEqual(@as(u8, 0xAB), back[5]);
}

test "commit/decommit" {
    const allocator = std.testing.allocator;

    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    const pg = std.heap.pageSize();
    var f = try td.dir.createFile("filemap_commit.dat", .{ .read = true, .truncate = true, .mode = 0o644 });
    defer f.close();

    const buf = try allocator.alloc(u8, pg);
    defer allocator.free(buf);
    @memset(buf, 0xCD);
    try f.writeAll(buf);

    var m = try map(f, .{
        .length = pg,
        .offset = 0,
        .protect = .{ .read = true, .write = true, .exec = false },
        .shared = true,
    });
    defer m.unmap();

    if (builtin.os.tag == .windows) {
        try std.testing.expectError(error.NotSupported, m.commit(0, pg, .{ .read = true, .write = true, .exec = false }));
        try std.testing.expectError(error.NotSupported, m.decommit(0, pg));
    } else {
        try m.commit(0, pg, .{ .read = true, .write = true, .exec = false });
        try m.decommit(0, pg);
    }
}

const std = @import("std");
const builtin = @import("builtin");
