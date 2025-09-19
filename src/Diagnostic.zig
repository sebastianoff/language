tag: Tag,
/// report "at" nearest token.
index: u32,
/// what we actually saw.
got: tokenizer.Token.Tag,

pub const Map = struct {
    /// byte offsets of the first bytes of each line, 0-based.
    starts: std.ArrayList(u32) = .empty,
    /// the total length of the source in bytes.
    source_len: u32,
    /// cache for monotonic lookups
    last_lookup_idx: u32 = 0,

    /// counts `\n`, `\r`, and `\r\n` as line endings.
    pub fn init(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Map {
        var map: Map = .{
            .source_len = @intCast(source.len),
        };
        errdefer map.deinit(allocator);
        // assume average 80 characters per line
        try map.starts.ensureUnusedCapacity(allocator, source.len / 80 + 2);
        map.starts.appendAssumeCapacity(0);

        var search_from: usize = 0;
        while (search_from < source.len) {
            const next_break = std.mem.indexOfAny(u8, source[search_from..], "\r\n");
            if (next_break) |found_at| {
                const i = search_from + found_at;
                if (source[i] == '\r' and i + 1 < source.len and source[i + 1] == '\n') {
                    // CRLF
                    try map.starts.append(allocator, @intCast(i + 2));
                    search_from = i + 2;
                } else {
                    // lone \r or \n
                    try map.starts.append(allocator, @intCast(i + 1));
                    search_from = i + 1;
                }
            } else {
                break;
            }
        }
        return map;
    }

    pub fn deinit(map: *Map, allocator: std.mem.Allocator) void {
        map.starts.deinit(allocator);
        map.* = undefined;
    }

    pub const Location = struct {
        line: u32,
        col: u32,
        pub const Span = struct { start: Location, end: Location };
    };

    pub fn lookup(map: *Map, offset: u32) Location {
        const clamped_offset = @min(offset, map.source_len);

        // search forward from the last-used index.
        var line_index = map.last_lookup_idx;
        if (clamped_offset < map.starts.items[line_index]) {
            // not monotonic so do a full binary search.
            line_index = @intCast(std.sort.upperBound(u32, map.starts.items, clamped_offset, struct {
                fn compare(context: u32, item: u32) std.math.Order {
                    return std.math.order(context, item);
                }
            }.compare) - 1);
        } else {
            // search forward from last position.
            while (line_index + 1 < map.starts.items.len and
                clamped_offset >= map.starts.items[line_index + 1])
            {
                line_index += 1;
            }
        }
        map.last_lookup_idx = line_index;

        const line_start_offset = map.starts.items[line_index];
        return .{
            .line = @intCast(line_index + 1),
            .col = clamped_offset - line_start_offset + 1,
        };
    }

    pub fn lookupSlice(map: *Map, span: tokenizer.Token.Span) Location.Span {
        const start = map.lookup(span.start);
        // the end of the span is inclusive, so we subtract 1 from the length.
        // but if length is 0, we want the same location.
        const end_offset = if (span.len == 0) span.start else span.start + span.len - 1;
        const end = map.lookup(end_offset);
        return .{ .start = start, .end = end };
    }
};

pub const Tag = enum {
    parse_expected_identifier,
    parse_expected_equal,
    parse_expected_semicolon,
    parse_expected_expression,
    parse_expected_statement,
};

const std = @import("std");
const tokenizer = @import("tokenizer.zig");
