//! n frontend. Lexer/tokenizer.

pub const Token = struct {
    tag: Tag,
    span: Span,

    pub const Span = struct {
        start: u32,
        len: u32,

        pub fn init(start: u32, len: u32) Span {
            return .{ .start = start, .len = len };
        }
    };

    pub const Tag = enum {
        identifier,
        number,
        unknown,
    };
};

pub fn tokenize(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!std.MultiArrayList(Token) {
    var tokens: std.MultiArrayList(Token) = .empty;
    errdefer tokens.deinit(allocator);
    // assume average len is 4 characters.
    try tokens.ensureTotalCapacity(allocator, source.len / 4);
    var cursor: usize = 0;
    const lut = Class.lut;
    while (cursor < source.len) {
        const first_char_class = lut[source[cursor]];

        switch (first_char_class) {
            .whitespace => {
                // tight.
                cursor += 1;
                while (cursor < source.len and lut[source[cursor]] == .whitespace) {
                    cursor += 1;
                }
            },
            .letter, .digit, .other, .slash => {
                if (first_char_class == .slash and cursor + 1 < source.len and source[cursor + 1] == '/') {
                    // since it's a comment it would be faster to skip to the end of line
                    // rather than tokenizing it.
                    cursor += 2;
                    while (cursor < source.len and source[cursor] != '\n') {
                        cursor += 1;
                    }
                    continue;
                }

                const start_idx = cursor;
                var prev_class = first_char_class;
                cursor += 1;
                while (cursor < source.len) {
                    const current_class = lut[source[cursor]];
                    if (!isContinuation(prev_class, current_class)) {
                        break;
                    }
                    prev_class = current_class;
                    cursor += 1;
                }
                const end_idx = cursor;
                const tag: Token.Tag = switch (first_char_class) {
                    .letter => .identifier,
                    .digit => .number,
                    .slash,
                    .other,
                    => .unknown,
                    .whitespace => unreachable, // handled above
                };
                try tokens.append(allocator, .{
                    .tag = tag,
                    .span = .init(@intCast(start_idx), @intCast(end_idx - start_idx)),
                });
            },
        }
    }

    return tokens;
}

const Class = enum(u8) {
    /// ' ', '\n', '\t', '\r'
    whitespace,
    /// 'a'...'z', 'A'...'Z', '_'
    letter,
    /// '0'...'9'
    digit,
    /// '/'
    slash,
    // any other character.
    other,

    const lut_len: usize = 256;
    const lut: [lut_len]Class = lut: {
        var table: [lut_len]Class = undefined;
        for (0..table.len) |code| {
            table[code] = switch (code) {
                ' ', '\n', '\t', '\r' => .whitespace,
                'a'...'z', 'A'...'Z', '_' => .letter,
                '0'...'9' => .digit,
                '/' => .slash,
                else => .other,
            };
        }
        break :lut table;
    };
};

const transition_len: usize = 5;
const transition: [transition_len][transition_len]bool = transition: {
    // zig fmt: off
    const table: [transition_len][transition_len]bool = .{
        // current:  .whitespace, .letter, .digit, .slash, .other
        // prev:
        .{           true,        false,   false,  false,  false  }, // .whitespace
        .{           false,       true,    true,   false,  false  }, // .letter
        .{           false,       false,   true,   false,  false  }, // .digit
        .{           false,       false,   false,  false,  false  }, // .slash
        .{           false,       false,   false,  false,  true   }, // .other
    };
    // zig fmt: on
    break :transition table;
};

/// a token chunk continues if the transition from the previous `Class` to the current one is valid.
/// e.g., (letter, digit) is a valid continuation for an identifier.
fn isContinuation(previous: Class, current: Class) bool {
    return transition[@intFromEnum(previous)][@intFromEnum(current)];
}

const std = @import("std");
