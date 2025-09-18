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

    // zig fmt: off
    pub const Tag = enum(u8) {
        identifier,
        number,
        string,
        invalid_number,
        invalid_character,
        comment,
        // delimiters
        l_paren, r_paren, l_bracket, r_bracket, l_brace, r_brace,
        comma, semicolon, colon, colon_colon,
        // alternation and pipelines
        pipe,              // |
        pipe_pipe,         // ||
        pipe_greater,      // |>
        less_pipe,         // <|
        // dots
        dot, dot_dot,      // ., ..
        // arrows
        arrow,             // ->
        fat_arrow,         // =>
        // equality and comparisons
        equal,             // =
        equal_equal,       // ==
        not_equal,         // != or /=
        less, less_equal,  // <, <=
        greater, greater_equal, // >, >=
        // boolean ops
        amp_amp,           // &&
        // arithmetic
        plus, minus, star, slash, percent,
    };
    // zig fmt: on
};

pub fn tokenize(allocator: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!std.MultiArrayList(Token) {
    var tokens: std.MultiArrayList(Token) = .empty;
    errdefer tokens.deinit(allocator);

    if (source.len > 0) {
        try tokens.ensureTotalCapacity(allocator, @max(16, source.len / 4));
    }

    var cursor: usize = 0;
    while (cursor < source.len) {
        const c = source[cursor];

        if (c == ' ' or c == '\n' or c == '\t' or c == '\r') {
            cursor += 1;
            while (cursor < source.len) {
                const w = source[cursor];
                if (w == ' ' or w == '\n' or w == '\t' or w == '\r') {
                    cursor += 1;
                } else break;
            }
            continue;
        }

        // [A-Za-z_][A-Za-z0-9_]*
        if (isIdentifierStart(c)) {
            const start_idx = cursor;
            while (cursor < source.len and isIdentifierPart(source[cursor])) {
                cursor += 1;
            }
            try tokens.append(allocator, .{
                .tag = .identifier,
                .span = .init(@intCast(start_idx), @intCast(cursor - start_idx)),
            });
            continue;
        }
        // [0-9]+, with invalid_number if followed by [A-Za-z_]
        if (std.ascii.isDigit(c)) {
            const start_idx = cursor;
            while (cursor < source.len and std.ascii.isDigit(source[cursor])) {
                cursor += 1;
            }

            if (cursor < source.len and isIdentifierStart(source[cursor])) {
                while (cursor < source.len and isIdentifierPart(source[cursor])) {
                    cursor += 1;
                }
                try tokens.append(allocator, .{
                    .tag = .invalid_number,
                    .span = .init(@intCast(start_idx), @intCast(cursor - start_idx)),
                });
            } else {
                try tokens.append(allocator, .{
                    .tag = .number,
                    .span = .init(@intCast(start_idx), @intCast(cursor - start_idx)),
                });
            }
            continue;
        }

        if (scanPunct(source, cursor)) |p| {
            try tokens.append(allocator, .{
                .tag = p.tag,
                .span = .init(@intCast(cursor), @intCast(p.len)),
            });
            cursor += p.len;
            continue;
        }

        try tokens.append(allocator, .{
            .tag = .invalid_character,
            .span = .init(@intCast(cursor), 1),
        });
        cursor += 1;
    }

    return tokens;
}

fn isIdentifierStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

fn isIdentifierPart(c: u8) bool {
    return std.ascii.isAlphanumeric(c) or c == '_';
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
};

const Punct = struct {
    len: usize,
    tag: Token.Tag,
};

pub inline fn scanPunct(source: []const u8, i: usize) ?Punct {
    if (i >= source.len) return null;

    const a = source[i];
    const b: u8 = if (i + 1 < source.len) source[i + 1] else 0;

    switch (a) {
        '/' => {
            if (b == '/') {
                var j = i + 2;
                while (j < source.len and source[j] != '\n') j += 1;
                return .{ .len = j - i, .tag = .comment };
            }
            if (b == '=') return .{ .len = 2, .tag = .not_equal }; // '/='
            return .{ .len = 1, .tag = .slash };
        },

        '(' => return .{ .len = 1, .tag = .l_paren },
        ')' => return .{ .len = 1, .tag = .r_paren },

        '[' => return .{ .len = 1, .tag = .l_bracket },
        ']' => return .{ .len = 1, .tag = .r_bracket },
        '{' => return .{ .len = 1, .tag = .l_brace },
        '}' => return .{ .len = 1, .tag = .r_brace },

        ',' => return .{ .len = 1, .tag = .comma },
        ';' => return .{ .len = 1, .tag = .semicolon },

        ':' => {
            if (b == ':') return .{ .len = 2, .tag = .colon_colon };
            return .{ .len = 1, .tag = .colon };
        },

        '|' => {
            if (b == '>') return .{ .len = 2, .tag = .pipe_greater }; // |>
            if (b == '|') return .{ .len = 2, .tag = .pipe_pipe }; // ||
            return .{ .len = 1, .tag = .pipe }; // |
        },
        '<' => {
            if (b == '|') return .{ .len = 2, .tag = .less_pipe }; // <|
            if (b == '=') return .{ .len = 2, .tag = .less_equal };
            return .{ .len = 1, .tag = .less };
        },
        '>' => {
            if (b == '=') return .{ .len = 2, .tag = .greater_equal };
            return .{ .len = 1, .tag = .greater };
        },

        '.' => {
            if (b == '.') return .{ .len = 2, .tag = .dot_dot };
            return .{ .len = 1, .tag = .dot };
        },

        '=' => {
            if (b == '=') return .{ .len = 2, .tag = .equal_equal };
            if (b == '>') return .{ .len = 2, .tag = .fat_arrow }; // =>
            return .{ .len = 1, .tag = .equal };
        },
        '-' => {
            if (b == '>') return .{ .len = 2, .tag = .arrow }; // ->
            return .{ .len = 1, .tag = .minus };
        },

        '&' => {
            if (b == '&') return .{ .len = 2, .tag = .amp_amp }; // &&
            return null;
        },
        '!' => {
            if (b == '=') return .{ .len = 2, .tag = .not_equal }; // !=
            return null;
        },

        '+' => return .{ .len = 1, .tag = .plus },
        '*' => return .{ .len = 1, .tag = .star },
        '%' => return .{ .len = 1, .tag = .percent },

        else => return null,
    }
}

const std = @import("std");
