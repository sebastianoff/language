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

        pub fn slice(self: Span, source: []const u8) []const u8 {
            return source[self.start .. self.start + self.len];
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

        if (std.ascii.isWhitespace(c)) {
            cursor += 1;
            while (cursor < source.len and std.ascii.isWhitespace(source[cursor])) {
                cursor += 1;
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
            const num: Number = .init(source, start_idx);

            try tokens.append(allocator, .{
                .tag = if (num.valid) .number else .invalid_number,
                .span = .init(@intCast(start_idx), @intCast(num.len)),
            });
            cursor += num.len;
            continue;
        }

        if (Punct.init(source, cursor)) |p| {
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

    pub inline fn init(source: []const u8, i: usize) ?Punct {
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
};

const Number = struct {
    len: usize,
    valid: bool,

    const Base = enum(u2) { dec, hex, oct, bin };

    pub fn init(source: []const u8, start: usize) Number {
        var i = start;
        const n = source.len;

        if (i >= n) return .{ .len = 0, .valid = false };
        // 0[xobXOB]
        if (source[i] == '0' and i + 1 < n) {
            const p = source[i + 1] | 0x20; // normalize to lowercase
            const base: ?Base = switch (p) {
                'x' => .hex,
                'o' => .oct,
                'b' => .bin,
                else => null,
            };
            if (base) |b| {
                i += 2; // skip prefix
                var seen_digit = false;
                var prev_us = false;
                var invalid = false;

                while (i < n) {
                    const ch = source[i];
                    if (ch == '_') {
                        // underscores cannot be leading, trailing, or doubled
                        if (!seen_digit or prev_us) invalid = true;
                        prev_us = true;
                        i += 1;
                        continue;
                    }

                    if (isDigitInBase(ch, b)) {
                        seen_digit = true;
                        prev_us = false;
                        i += 1;
                        continue;
                    }
                    // if identifier-ish, eat the rest as invalid number tail
                    if (isIdentifierPart(ch)) {
                        invalid = true;
                        i = consumeIdentTail(source, i + 1);
                    }
                    break;
                }

                if (!seen_digit or prev_us) invalid = true;
                return .{ .len = i - start, .valid = !invalid };
            }
        }
        // decimal
        var seen_digit = false;
        var prev_us = false;
        var invalid = false;

        while (i < n) {
            const ch = source[i];
            if (ch == '_') {
                if (!seen_digit or prev_us) invalid = true;
                prev_us = true;
                i += 1;
                continue;
            }
            if (isDecDigit(ch)) {
                seen_digit = true;
                prev_us = false;
                i += 1;
                continue;
            }
            // identifier-ish immediately after digits makes it invalid so consume the tail
            if (isIdentifierPart(ch)) {
                invalid = true;
                i = consumeIdentTail(source, i + 1);
            }
            break;
        }

        if (!seen_digit or prev_us) invalid = true;
        return .{ .len = i - start, .valid = !invalid };
    }

    fn isDecDigit(ch: u8) bool {
        return ch >= '0' and ch <= '9';
    }

    fn isDigitInBase(ch: u8, base: Base) bool {
        return switch (base) {
            .bin => ch == '0' or ch == '1',
            .oct => ch >= '0' and ch <= '7',
            .hex => {
                if (ch >= '0' and ch <= '9') return true;
                const lc = ch | 0x20; // normalize to lowercase
                return lc >= 'a' and lc <= 'f';
            },
            .dec => unreachable, // not used in prefix path
        };
    }

    fn consumeIdentTail(source: []const u8, start: usize) usize {
        var i = start;
        const n = source.len;
        while (i < n and isIdentifierPart(source[i])) : (i += 1) {}
        return i;
    }
};

fn testTokenize(source: []const u8, expected: []const struct {
    tag: Token.Tag,
    start: u32,
    len: u32,
}) !void {
    const allocator = std.testing.allocator;

    var toks = try tokenize(allocator, source);
    defer toks.deinit(allocator);

    try std.testing.expectEqual(expected.len, toks.len);

    const got_tags = toks.items(.tag);
    const got_spans = toks.items(.span);

    var i: usize = 0;
    while (i < expected.len) : (i += 1) {
        try std.testing.expectEqual(expected[i].tag, got_tags[i]);
        try std.testing.expectEqual(Token.Span.init(expected[i].start, expected[i].len), got_spans[i]);
    }
}

test "empty and whitespace-only" {
    try testTokenize("", &.{});
    try testTokenize(" \t\r\n   \n\r\t", &.{});
}

test "identifiers happy path" {
    // "foo _bar baz123 _xyz_123"
    try testTokenize("foo _bar baz123 _xyz_123", &.{
        .{ .tag = .identifier, .start = 0, .len = 3 }, // foo
        .{ .tag = .identifier, .start = 4, .len = 4 }, // _bar
        .{ .tag = .identifier, .start = 9, .len = 6 }, // baz123
        .{ .tag = .identifier, .start = 16, .len = 8 }, // _xyz_123
    });
}

test "slash and comments" {
    // single slash
    try testTokenize("/", &.{
        .{ .tag = .slash, .start = 0, .len = 1 },
    });
    // line comment to EOF
    try testTokenize("//abc", &.{
        .{ .tag = .comment, .start = 0, .len = 5 }, // includes the 2 slashes + "abc"
    });
    // identifier + comment + identifier on next line
    // "a//c\nb"
    try testTokenize("a//c\nb", &.{
        .{ .tag = .identifier, .start = 0, .len = 1 }, // a
        .{ .tag = .comment, .start = 1, .len = 3 }, // //c (stops before '\n')
        .{ .tag = .identifier, .start = 5, .len = 1 }, // b
    });
}

test "single-character punctuations (parens, brackets, braces)" {
    try testTokenize("()", &.{
        .{ .tag = .l_paren, .start = 0, .len = 1 },
        .{ .tag = .r_paren, .start = 1, .len = 1 },
    });
    try testTokenize("[]", &.{
        .{ .tag = .l_bracket, .start = 0, .len = 1 },
        .{ .tag = .r_bracket, .start = 1, .len = 1 },
    });
    try testTokenize("{}", &.{
        .{ .tag = .l_brace, .start = 0, .len = 1 },
        .{ .tag = .r_brace, .start = 1, .len = 1 },
    });
}

test "single-character delimiters" {
    try testTokenize(",;:", &.{
        .{ .tag = .comma, .start = 0, .len = 1 },
        .{ .tag = .semicolon, .start = 1, .len = 1 },
        .{ .tag = .colon, .start = 2, .len = 1 },
    });
}

test "single-character operators and comparisons" {
    // "=<>+-*/%"
    try testTokenize("=<>+-*/%", &.{
        .{ .tag = .equal, .start = 0, .len = 1 },
        .{ .tag = .less, .start = 1, .len = 1 },
        .{ .tag = .greater, .start = 2, .len = 1 },
        .{ .tag = .plus, .start = 3, .len = 1 },
        .{ .tag = .minus, .start = 4, .len = 1 },
        .{ .tag = .star, .start = 5, .len = 1 },
        .{ .tag = .slash, .start = 6, .len = 1 },
        .{ .tag = .percent, .start = 7, .len = 1 },
    });
}

test "dot and double dot" {
    try testTokenize(".", &.{
        .{ .tag = .dot, .start = 0, .len = 1 },
    });
    try testTokenize("..", &.{
        .{ .tag = .dot_dot, .start = 0, .len = 2 },
    });
    // greedy: "..." -> ".." + "."
    try testTokenize("...", &.{
        .{ .tag = .dot_dot, .start = 0, .len = 2 },
        .{ .tag = .dot, .start = 2, .len = 1 },
    });
}

test "pipe variants" {
    try testTokenize("|", &.{
        .{ .tag = .pipe, .start = 0, .len = 1 },
    });
    try testTokenize("||", &.{
        .{ .tag = .pipe_pipe, .start = 0, .len = 2 },
    });
    try testTokenize("|>", &.{
        .{ .tag = .pipe_greater, .start = 0, .len = 2 },
    });
    // greedy "|||": "||", "|"
    try testTokenize("|||", &.{
        .{ .tag = .pipe_pipe, .start = 0, .len = 2 },
        .{ .tag = .pipe, .start = 2, .len = 1 },
    });
}

test "less/greater combos" {
    try testTokenize("<", &.{
        .{ .tag = .less, .start = 0, .len = 1 },
    });
    try testTokenize(">", &.{
        .{ .tag = .greater, .start = 0, .len = 1 },
    });
    try testTokenize("<=", &.{
        .{ .tag = .less_equal, .start = 0, .len = 2 },
    });
    try testTokenize(">=", &.{
        .{ .tag = .greater_equal, .start = 0, .len = 2 },
    });
    try testTokenize("<|", &.{
        .{ .tag = .less_pipe, .start = 0, .len = 2 },
    });
    // greedy "<==": "<=", "="
    try testTokenize("<==", &.{
        .{ .tag = .less_equal, .start = 0, .len = 2 },
        .{ .tag = .equal, .start = 2, .len = 1 },
    });
}

test "equality, arrows, double-colon" {
    try testTokenize("==", &.{
        .{ .tag = .equal_equal, .start = 0, .len = 2 },
    });
    try testTokenize("=>", &.{
        .{ .tag = .fat_arrow, .start = 0, .len = 2 },
    });
    try testTokenize("->", &.{
        .{ .tag = .arrow, .start = 0, .len = 2 },
    });
    // greedy "==>" -> "==", ">"
    try testTokenize("==>", &.{
        .{ .tag = .equal_equal, .start = 0, .len = 2 },
        .{ .tag = .greater, .start = 2, .len = 1 },
    });
    try testTokenize("::", &.{
        .{ .tag = .colon_colon, .start = 0, .len = 2 },
    });
}

test "boolean and not-equal variants" {
    try testTokenize("&&", &.{
        .{ .tag = .amp_amp, .start = 0, .len = 2 },
    });
    try testTokenize("/=", &.{
        .{ .tag = .not_equal, .start = 0, .len = 2 },
    });
    try testTokenize("!=", &.{
        .{ .tag = .not_equal, .start = 0, .len = 2 },
    });
    // gsreedy "/=/" -> "/=", "/"
    try testTokenize("/=/", &.{
        .{ .tag = .not_equal, .start = 0, .len = 2 },
        .{ .tag = .slash, .start = 2, .len = 1 },
    });
}

test "invalid characters - lone & and !" {
    try testTokenize("&", &.{
        .{ .tag = .invalid_character, .start = 0, .len = 1 },
    });
    try testTokenize("!", &.{
        .{ .tag = .invalid_character, .start = 0, .len = 1 },
    });
}

test "number formats" {
    try testTokenize("0 42 123 1_234_567 0x123_abc 0XDEAD_BEEF 0o123_4567 0b1010_0101", &.{
        .{ .tag = .number, .start = 0, .len = 1 }, // 0
        .{ .tag = .number, .start = 2, .len = 2 }, // 42
        .{ .tag = .number, .start = 5, .len = 3 }, // 123
        .{ .tag = .number, .start = 9, .len = 9 }, // 1_234_567
        .{ .tag = .number, .start = 19, .len = 9 }, // 0x123_abc
        .{ .tag = .number, .start = 29, .len = 11 }, // 0XDEAD_BEEF
        .{ .tag = .number, .start = 41, .len = 10 }, // 0o123_4567
        .{ .tag = .number, .start = 52, .len = 11 }, // 0b1010_0101
    });

    // with surrounding text
    try testTokenize("let x = 0x123;", &.{
        .{ .tag = .identifier, .start = 0, .len = 3 },
        .{ .tag = .identifier, .start = 4, .len = 1 },
        .{ .tag = .equal, .start = 6, .len = 1 },
        .{ .tag = .number, .start = 8, .len = 5 },
        .{ .tag = .semicolon, .start = 13, .len = 1 },
    });
}

test "invalid number formats" {
    try testTokenize("123abc 7_ 9a_b 123_ 1__23 0x12g 0o128 0b102 0x 0b_ 123a", &.{
        .{ .tag = .invalid_number, .start = 0, .len = 6 }, // 123abc
        .{ .tag = .invalid_number, .start = 7, .len = 2 }, // 7_
        .{ .tag = .invalid_number, .start = 10, .len = 4 }, // 9a_b
        .{ .tag = .invalid_number, .start = 15, .len = 4 }, // 123_
        .{ .tag = .invalid_number, .start = 20, .len = 5 }, // 1__23
        .{ .tag = .invalid_number, .start = 26, .len = 5 }, // 0x12g
        .{ .tag = .invalid_number, .start = 32, .len = 5 }, // 0o128
        .{ .tag = .invalid_number, .start = 38, .len = 5 }, // 0b102
        .{ .tag = .invalid_number, .start = 44, .len = 2 }, // 0x
        .{ .tag = .invalid_number, .start = 47, .len = 3 }, // 0b_
        .{ .tag = .invalid_number, .start = 51, .len = 4 }, // 123a
    });
}

fn allocSafeTokenize(allocator: std.mem.Allocator, src: []const u8) !void {
    var toks = try tokenize(allocator, src);
    toks.deinit(allocator);
}

test "allocation failure" {
    const src =
        "id_1 123abc //comment\n" ++
        "a&&b|>c<|d::e==f=>g->h<=42>=7!=/=\n" ++
        "... () [] {} ,;: +-* / % < > = |";

    try std.testing.checkAllAllocationFailures(
        std.testing.allocator,
        allocSafeTokenize,
        .{src},
    );
}

const std = @import("std");
