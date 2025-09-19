tag: Tag,
/// report "at" nearest token.
index: u32,
/// what we actually saw.
got: tokenizer.Token.Tag,

pub const Tag = enum {
    parse_expected_identifier,
    parse_expected_equal,
    parse_expected_semicolon,
    parse_expected_expression,
    parse_expected_statement,
};

const tokenizer = @import("tokenizer.zig");
