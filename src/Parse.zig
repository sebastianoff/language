//! n frontend. Parse.
ast: *Ast,
cursor: u32 = 0,
tokens: std.MultiArrayList(tokenizer.Token).Slice,
diagnostics: std.ArrayList(Diagnostic) = .empty,

pub fn init(ast: *Ast) Parse {
    return .{ .ast = ast, .tokens = ast.tokens.slice() };
}

pub fn deinit(parse: *Parse, allocator: std.mem.Allocator) void {
    parse.diagnostics.deinit(allocator);
}

pub const TokenInfo = struct {
    tag: tokenizer.Token.Tag,
    index: u32,
    /// Inserted/missing token.
    synthetic: bool = false,
};

const OperatorKind = enum(u2) {
    none,
    infix_left,
    /// e.g. an exponent or an assignment
    infix_right,
};

const OperatorInfo = struct {
    kind: OperatorKind = .none,
    lbp: u8 = 0,
    rbp: u8 = 0,

    fn infixLeft(bp: u8) OperatorInfo {
        // left-associative
        // rhs min-bp = lbp + 1
        return .{ .kind = .infix_left, .lbp = bp, .rbp = bp + 1 };
    }

    fn infixRight(bp: u8) OperatorInfo {
        // right-associative
        // rhs min-bp = lbp
        return .{ .kind = .infix_right, .lbp = bp, .rbp = bp };
    }
};

inline fn operatorInfo(tag: tokenizer.Token.Tag) OperatorInfo {
    const node = Ast.binaryTag(tag);
    if (node) |nt| {
        const precedence = Ast.binaryPrecedence(nt);
        // everything here is left-associative
        return .infixLeft(precedence);
    }
    return .{};
}

pub const sets = struct {
    pub const statement_stop = &.{ .semicolon, .invalid_character, .identifier };
    pub const rhs_stop = &.{ .semicolon, .invalid_character, .identifier };

    pub fn inSet(comptime set: []const tokenizer.Token.Tag, tag: tokenizer.Token.Tag) bool {
        inline for (set) |t| {
            if (t == tag) return true;
        }
        return false;
    }
};

pub fn appendError(parse: *Parse, allocator: std.mem.Allocator, tag: Diagnostic.Tag, at: TokenInfo) void {
    if (parse.diagnostics.items.len >= 64) {
        // force advance to eof to stop.
        parse.cursor = @intCast(parse.tokens.len);
        return;
    }
    parse.diagnostics.append(allocator, .{ .tag = tag, .index = at.index, .got = at.tag }) catch return;
}

pub fn appendNode(parse: *Parse, allocator: std.mem.Allocator, node: Ast.Node) std.mem.Allocator.Error!Ast.Node.Index {
    const index: u32 = @intCast(parse.ast.nodes.items.len);
    try parse.ast.nodes.append(allocator, node);
    return index;
}

/// scan forward to any token in `stop`.
fn recoverUntil(parse: *Parse, comptime stop: []const tokenizer.Token.Tag, mode: enum { keep, consume }) TokenInfo {
    var index = parse.cursor;
    const tags = parse.tokens.items(.tag);
    while (index < tags.len) : (index += 1) {
        const tag = tags[index];
        if (tag != .comment and sets.inSet(stop, tag)) {
            break;
        }
    }
    // position the cursor and return the token we stopped at, or eof
    if (mode == .consume and index < tags.len) {
        parse.cursor = index + 1;
    } else {
        parse.cursor = index;
    }
    if (index >= tags.len) {
        return .{ .tag = .invalid_character, .index = @intCast(tags.len), .synthetic = false };
    }
    return .{ .tag = tags[index], .index = index, .synthetic = false };
}

/// expect a token; on failure either insert it or panic to sync set.
fn expect(
    parse: *Parse,
    allocator: std.mem.Allocator,
    want: tokenizer.Token.Tag,
    comptime sync: []const tokenizer.Token.Tag,
    mode: enum { insert, panic },
) TokenInfo {
    const token = parse.peek(.{});
    if (token.tag == want) {
        parse.cursor = token.index + 1;
        return token;
    }
    // record
    const tag: Diagnostic.Tag = switch (want) {
        .identifier => .parse_expected_identifier,
        .equal => .parse_expected_equal,
        .semicolon => .parse_expected_semicolon,
        else => .parse_expected_expression,
    };
    parse.appendError(allocator, tag, token);
    if (mode == .insert) {
        // do not advance
        return .{ .tag = want, .index = token.index, .synthetic = true };
    }
    // scan forward to sync, but don't consume the sync token itself
    const stop = parse.recoverUntil(sync, .keep);
    return .{ .tag = want, .index = stop.index, .synthetic = true };
}

/// transparent grouping.
/// (expr) doesn't allocate a node; returns the inner expression
fn nud(
    parse: *Parse,
    allocator: std.mem.Allocator,
    comptime sync: []const tokenizer.Token.Tag,
) std.mem.Allocator.Error!Ast.Node.Index {
    const before = parse.peek(.{});
    switch (before.tag) {
        .identifier => {
            _ = parse.match(&.{.identifier});
            return parse.appendNode(allocator, .{
                .tag = .identifier,
                .token_start = before.index,
                .token_len = 1,
                .data = .{ .none = {} },
            });
        },
        .number => {
            _ = parse.match(&.{.number});
            return parse.appendNode(allocator, .{
                .tag = .number_literal,
                .token_start = before.index,
                .token_len = 1,
                .data = .{ .none = {} },
            });
        },
        .l_paren => {
            // ( expr )
            // consume '('
            parse.cursor = before.index + 1;
            // parse inner with lowest min-bp
            const inner = try parse.expression(allocator, 0, sync);
            // expect ')', but insert on failure
            _ = parse.expect(allocator, .r_paren, sync, .insert);
            return inner; // no group node
        },
        else => {
            parse.appendError(allocator, .parse_expected_expression, before);
            if (before.tag != .semicolon) {
                _ = parse.recoverUntil(sync, .consume);
            }
            return try parse.appendNode(
                allocator,
                .{ .tag = .@"error", .token_start = before.index, .token_len = 0, .data = .{ .none = {} } },
            );
        },
    }
}

pub fn expression(
    parse: *Parse,
    allocator: std.mem.Allocator,
    min_bp: u8,
    comptime sync: []const tokenizer.Token.Tag,
) std.mem.Allocator.Error!Ast.Node.Index {
    // parse left
    var lhs = try parse.nud(allocator, sync);
    // while we have an infix operator with lbp >= min_bp, consume it and parse RHS with rbp
    while (true) {
        const op = parse.peek(.{});
        // stop if we reached end or a token from sync set
        if (op.tag == .invalid_character or sets.inSet(sync, op.tag)) break;

        const info = operatorInfo(op.tag);
        if (info.kind == .none) break;
        if (info.lbp < min_bp) break;
        // consume operator
        parse.cursor = op.index + 1;
        // parse RHS with rbp
        const rhs = try parse.expression(allocator, info.rbp, sync);
        // build operator node
        const lhs_node = parse.ast.nodes.items[lhs];
        const start = lhs_node.token_start;

        const node = Ast.binaryTag(op.tag) orelse unreachable;

        const end = parse.cursor; // current token cursor after parsing RHS
        lhs = try parse.appendNode(allocator, .{
            .tag = node,
            .token_start = start,
            .token_len = @intCast(end - start),
            .data = .{ .binary_op = .{ .lhs = lhs, .rhs = rhs } },
        });
    }
    return lhs;
}

pub fn statement(parse: *Parse, allocator: std.mem.Allocator) std.mem.Allocator.Error!Ast.Node.Index {
    if (parse.peek(.{}).tag == .identifier and parse.peek(.{ .offset = 1 }).tag == .equal) {
        return parse.assignmentStatement(allocator);
    }
    // error
    const at = parse.peek(.{});
    parse.appendError(allocator, .parse_expected_statement, at);
    _ = parse.recoverUntil(sets.statement_stop, .consume);
    return parse.appendNode(allocator, .{
        .tag = .@"error",
        .token_start = at.index,
        .token_len = 0,
        .data = .{ .none = {} },
    });
}

pub fn atEnd(parse: *Parse) bool {
    return parse.peek(.{}).tag == .invalid_character;
}

fn assignmentStatement(parse: *Parse, allocator: std.mem.Allocator) !Ast.Node.Index {
    const start = parse.peek(.{}).index;
    // lhs
    const lhs_token = parse.expect(allocator, .identifier, sets.statement_stop, .panic);
    const lhs_node = if (lhs_token.synthetic)
        try parse.appendNode(allocator, .{
            .tag = .@"error",
            .token_start = lhs_token.index,
            .token_len = 0,
            .data = .{ .none = {} },
        })
    else
        try parse.appendNode(allocator, .{
            .tag = .identifier,
            .token_start = lhs_token.index,
            .token_len = 1,
            .data = .{ .none = {} },
        });

    // '='
    _ = parse.expect(allocator, .equal, sets.statement_stop, .insert);
    // rhs
    const rhs_node = try parse.expression(allocator, 0, sets.rhs_stop);
    // ';'
    _ = parse.expect(allocator, .semicolon, sets.statement_stop, .insert);
    // if nothing advanced, consume one sync token
    if (parse.cursor == start and !parse.atEnd()) {
        _ = parse.recoverUntil(sets.statement_stop, .consume);
    }
    const end = parse.cursor;
    return try parse.appendNode(
        allocator,
        .{
            .tag = .assign,
            .token_start = start,
            .token_len = @intCast(end - start),
            .data = .{ .binary_op = .{ .lhs = lhs_node, .rhs = rhs_node } },
        },
    );
}

fn peek(parse: *Parse, options: struct { offset: u32 = 0 }) TokenInfo {
    const tags = parse.tokens.items(.tag);
    var index = parse.cursor;
    var i: u32 = 0;
    while (i <= options.offset) {
        // skip comments
        while (index < tags.len and tags[index] == .comment) {
            index += 1;
        }
        if (i == options.offset) break;
        // if we are at the end after skipping comments, we can't advance further.
        if (index >= tags.len) break;

        index += 1;
        i += 1;
    }

    if (index >= tags.len) {
        return .{ .tag = .invalid_character, .index = @intCast(tags.len) };
    }
    return .{ .tag = tags[index], .index = index };
}

fn match(parse: *Parse, tags: []const tokenizer.Token.Tag) ?TokenInfo {
    const token = parse.peek(.{});
    for (tags) |tag| {
        if (token.tag == tag) {
            parse.cursor = token.index + 1;
            return token;
        }
    }
    return null;
}

fn expectNodeBasic(n: Ast.Node, tag: Ast.Node.Tag, token_start: u32, token_len: u16) !void {
    try std.testing.expectEqual(tag, n.tag);
    try std.testing.expectEqual(token_start, n.token_start);
    try std.testing.expectEqual(token_len, n.token_len);
}

fn expectBinary(
    n: Ast.Node,
    tag: Ast.Node.Tag,
    token_start: u32,
    token_len: u16,
    lhs: Ast.Node.Index,
    rhs: Ast.Node.Index,
) !void {
    try expectNodeBasic(n, tag, token_start, token_len);
    try std.testing.expectEqual(lhs, n.data.binary_op.lhs);
    try std.testing.expectEqual(rhs, n.data.binary_op.rhs);
}

test "simple assignment x=1;" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=1;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1); // token 0: identifier x
    try expectNodeBasic(num, .number_literal, 2, 1); // token 2: number 1
    try expectNodeBasic(asn, .assign, 0, 4); // spanning tokens [0..4)

    try std.testing.expectEqual(@as(Ast.Node.Index, 0), asn.data.binary_op.lhs);
    try std.testing.expectEqual(@as(Ast.Node.Index, 1), asn.data.binary_op.rhs);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "assignment with comment between identifier and = (x//c\\n=1;)" {
    const allocator = std.testing.allocator;

    // Tokens: 0:id(x), 1:comment, 2:'=', 3:'1', 4:';'
    const tokens = try tokenizer.tokenize(allocator, "x//c\n=1;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(num, .number_literal, 3, 1);
    try expectNodeBasic(asn, .assign, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "assignment missing semicolon (x=1)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=1");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const num = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(num, .number_literal, 2, 1);
    try expectNodeBasic(asn, .assign, 0, 3); // no semicolon, end at tokens.len

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_semicolon, parse.diagnostics.items[0].tag);
}

test "missing RHS (x = ;)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const err = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(err, .@"error", 2, 0); // error at semicolon
    try expectNodeBasic(asn, .assign, 0, 3);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_expression, parse.diagnostics.items[0].tag);
}

test "invalid RHS token (x = &;)" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "x=&;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 3), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const err = ast.nodes.items[1];
    const asn = ast.nodes.items[2];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(err, .@"error", 2, 0); // error at invalid_character '&'
    try expectNodeBasic(asn, .assign, 0, 4);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_expression, parse.diagnostics.items[0].tag);
}

test "statement starting with a number" {
    const allocator = std.testing.allocator;

    const tokens = try tokenizer.tokenize(allocator, "42;");

    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);

    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);
    while (!parse.atEnd()) {
        _ = try parse.statement(allocator);
    }

    try std.testing.expectEqual(@as(usize, 1), ast.nodes.items.len);

    const err = ast.nodes.items[0];
    try expectNodeBasic(err, .@"error", 0, 0);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_statement, parse.diagnostics.items[0].tag);
}

test "precedence mul > add (x=1+2*3;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=1+2*3;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 7), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const n2 = ast.nodes.items[2];
    const n3 = ast.nodes.items[3];
    const mul = ast.nodes.items[4];
    const add = ast.nodes.items[5];
    const asn = ast.nodes.items[6];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 2, 1);
    try expectNodeBasic(n2, .number_literal, 4, 1);
    try expectNodeBasic(n3, .number_literal, 6, 1);
    try expectBinary(mul, .mul, 4, 3, 2, 3); // 2*3 spans [4..7)
    try expectBinary(add, .add, 2, 5, 1, 4); // 1+(2*3) spans [2..7)
    try expectBinary(asn, .assign, 0, 8, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "left associativity for minus (x=10-3-2;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=10-3-2;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 7), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n10 = ast.nodes.items[1];
    const n3 = ast.nodes.items[2];
    const sub1 = ast.nodes.items[3];
    const n2 = ast.nodes.items[4];
    const sub2 = ast.nodes.items[5];
    const asn = ast.nodes.items[6];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n10, .number_literal, 2, 1);
    try expectNodeBasic(n3, .number_literal, 4, 1);
    try expectBinary(sub1, .sub, 2, 3, 1, 2); // (10-3) [2..5)
    try expectNodeBasic(n2, .number_literal, 6, 1);
    try expectBinary(sub2, .sub, 2, 5, 3, 4); // (10-3)-2 [2..7)
    try expectBinary(asn, .assign, 0, 8, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "parentheses override precedence (x=(1+2)*3;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=(1+2)*3;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 7), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const n2 = ast.nodes.items[2];
    const add = ast.nodes.items[3];
    const n3 = ast.nodes.items[4];
    const mul = ast.nodes.items[5];
    const asn = ast.nodes.items[6];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 3, 1);
    try expectNodeBasic(n2, .number_literal, 5, 1);
    try expectBinary(add, .add, 3, 3, 1, 2); // (1+2) [3..6)
    try expectNodeBasic(n3, .number_literal, 8, 1);
    try expectBinary(mul, .mul, 3, 6, 3, 4); // (1+2)*3 [3..9)
    try expectBinary(asn, .assign, 0, 10, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "comparison binds tighter than equality (x=1<2==3;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=1<2==3;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 7), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const n2 = ast.nodes.items[2];
    const less = ast.nodes.items[3];
    const n3 = ast.nodes.items[4];
    const eq = ast.nodes.items[5];
    const asn = ast.nodes.items[6];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 2, 1);
    try expectNodeBasic(n2, .number_literal, 4, 1);
    try expectBinary(less, .less, 2, 3, 1, 2); // 1<2 [2..5)
    try expectNodeBasic(n3, .number_literal, 6, 1);
    try expectBinary(eq, .equal_equal, 2, 5, 3, 4); // (1<2)==3 [2..7)
    try expectBinary(asn, .assign, 0, 8, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "range binds looser than add (x=1..2+3;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=1..2+3;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 7), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const n2 = ast.nodes.items[2];
    const n3 = ast.nodes.items[3];
    const add = ast.nodes.items[4];
    const rng = ast.nodes.items[5];
    const asn = ast.nodes.items[6];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 2, 1);
    try expectNodeBasic(n2, .number_literal, 4, 1);
    try expectNodeBasic(n3, .number_literal, 6, 1);
    try expectBinary(add, .add, 4, 3, 2, 3); // 2+3 [4..7)
    try expectBinary(rng, .range, 2, 5, 1, 4); // 1..(2+3) [2..7)
    try expectBinary(asn, .assign, 0, 8, 0, 5);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "pipeline vs boolean precedence (x=a|b&&c||d;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=a|b&&c||d;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 9), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const a = ast.nodes.items[1];
    const b = ast.nodes.items[2];
    const pipe = ast.nodes.items[3];
    const c = ast.nodes.items[4];
    const andn = ast.nodes.items[5];
    const d = ast.nodes.items[6];
    const orn = ast.nodes.items[7];
    const asn = ast.nodes.items[8];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(a, .identifier, 2, 1);
    try expectNodeBasic(b, .identifier, 4, 1);
    try expectBinary(pipe, .pipe, 2, 3, 1, 2); // a|b [2..5)
    try expectNodeBasic(c, .identifier, 6, 1);
    try expectBinary(andn, .@"and", 2, 5, 3, 4); // (a|b)&&c [2..7)
    try expectNodeBasic(d, .identifier, 8, 1);
    try expectBinary(orn, .@"or", 2, 7, 5, 6); // ((a|b)&&c)||d [2..9)
    try expectBinary(asn, .assign, 0, 10, 0, 7);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "comments inside expression (x=1//c\\n+2;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=1//c\n+2;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 5), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const n2 = ast.nodes.items[2];
    const add = ast.nodes.items[3];
    const asn = ast.nodes.items[4];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 2, 1);
    try expectNodeBasic(n2, .number_literal, 5, 1);
    try expectBinary(add, .add, 2, 4, 1, 2); // spans tokens [2..6) including the comment in the range
    try expectBinary(asn, .assign, 0, 7, 0, 3);

    try std.testing.expectEqual(@as(usize, 0), parse.diagnostics.items.len);
}

test "missing RHS after operator (x=1+;)" {
    const allocator = std.testing.allocator;
    const tokens = try tokenizer.tokenize(allocator, "x=1+;");
    var ast: Ast = .init(tokens);
    defer ast.deinit(allocator);
    var parse: Parse = .init(&ast);
    defer parse.deinit(allocator);

    while (!parse.atEnd()) _ = try parse.statement(allocator);

    try std.testing.expectEqual(@as(usize, 5), ast.nodes.items.len);

    const id = ast.nodes.items[0];
    const n1 = ast.nodes.items[1];
    const err = ast.nodes.items[2];
    const add = ast.nodes.items[3];
    const asn = ast.nodes.items[4];

    try expectNodeBasic(id, .identifier, 0, 1);
    try expectNodeBasic(n1, .number_literal, 2, 1);
    try expectNodeBasic(err, .@"error", 4, 0); // error at ';'
    try expectBinary(add, .add, 2, 2, 1, 2); // only covers '1+'
    try expectBinary(asn, .assign, 0, 5, 0, 3);

    try std.testing.expectEqual(@as(usize, 1), parse.diagnostics.items.len);
    try std.testing.expectEqual(Diagnostic.Tag.parse_expected_expression, parse.diagnostics.items[0].tag);
}

const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Diagnostic = @import("Diagnostic.zig");
const Ast = @import("Ast.zig");
const Parse = @This();
