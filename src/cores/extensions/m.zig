const component = @import("../../rv_types.zig").component;

pub fn mul(a: u32, b: u32) u32 {
    const comp: component = .{
        .dword_s = @as(i32, @bitCast(a)) * @as(i32, @bitCast(b)),
    };
    return comp.word[0];
}

pub fn mulh(a: u32, b: u32) u32 {
    const comp: component = .{
        .dword_s = @as(i32, @bitCast(a)) * @as(i32, @bitCast(b)),
    };
    return comp.word[1];
}

pub fn mulhsu(a: u32, b: u32) u32 {
    const rs1: i64 = @as(i32, @bitCast(a));
    const rs2: i64 = @as(i64, @bitCast(@as(u64, @intCast(b))));
    const comp: component = .{
        .dword_s = rs1 * rs2,
    };

    return comp.word[1];
}

pub fn mulhu(a: u32, b: u32) u32 {
    const comp: component = .{
        .dword = a * b,
    };
    return comp.word[1];
}

pub fn div(rs1: i32, rs2: i32) u32 {
    return @as(u32, @bitCast(@divFloor(rs1, rs2)));
}

pub fn divu(rs1: u32, rs2: u32) u32 {
    return rs1 / rs2;
}

pub fn rem(rs1: i32, rs2: i32) u32 {
    return @as(u32, @bitCast(@rem(rs1, rs2)));
}

pub fn remu(rs1: u32, rs2: u32) u32 {
    return rs1 % rs2;
}
