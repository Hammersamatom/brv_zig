const std = @import("std");
const types = @import("rv_types.zig");

const compressed_encoding = extern union {
    raw: u16,
    cr: packed struct {
        op: u2,
        rs2: u5,
        rs_rs1: u5,
        funct4: u4,
    },
    ci: packed struct {
        op: u2,
        imm6_2: i5,
        rd_rs1: u5,
        imm12: i1,
        funct3: u3,
    },
    css: packed struct {
        op: u2,
        rs2: u5,
        imm: i6,
        funct3: u3,
    },
    ciw: packed struct {
        op: u2,
        rd_lim: u3,
        imm: packed union {
            raw: i8,
            bits: packed struct {
                bit0: i1,
                bit1: i1,
                bit2: i1,
                bit3: i1,
                bit4: i1,
                bit5: i1,
                bit6: i1,
                bit7: i1,
            },
        },
        funct3: u3,
    },
    cl: packed struct {
        op: u2,
        rd_lim: u3,
        imm6_5: packed union {
            raw: u2,
            raw_s: i2,
            bits: packed struct {
                bit5: u1,
                bit6: u1,
            },
        },
        rs1_lim: u3,
        imm12_10: packed union {
            raw: u3,
            raw_s: i3,
            bits: packed struct {
                bit10: u1,
                bit11: u1,
                bit12: u1,
            },
        },
        funct3: u3,
    },
    cs: packed struct {
        op: u2,
        rs2_lim: u3,
        imm6_5: packed union {
            raw: i2,
            bits: packed struct {
                bit5: u1,
                bit6: u1,
            },
        },
        rd_rs1_lim: u3,
        imm12_10: packed union {
            raw: i3,
            bits: packed struct {
                bit10: u1,
                bit11: u1,
                bit12: u1,
            },
        },
        funct3: u3,
    },
    ca: packed struct {
        op: u2,
        rs2_lim: u3,
        funct2: u2,
        rd_rs1_lim: u3,
        funct6: u6,
    },
    cb: packed struct {
        op: u2,
        imm6_2: i5,
        rs1_lim: u3,
        imm12_10: i3,
        funct3: u3,
    },
    cj: packed struct {
        op: u2,
        offset: packed union {
            raw: i11,
            bits: packed struct {
                bit0: u1,
                bit1: u1,
                bit2: u1,
                bit3: u1,
                bit4: u1,
                bit5: u1,
                bit6: u1,
                bit7: u1,
                bit8: u1,
                bit9: u1,
                bit10: u1,
            },
        },
        funct3: u3,
    },
};

pub fn zca_decoder(in: u16) u32 {
    const encoded_form: compressed_encoding = .{ .raw = in };
    return switch (encoded_form.cr.op) {
        0b00 => switch (encoded_form.ciw.funct3) {
            // c.addi4spn =>
            0b000 => out: {
                //const imm_recon = packed union {
                //    raw: u10,
                //    bits: packed struct {
                //        bit0: u1,
                //        bit1: u1,
                //        bit2: u1,
                //        bit3: u1,
                //        bit4: u1,
                //        bit5: u1,
                //        bit6: u1,
                //        bit7: u1,
                //        bit8: u1,
                //        bit9: u1,
                //    },
                //};
                //const ir: imm_recon = .{
                //    .bits = .{
                //        .bit0 = 0,
                //        .bit1 = 0,
                //        .bit2 = encoded_form.ciw.imm.bits.bit1,
                //        .bit3 = encoded_form.ciw.imm.bits.bit0,
                //        .bit4 = encoded_form.ciw.imm.bits.bit6,
                //        .bit5 = encoded_form.ciw.imm.bits.bit7,
                //        .bit6 = encoded_form.ciw.imm.bits.bit2,
                //        .bit7 = encoded_form.ciw.imm.bits.bit3,
                //        .bit8 = encoded_form.ciw.imm.bits.bit4,
                //        .bit9 = encoded_form.ciw.imm.bits.bit5,
                //    },
                //};
                //const instr: types.instr = .{
                //    .i_type = .{
                //        .opcode = @intFromEnum(types.inst_types.IMM),
                //        .rd = @as(u5, @intCast(encoded_form.ciw.rd_lim)) + 8,
                //        .funct3 = 0x0,
                //        .rs1 = 2,
                //        .imm = ir.raw,
                //    },
                //};
                //break :out instr.instruction;
                const imm_raw = packed union {
                    raw: i8,
                    imm: packed struct {
                        imm3: i1,
                        imm2: i1,
                        imm9_6: i4,
                        imm5_4: i2,
                    },
                };
                const ir_raw: imm_raw = .{ .raw = encoded_form.ciw.imm.raw };

                const imm_recon = packed union {
                    raw: i8,
                    imm: packed struct {
                        imm2: i1,
                        imm3: i1,
                        imm5_4: i2,
                        imm9_6: i4,
                    },
                };

                const ir: imm_recon = .{
                    .imm = .{
                        .imm2 = ir_raw.imm.imm2,
                        .imm3 = ir_raw.imm.imm3,
                        .imm5_4 = ir_raw.imm.imm5_4,
                        .imm9_6 = ir_raw.imm.imm9_6,
                    },
                };

                const instr: types.instr = .{
                    .i_type = .{
                        .opcode = @intFromEnum(types.inst_types.IMM),
                        .rd = @as(u5, @intCast(encoded_form.ciw.rd_lim)) + 8,
                        .funct3 = 0x0,
                        .rs1 = 2,
                        .imm = @as(i12, @intCast(ir.raw)) << 2,
                    },
                };
                break :out instr.instruction;
            },
            // c.lw
            0b010 => out: {
                const imm_recon = packed union {
                    raw: u7,
                    comp: packed struct {
                        unused: u2,
                        bit2: u1,
                        bits5_3: u3,
                        bit6: u1,
                    },
                };
                const ir: imm_recon = .{
                    .comp = .{
                        .unused = 0,
                        .bit2 = encoded_form.cl.imm6_5.bits.bit6,
                        .bits5_3 = encoded_form.cl.imm12_10.raw,
                        .bit6 = encoded_form.cl.imm6_5.bits.bit5,
                    },
                };
                const instr: types.instr = .{
                    .i_type = .{
                        .opcode = @intFromEnum(types.inst_types.LOAD),
                        .rd = @as(u5, @intCast(encoded_form.cl.rd_lim)) + 8,
                        .funct3 = 0x2,
                        .rs1 = @as(u5, @intCast(encoded_form.cl.rs1_lim)) + 8,
                        .imm = ir.raw,
                    },
                };

                break :out instr.instruction;
            },
            // c.sw
            0b110 => out: {
                const imm_recon = packed union {
                    raw: i12,
                    bits: packed struct {
                        bit0: u1,
                        bit1: u1,
                        bit2: u1,
                        bit3: u1,
                        bit4: u1,
                        bit5: u1,
                        bit6: u1,
                        bit7: u1,
                        bit8: u1,
                        bit9: u1,
                        bit10: u1,
                        bit11: u1,
                    },
                    comp: packed struct {
                        imm4_0: i5,
                        imm11_5: i7,
                    },
                };
                const ir: imm_recon = .{
                    .bits = .{
                        .bit0 = 0,
                        .bit1 = 0,
                        .bit2 = encoded_form.cs.imm6_5.bits.bit6,
                        .bit3 = encoded_form.cs.imm12_10.bits.bit10,
                        .bit4 = encoded_form.cs.imm12_10.bits.bit11,
                        .bit5 = encoded_form.cs.imm12_10.bits.bit12,
                        .bit6 = encoded_form.cs.imm6_5.bits.bit5,
                        .bit7 = 0,
                        .bit8 = 0,
                        .bit9 = 0,
                        .bit10 = 0,
                        .bit11 = 0,
                    },
                };
                const instr: types.instr = .{
                    .s_type = .{
                        .opcode = @intFromEnum(types.inst_types.STORE),
                        .imm4_0 = ir.comp.imm4_0,
                        .funct3 = 0x2,
                        .rs1 = @as(u5, @intCast(encoded_form.cs.rd_rs1_lim)) + 8,
                        .rs2 = @as(u5, @intCast(encoded_form.cs.rs2_lim)) + 8,
                        .imm11_5 = ir.comp.imm11_5,
                    },
                };

                break :out instr.instruction;
            },
            else => return 0,
        },
        0b01 => switch (encoded_form.ci.funct3) {
            // c.addi
            0b000 => out: {
                const imm_recon = packed union {
                    raw: i6,
                    comp: packed struct {
                        imm4_0: i5,
                        imm5: i1,
                    },
                };
                const ir: imm_recon = .{
                    .comp = .{
                        .imm4_0 = encoded_form.ci.imm6_2,
                        .imm5 = encoded_form.ci.imm12,
                    },
                };
                const instr: types.instr = .{
                    .i_type = .{
                        .opcode = @as(u7, @intFromEnum(types.inst_types.IMM)),
                        .rd = encoded_form.ci.rd_rs1,
                        .funct3 = 0x0,
                        .rs1 = encoded_form.ci.rd_rs1,
                        .imm = @intCast(ir.raw),
                    },
                };
                break :out instr.instruction;
            },
            // c.jal
            0b001 => out: {
                const offset_rearrange = packed union {
                    raw: i11,
                    imm: packed struct {
                        imm5: i1,
                        imm3_1: i3,
                        imm7: i1,
                        imm6: i1,
                        imm10: i1,
                        imm9_8: i2,
                        imm4: i1,
                        imm11: i1,
                    },
                };
                const offset_bits: offset_rearrange = .{ .raw = encoded_form.cj.offset.raw };

                const imm_recon = packed union {
                    raw: i12,
                    imm: packed struct {
                        imm0: i1,
                        imm3_1: i3,
                        imm4: i1,
                        imm5: i1,
                        imm6: i1,
                        imm7: i1,
                        imm9_8: i2,
                        imm10: i1,
                        imm11: i1,
                    },
                };
                const ir: imm_recon = .{
                    .imm = .{
                        .imm0 = 0,

                        .imm3_1 = offset_bits.imm.imm3_1,
                        .imm4 = offset_bits.imm.imm4,
                        .imm5 = offset_bits.imm.imm5,
                        .imm6 = offset_bits.imm.imm6,
                        .imm7 = offset_bits.imm.imm7,
                        .imm9_8 = offset_bits.imm.imm9_8,
                        .imm10 = offset_bits.imm.imm10,
                        .imm11 = offset_bits.imm.imm11,
                    },
                };

                const j_imm = packed union {
                    raw: i21,
                    comp: packed struct {
                        imm0: i1,
                        imm10_1: i10,
                        imm11: i1,
                        imm19_12: i8,
                        imm20: i1,
                    },
                };
                const ir_ext: j_imm = .{
                    .raw = ir.raw, // << 1,
                };

                const instr: types.instr = .{
                    .j_type = .{
                        .opcode = @intFromEnum(types.inst_types.JAL),
                        .rd = 1, //ra
                        .imm19_12 = ir_ext.comp.imm19_12,
                        .imm11 = ir_ext.comp.imm11,
                        .imm10_1 = ir_ext.comp.imm10_1,
                        .imm20 = ir_ext.comp.imm20,
                    },
                };
                break :out instr.instruction;
            },
            0b010 => 0,
            0b011 => 0,
            0b100 => switch (encoded_form.ca.funct6) {
                else => 0,
            },
            0b101 => 0,
            0b110 => 0,
            0b111 => 0,
        },
        0b10 => 0,
        0b11 => 0, // Should never even get this far.
    };
}

test "c.addi4spn" {
    const input = 0x0028;
    const output = zca_decoder(input);
    const expected = 0x00810513;

    std.debug.print("Input: 0x{x:0>4}\n", .{input});
    std.debug.print("Output: 0x{x:0>8}\nExpected: 0x{x:0>8}\n", .{ output, expected });

    try std.testing.expect(output == expected);
}
test "c.lw" {
    const input = 0x4440;
    const output = zca_decoder(input);
    const expected = 0x00c42403;

    std.debug.print("Input: 0x{x:0>4}\n", .{input});
    std.debug.print("Output: 0x{x:0>8}\nExpected: 0x{x:0>8}\n", .{ output, expected });

    try std.testing.expect(output == expected);
}
test "c.sw" {
    const input = 0xc440;
    const output = zca_decoder(input);
    const expected = 0x00842623;

    std.debug.print("Input: 0x{x:0>4}\n", .{input});
    std.debug.print("Output: 0x{x:0>8}\nExpected: 0x{x:0>8}\n", .{ output, expected });

    try std.testing.expect(output == expected);
}

test "c.addi" {
    const input = 0x05fd;
    const output = zca_decoder(input);
    const expected = 0x01f58593;

    std.debug.print("Input: 0x{x:0>4}\n", .{input});
    std.debug.print("Output: 0x{x:0>8}\nExpected: 0x{x:0>8}\n", .{ output, expected });

    try std.testing.expect(output == expected);
}

test "c.jal" {
    const in_out_pair = struct {
        input: u16,
        expected: u32,
    };

    const pairs: [3]in_out_pair = .{
        .{ .input = 0x3fcd, .expected = 0xfe5ff0ef },
        .{ .input = 0x3fed, .expected = 0xff5ff0ef },
        .{ .input = 0x3fd5, .expected = 0xff5ff0ef },
    };

    for (pairs, 0..) |pair, run| {
        const output = zca_decoder(pair.input);

        std.debug.print("Input: 0x{x:0>4}\n", .{pair.input});
        std.debug.print("Output: 0x{x:0>8}\n", .{output});

        _ = std.testing.expect(output == pair.expected) catch {
            std.debug.print("Run {} failed, got 0x{x:0>8} instead\n", .{ run, pair.expected });
            const out_bits: types.instr = .{ .instruction = pair.expected };
            std.debug.print("recon'ed instr: {b:0>1} {b:0>10} {b:0>1} {b:0>8} {b:0>5} {b:0>7}\n", .{
                @as(u1, @bitCast(out_bits.j_type.imm20)),
                @as(u10, @bitCast(out_bits.j_type.imm10_1)),
                @as(u1, @bitCast(out_bits.j_type.imm11)),
                @as(u8, @bitCast(out_bits.j_type.imm19_12)),
                @as(u5, @bitCast(out_bits.j_type.rd)),
                @as(u7, @bitCast(out_bits.j_type.opcode)),
            });
        };
    }
}
