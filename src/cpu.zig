const std = @import("std");
const un = @import("rv_types.zig");

const component = union {
    word: u32,
    word_s: i32,
    short: [2]u16,
    short_s: [2]i16,
    byte: [4]u8,
    byte_s: [4]i8,
};

pub const cpu_state = struct {
    pc_reg: u32,
    gp_regs: [32]u32,
};

const inst_types = enum(u7) {
    REG = 0b0110011,
    IMM = 0b0010011,
    LOAD = 0b0000011,
    STORE = 0b0100011,
    BRANCH = 0b1100011,
    // Specialized
    JAL = 0b1101111,
    JALR = 0b1100111,
    LUI = 0b0110111,
    AUIPC = 0b0010111,
    SYSTEM = 0b1110011,
};

pub fn step_cpu(core_state: *cpu_state, memory: *const []u8) !void {
    const gp_regs: *[32]u32 = &core_state.*.gp_regs;
    const pc_reg: *u32 = &core_state.*.pc_reg;

    const inst: un.instr = un.instr{
        // Converts a slice of 4 u8 into a u32
        .instruction = std.mem.readVarInt(u32, memory.*[pc_reg.* .. pc_reg.* + 4], std.builtin.Endian.little),
    };

    // Map the references to pointers to avoid a copy
    const rd: *u32 = &gp_regs[inst.r_type.rd];
    const rs1: *u32 = &gp_regs[inst.r_type.rs1];
    const rs2: *u32 = &gp_regs[inst.r_type.rs2];

    var branched: bool = false;

    switch (@as(inst_types, @enumFromInt(inst.op_only.opcode))) {
        .REG, .IMM => { // REG, IMM
            const rs2_or_imm: bool = (inst.op_only.opcode & 0x20) == 0x20;
            const value = if (rs2_or_imm)
                rs2.*
            else
                @as(u32, @bitCast(@as(i32, @intCast(@as(i12, @bitCast(inst.i_type.imm))))));
            rd.* = switch (inst.i_type.funct3) {
                0x0 => switch (rs2_or_imm) {
                    true => switch (inst.r_type.funct7) {
                        0x00 => rs1.* + value,
                        0x20 => rs1.* - value,
                        else => std.debug.panic("Invalid instruction (REG, IMM) (ADD / SUB): {x}\n", .{inst.r_type.funct7}),
                    },
                    false => rs1.* + value,
                },
                0x4 => rs1.* ^ value, // XOR/I
                0x6 => rs1.* | value, // OR/I
                0x7 => rs1.* & value, // AND/I
                0x1 => rs1.* << @intCast(value & 0x1F), // SLL/I
                0x5 => switch (inst.r_type.funct7) { // SRL/I / SRA/I
                    0x00 => rs1.* >> @truncate(value),
                    0x20 => @as(u32, @bitCast(@as(i32, @bitCast(rs1.*)) >> @truncate(value))),
                    else => std.debug.panic("Invalid instruction (REG, IMM) (SRL/I / SRA/I): {x}\n", .{inst.r_type.funct7}),
                },
                0x2 => if (@as(i32, @bitCast(rs1.*)) < @as(i32, @bitCast(value))) 1 else 0, // SLTI (Set Less Than Immediate)
                0x3 => if (rs1.* < value) 1 else 0, // SLTIU (Set Less Than Immediate Unsigned)
            };
        },
        .LOAD => { // LOAD
            // Can't you offset backwards?
            const ls_offset: u32 = rs1.* + @as(u32, @bitCast(@as(i32, @bitCast(inst.instruction)) >> 20));
            var t: component = .{ .word = 0 };
            switch (inst.i_type.funct3) {
                // LB (Load Byte, sign extended)
                0x0 => {
                    t.byte[3] = memory.*[ls_offset + 0];
                    rd.* = @as(u32, @bitCast(t.word_s >> 24));
                },
                // LH (Load Half, sign-extended)
                0x1 => {
                    t.byte[3] = memory.*[ls_offset + 1];
                    t.byte[2] = memory.*[ls_offset + 0];
                    rd.* = @as(u32, @bitCast(t.word_s >> 16));
                },
                // LW (Load Word)
                0x2 => {
                    t.byte[3] = memory.*[ls_offset + 3];
                    t.byte[2] = memory.*[ls_offset + 2];
                    t.byte[1] = memory.*[ls_offset + 1];
                    t.byte[0] = memory.*[ls_offset + 0];
                    rd.* = @as(u32, @bitCast(t.word));
                },
                // LHU (Load Half Unsigned)
                0x5 => {
                    t.byte[1] = memory.*[ls_offset + 1];
                    t.byte[0] = memory.*[ls_offset + 0];
                    rd.* = @as(u32, @bitCast(t.word));
                },
                // LBU (Load Byte Unsigned)
                0x4 => {
                    t.byte[0] = memory.*[ls_offset + 0];
                    rd.* = @as(u32, @bitCast(t.word));
                },
                else => std.debug.panic("Invalid instruction: {x}\n", .{inst.instruction}),
            }
        },
        .STORE => { // STORE
            const imm: un.imm_recon_s = un.imm_recon_s{
                .imm_s = .{
                    .imm4_0 = inst.s_type.imm4_0,
                    .imm11_5 = inst.s_type.imm11_5,
                },
            };
            const ls_offset: u32 = rs1.* + @as(u32, @bitCast(@as(i32, @intCast(imm.word_s))));
            const t: component = .{ .word = rs2.* };

            switch (inst.s_type.funct3) {
                0x2 => { // SW
                    memory.*[ls_offset + 3] = t.byte[3];
                    memory.*[ls_offset + 2] = t.byte[2];
                    memory.*[ls_offset + 1] = t.byte[1];
                    memory.*[ls_offset + 0] = t.byte[0];
                },
                0x1 => { // SH
                    memory.*[ls_offset + 1] = t.byte[1];
                    memory.*[ls_offset + 0] = t.byte[0];
                },
                0x0 => { // SB
                    memory.*[ls_offset + 0] = t.byte[0];
                },
                else => std.debug.panic("Invalid instruction: {x}\n", .{inst.instruction}),
            }
        },
        .BRANCH => { // BRANCH
            const imm: un.imm_recon_b = un.imm_recon_b{
                .imm_b = .{
                    .imm12 = inst.b_type.imm12,
                    .imm11 = inst.b_type.imm11,
                    .imm10_5 = inst.b_type.imm10_5,
                    .imm4_1 = inst.b_type.imm4_1,
                    .unused_1 = 0,
                },
            };
            switch (inst.b_type.funct3) {
                0x0 => {
                    if (rs1.* == rs2.*) branched = true;
                },
                0x1 => {
                    if (rs1.* != rs2.*) branched = true;
                },
                0x4 => {
                    if (@as(i32, @bitCast(rs1.*)) < @as(i32, @bitCast(rs2.*))) branched = true;
                },
                0x5 => {
                    if (@as(i32, @bitCast(rs1.*)) >= @as(i32, @bitCast(rs2.*))) branched = true;
                },
                0x6 => {
                    if (rs1.* < rs2.*) branched = true;
                },
                0x7 => {
                    if (rs1.* >= rs2.*) branched = true;
                },
                else => std.debug.panic("Invalid instruction: {x}\n", .{inst.instruction}),
            }
            if (branched)
                pc_reg.* += @as(u32, @bitCast(@as(i32, @intCast(imm.word_s))));
        },
        .JAL, .JALR => { // JAL / JALR
            const jal_or_jalr: bool = (inst.op_only.opcode & 0b0001000) == 0b0001000;

            const imm: un.imm_recon_j = un.imm_recon_j{
                .imm_j = .{
                    .imm20 = inst.j_type.imm20,
                    .imm19_12 = inst.j_type.imm19_12,
                    .imm11 = inst.j_type.imm11,
                    .imm10_1 = inst.j_type.imm10_1,
                    .unused_1 = 0,
                },
            };

            rd.* = pc_reg.* + 4;
            branched = true;
            pc_reg.* = rs1.* + if (jal_or_jalr == true)
                @as(u32, @bitCast(@as(i32, @intCast(imm.word_s))))
            else
                @as(u32, @bitCast(@as(i32, @intCast(@as(i12, @bitCast(inst.i_type.imm))))));
        },
        .LUI => rd.* = (@as(u32, @intCast(inst.u_type.imm31_12)) << 12), // LUI
        .AUIPC => rd.* = (@as(u32, @intCast(inst.u_type.imm31_12)) << 12) + pc_reg.*, //AUIPC
        .SYSTEM => { // SYSTEM
            switch (inst.i_type.imm) {
                0x0 => return, // ECALL
                0x1 => {
                    for (gp_regs.*, 0..) |reg, index| {
                        std.debug.print("gp_reg[{}] = {}, {}\n", .{ index, reg, @as(i32, @bitCast(reg)) });
                    }
                    std.debug.print("pc_reg = {}\n", .{pc_reg.*});
                    std.debug.print("Exiting Simulation", .{});
                    std.process.exit(0);
                }, // EBREAK
                else => std.debug.panic("Invalid instruction: {x}\n", .{inst.instruction}),
            }
        },
    }

    if (!branched)
        pc_reg.* += 4;

    gp_regs.*[0] = 0;
}
