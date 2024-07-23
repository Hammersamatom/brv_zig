const std = @import("std");
const un = @import("../rv_types.zig");
const reg = un.reg;

pub const cpu_state = struct {
    pc_reg: reg,
    gp_regs: [32]reg,
};

pub fn step_cpu(core_state: *cpu_state, memory: []u8) !void {
    const gp_regs: *[32]reg = &core_state.*.gp_regs;
    const pc_reg: *reg = &core_state.*.pc_reg;

    const inst: un.instr = un.instr{
        // Converts a slice of 4 u8 into a u32
        .instruction = std.mem.readVarInt(u32, memory[pc_reg.*.u .. pc_reg.*.u + 4], std.builtin.Endian.little),
    };

    // Map the references to pointers to avoid a copy
    const rd: *reg = &gp_regs[inst.r_type.rd];
    const rs1: *reg = &gp_regs[inst.r_type.rs1];
    const rs2: *reg = &gp_regs[inst.r_type.rs2];

    var branched: bool = false;

    switch (@as(un.inst_types, @enumFromInt(inst.op_only.opcode))) {
        .REG, .IMM => { // REG, IMM
            const rs2_or_imm: bool = (inst.op_only.opcode & 0x20) == 0x20;
            const value: u32 = switch (rs2_or_imm) {
                true => rs2.*.u,
                false => @as(u32, @bitCast(@as(i32, @intCast(@as(i12, @bitCast(inst.i_type.imm)))))),
            };
            rd.*.u = switch (@as(un.reg_imm_names, @enumFromInt(inst.i_type.funct3))) {
                .ADD_I_SUB => switch (rs2_or_imm) {
                    true => switch (inst.r_type.funct7) {
                        0x00 => rs1.*.u +% value,
                        0x20 => rs1.*.u -% value,
                        else => std.debug.panic("Invalid instruction (REG, IMM) (ADD / SUB): {x}\n", .{inst.r_type.funct7}),
                    },
                    false => rs1.*.u +% value,
                },
                .XOR_I => rs1.*.u ^ value, // XOR/I
                .OR_I => rs1.*.u | value, // OR/I
                .AND_I => rs1.*.u & value, // AND/I
                .SLL_I => rs1.*.u << @intCast(value & 0x1F), // SLL/I
                .SRL_I_SRA_I => switch (inst.r_type.funct7) { // SRL/I / SRA/I
                    0x00 => rs1.*.u >> @truncate(value),
                    0x20 => @as(u32, @bitCast(rs1.*.i >> @truncate(value))),
                    else => std.debug.panic("Invalid instruction (REG, IMM) (SRL/I / SRA/I): {x}\n", .{inst.r_type.funct7}),
                },
                .SLT_I => if (rs1.*.i < @as(i32, @bitCast(value))) 1 else 0, // SLT/I (Set Less Than Immediate)
                .SLT_I_U => if (rs1.*.u < value) 1 else 0, // SLT/I/U (Set Less Than Immediate Unsigned)
            };
        },
        .LOAD => { // LOAD
            // Can't you offset backwards?
            const ls_offset: u32 = @as(u32, @bitCast(rs1.*.i +% inst.i_type.imm));
            var t: un.component = .{ .dword = 0 };
            switch (@as(un.load_names, @enumFromInt(inst.i_type.funct3))) {
                // LB (Load Byte, sign extended)
                .LB => {
                    t.byte[3] = memory[ls_offset + 0];
                    rd.*.i = t.word_s[0] >> 24;
                },
                // LH (Load Half, sign-extended)
                .LH => {
                    t.byte[3] = memory[ls_offset + 1];
                    t.byte[2] = memory[ls_offset + 0];
                    rd.*.i = t.word_s[0] >> 16;
                },
                // LW (Load Word)
                .LW => {
                    t.byte[3] = memory[ls_offset + 3];
                    t.byte[2] = memory[ls_offset + 2];
                    t.byte[1] = memory[ls_offset + 1];
                    t.byte[0] = memory[ls_offset + 0];
                    rd.*.u = t.word[0];
                },
                // LHU (Load Half Unsigned)
                .LHU => {
                    t.byte[1] = memory[ls_offset + 1];
                    t.byte[0] = memory[ls_offset + 0];
                    rd.*.u = t.word[0];
                },
                // LBU (Load Byte Unsigned)
                .LBU => {
                    t.byte[0] = memory[ls_offset + 0];
                    rd.*.u = t.word[0];
                },
            }
        },
        .STORE => { // STORE
            const imm: un.imm_recon_s = un.imm_recon_s{
                .imm_s = .{
                    .imm4_0 = inst.s_type.imm4_0,
                    .imm11_5 = inst.s_type.imm11_5,
                },
            };
            const ls_offset: u32 = @as(u32, @bitCast(rs1.*.i +% imm.word_s));
            const t: un.component = .{ .dword = rs2.*.u };

            switch (@as(un.store_names, @enumFromInt(inst.s_type.funct3))) {
                .SW => { // SW
                    memory[ls_offset + 3] = t.byte[3];
                    memory[ls_offset + 2] = t.byte[2];
                    memory[ls_offset + 1] = t.byte[1];
                    memory[ls_offset + 0] = t.byte[0];
                },
                .SH => { // SH
                    memory[ls_offset + 1] = t.byte[1];
                    memory[ls_offset + 0] = t.byte[0];
                },
                .SB => { // SB
                    memory[ls_offset + 0] = t.byte[0];
                },
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
            branched = switch (@as(un.branch_names, @enumFromInt(inst.b_type.funct3))) {
                .BEQ => rs1.*.u == rs2.*.u,
                .BNE => rs1.*.u != rs2.*.u,
                .BLT => rs1.*.i < rs2.*.i,
                .BGE => rs1.*.i >= rs2.*.i,
                .BLTU => rs1.*.u < rs2.*.u,
                .BGEU => rs1.*.u >= rs2.*.u,
            };
            if (branched)
                pc_reg.*.i +%= imm.word_s;
        },
        .JALR => { // JALR
            const return_ptr: u32 = pc_reg.*.u +% 4;
            branched = true;
            pc_reg.*.i = rs1.*.i +% inst.i_type.imm;
            rd.*.u = return_ptr;
        },
        .JAL => { // JAL
            const imm: un.imm_recon_j = un.imm_recon_j{
                .imm_j = .{
                    .imm20 = inst.j_type.imm20,
                    .imm19_12 = inst.j_type.imm19_12,
                    .imm11 = inst.j_type.imm11,
                    .imm10_1 = inst.j_type.imm10_1,
                    .unused_1 = 0,
                },
            };

            rd.*.u = pc_reg.*.u +% 4;
            branched = true;
            pc_reg.*.i +%= imm.word_s;
        },
        .LUI => rd.*.u = (@as(u32, @intCast(inst.u_type.imm31_12)) << 12), // LUI
        .AUIPC => rd.*.u = (@as(u32, @intCast(inst.u_type.imm31_12)) << 12) +% pc_reg.*.u, //AUIPC
        .SYSTEM => { // SYSTEM
            switch (inst.i_type.imm) {
                0x0 => return, // ECALL
                0x1 => return, // EBREAK
                else => std.debug.panic("Invalid instruction: {x}\n", .{inst.instruction}),
            }
        },
    }

    if (!branched)
        pc_reg.*.u +%= 4;

    gp_regs.*[0].u = 0;
}
