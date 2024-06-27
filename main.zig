const std = @import("std");
const stdout = std.io.getStdOut().writer();
const un = @import("rv_types.zig");

const component = union {
    word: u32,
    word_s: i32,
    short: [2]u16,
    short_s: [2]i16,
    byte: [4]u8,
    byte_s: [4]i8,
};

const cpu_state = struct {
    pc_reg: u32,
    gp_regs: [32]u32,
};

fn step_cpu(core_state: *cpu_state, memory: *const []u8) !void {
    var inst: un.instr = undefined;
    inst.instr_bytes[0] = memory.*[core_state.*.pc_reg + 0];
    inst.instr_bytes[1] = memory.*[core_state.*.pc_reg + 1];
    inst.instr_bytes[2] = memory.*[core_state.*.pc_reg + 2];
    inst.instr_bytes[3] = memory.*[core_state.*.pc_reg + 3];

    // Map the references to pointers to avoid a copy
    const rd: *u32 = &core_state.*.gp_regs[inst.r_type.rd];
    const rs1: *u32 = &core_state.*.gp_regs[inst.r_type.rs1];
    const rs2: *u32 = &core_state.*.gp_regs[inst.r_type.rs2];

    const jumped: bool = false;

    switch (inst.op_only.opcode) {
        // REG, IMM
        0b0110011, 0b0010011 => {
            const rs2_or_imm: bool = if ((inst.op_only.opcode & 0x20) == 0x20) true else false;
            const value = if (rs2_or_imm) rs2.* else @as(u32, @bitCast(@as(i32, @bitCast(inst.instruction)) >> 20));
            switch (inst.r_type.funct3) {
                0x0 => switch (inst.r_type.funct7) { // ADD/I / SUB
                    0x00 => rd.* = rs1.* + value,
                    0x20 => rd.* = rs1.* - value,
                    else => return,
                },
                0x4 => rd.* = rs1.* ^ value, // XOR/I
                0x6 => rd.* = rs1.* | value, // OR/I
                0x7 => rd.* = rs1.* & value, // AND/I
                0x1 => rd.* = rs1.* << @intCast(value & 0x1F), // SLL/I
                0x5 => switch (inst.r_type.funct7) { // SRL/I / SRA/I
                    0x00 => rd.* = rs1.* >> @truncate(value),
                    0x20 => rd.* = @as(u32, @bitCast(@as(i32, @bitCast(value)) >> @truncate(rs2.*))),
                    else => return,
                },
                0x2 => rd.* = if (@as(i32, @bitCast(rs1.*)) < @as(i32, @bitCast(value))) 1 else 0, // SLTI (Set Less Than Immediate)
                0x3 => rd.* = if (rs1.* < value) 1 else 0, // SLTIU (Set Less Than Immediate Unsigned)
            }
        },
        // LOAD
        0b0000011 => {
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
                else => return,
            }
        },
        0b0100011 => {
            var imm: un.imm_recon_s = undefined;
            imm.imm_s = .{ .imm4_0 = inst.s_type.imm4_0, .imm11_5 = inst.s_type.imm11_5 };
            const ls_offset: u32 = rs1.* + @as(u12, @bitCast(imm.word_s));
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
                else => return,
            }
        },
        else => return,
    }

    if (!jumped)
        core_state.*.pc_reg += 4;

    core_state.*.gp_regs[0] = 0;
}

pub fn main() !void {
    // Set up the allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Allocate space on the heap for storing arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Get a file handle
    const fileHandler = try std.fs.cwd().openFile(
        args[1],
        .{},
    );
    defer fileHandler.close();

    // Allocate the memory on the heap. Stack isn't large enough (1 << 23) - (1 << 14).
    const mem_max: u32 = (1 << 23);
    const memory = try allocator.alloc(u8, mem_max);
    defer allocator.free(memory);

    // Read the file into the heap buffer
    const readBytes = try fileHandler.read(memory);
    _ = readBytes;

    //var gp_regs: [32]un.reg_un = undefined;
    var core_one: cpu_state = cpu_state{ .pc_reg = 0, .gp_regs = [_]u32{0} ** 32 };

    while (true) {
        try step_cpu(&core_one, &memory);
    }
}
