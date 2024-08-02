const op_only_str = packed struct {
    opcode: u7,
    unused: u25,
};
const r_type_str = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    funct7: u7,
};
const i_type_str = packed struct {
    opcode: u7,
    rd: u5,
    funct3: u3,
    rs1: u5,
    imm: i12,
};
const s_type_str = packed struct {
    opcode: u7,
    imm4_0: i5,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm11_5: i7,
};
const b_type_str = packed struct {
    opcode: u7,
    imm11: i1,
    imm4_1: i4,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm10_5: i6,
    imm12: i1,
};
const u_type_str = packed struct {
    opcode: u7,
    rd: u5,
    imm31_12: i20,
};
const j_type_str = packed struct {
    opcode: u7,
    rd: u5,
    imm19_12: i8,
    imm11: i1,
    imm10_1: i10,
    imm20: i1,
};
pub const instr = extern union {
    instruction: u32,
    instr_shorts: [2]u16,
    instr_bytes: [4]u8,
    op_only: op_only_str,
    r_type: r_type_str,
    i_type: i_type_str,
    s_type: s_type_str,
    b_type: b_type_str,
    u_type: u_type_str,
    j_type: j_type_str,
};

const imm_s_str = packed struct {
    imm4_0: i5,
    imm11_5: i7,
};
const imm_b_str = packed struct {
    unused_1: u1,
    imm4_1: i4,
    imm10_5: i6,
    imm11: i1,
    imm12: i1,
};
const imm_j_str = packed struct {
    unused_1: u1,
    imm10_1: i10,
    imm11: i1,
    imm19_12: i8,
    imm20: i1,
};
pub const imm_recon_s = packed union {
    word_s: i12,
    imm_s: imm_s_str,
};
pub const imm_recon_b = packed union {
    word_s: i13,
    imm_b: imm_b_str,
};
pub const imm_recon_j = packed union {
    word_s: i21,
    imm_j: imm_j_str,
};

pub const reg = extern union {
    u: u32,
    i: i32,
};

pub const abi: [32][]const u8 = [_][]const u8{
    "zero",
    "ra",
    "sp",
    "gp",
    "tp",
    "t0",
    "t1",
    "t2",
    "s0/fp",
    "s1",
    "a0",
    "a1",
    "a2",
    "a3",
    "a4",
    "a5",
    "a6",
    "a7",
    "s2",
    "s3",
    "s4",
    "s5",
    "s6",
    "s7",
    "s8",
    "s9",
    "s10",
    "s11",
    "t3",
    "t4",
    "t5",
    "t6",
};

pub const component = extern union {
    dword: u64,
    dword_s: i64,
    word: [2]u32,
    word_s: [2]i32,
    short: [4]u16,
    short_s: [4]i16,
    byte: [8]u8,
    byte_s: [8]i8,
};

pub const inst_types = enum(u7) {
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
pub const reg_imm_names = enum(u3) {
    ADD_I_SUB = 0x0,
    SLL_I = 0x1,
    SLT_I = 0x2,
    SLT_I_U = 0x3,
    XOR_I = 0x4,
    SRL_I_SRA_I = 0x5,
    OR_I = 0x6,
    AND_I = 0x7,
};
pub const load_names = enum(u3) {
    LB = 0x0,
    LH = 0x1,
    LW = 0x2,
    LBU = 0x4,
    LHU = 0x5,
};
pub const store_names = enum(u3) {
    SB = 0x0,
    SH = 0x1,
    SW = 0x2,
};
pub const branch_names = enum(u3) {
    BEQ = 0x0,
    BNE = 0x1,
    BLT = 0x4,
    BGE = 0x5,
    BLTU = 0x6,
    BGEU = 0x7,
};
pub const mul_names = enum(u3) {
    MUL = 0x0,
    MULH = 0x1,
    MULHSU = 0x2,
    MULHU = 0x3,
    DIV = 0x4,
    DIVU = 0x5,
    REM = 0x6,
    REMU = 0x7,
};
