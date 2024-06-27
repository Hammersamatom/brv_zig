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
    imm: u12,
};
const s_type_str = packed struct {
    opcode: u7,
    imm4_0: u5,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm11_5: u7,
};
const b_type_str = packed struct {
    opcode: u7,
    imm11: u1,
    imm4_1: u4,
    funct3: u3,
    rs1: u5,
    rs2: u5,
    imm10_5: u6,
    imm12: u1,
};
const u_type_str = packed struct {
    opcode: u7,
    rd: u5,
    imm31_12: u20,
};
const j_type_str = packed struct {
    opcode: u7,
    rd: u5,
    imm19_12: u8,
    imm11: u1,
    imm10_1: u10,
    imm20: u1,
};
pub const instr = extern union {
    instruction: u32,
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
    imm4_0: u5,
    imm11_5: u7,
};
const imm_b_str = packed struct {
    unused_1: u1,
    imm4_1: u4,
    imm10_5: u6,
    imm11: u1,
    imm12: u1,
};
const imm_j_str = packed struct {
    unused_1: u1,
    imm10_1: u10,
    imm11: u1,
    imm19_12: u8,
    imm20: u1,
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

pub const reg_un = extern union {
    word: u32,
    word_s: i32,
};
