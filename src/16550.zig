// Specifications
// https://caro.su/msx/ocm_de1/16550.pdf
const std = @import("std");
const time = std.time;
const stdprint = std.io.getStdOut().writer();

const rhr_register = extern union {
    raw: u8,
    interp: packed struct {
        character_received: u8,
    },
};
const thr_register = extern union {
    raw: u8,
    interp: packed struct {
        character_to_be_transmitted: u8,
    },
};
const ier_register = extern union {
    raw: u8,
    interp: packed struct {
        data_ready: u1,
        thr_empty: u1,
        receiver_line_status: u1,
        modem_status: u1,
        unused: u2,
        dma_rx_end: u1,
        dma_tx_end: u1,
    },
};
const isr_register = extern union {
    raw: u8,
    interp: packed struct {
        interrupt_status: u1,
        interrupt_identification_code: u3,
        dma_rx_end: u1,
        dma_tx_end: u1,
        fifos_enabled_1: u1,
        fifos_enabled_2: u1,
    },
};
const fcr_register = extern union {
    raw: u8,
    interp: packed struct {
        fifo_enabled: u1,
        rx_fifo_reset: u1,
        tx_fifo_reset: u1,
        dma_mode: u1,
        enable_dma_end: u1,
        unused: u1,
        receivers_fifo_trigger_level: u2,
    },
};
const lcr_register = extern union {
    raw: u8,
    interp: packed struct {
        word_length: u2,
        stop_bits: u1,
        parity_enable: u1,
        even_parity: u1,
        force_parity: u1,
        set_break: u1,
        dlab: u1,
    },
};
const mcr_register = extern union {
    raw: u8,
    interp: packed struct {
        dtr: u1,
        rts: u1,
        out_1: u1,
        out_2_int_enable: u1,
        loop_back: u1,
        unused: u3,
    },
};
const lsr_register = extern union {
    raw: u8,
    interp: packed struct {
        data_ready: u1,
        overrun_error: u1,
        parity_error: u1,
        framing_error: u1,
        break_interrupt: u1,
        thr_empty: u1,
        transmitter_empty: u1,
        fifo_data_error: u1,
    },
};
const msr_register = extern union {
    raw: u8,
    interp: packed struct {
        delta_cts: u1,
        delta_dsr: u1,
        trailing_edge_ri: u1,
        delta_cd: u1,
        cts: u1,
        dsr: u1,
        ri: u1,
        cd: u1,
    },
};
const spr_register = extern union {
    raw: u8,
    interp: packed struct {
        user_data: u8,
    },
};
const dll_register = extern union {
    raw: u8,
    interp: packed struct {
        baudrate_divisors_constant_lsb: u8,
    },
};
const dlm_register = extern union {
    raw: u8,
    interp: packed struct {
        baudrate_divisors_constant_msb: u8,
    },
};
const psd_register = extern union {
    raw: u8,
    interp: packed struct {
        prescalers_divison_factor: u4,
        unused: u4,
    },
};

pub const uart = struct {
    f_clk: i128,
    f_baud: f64,
    f_time_delta: i128,
    f_time_last: i128,

    // 0b000, DLAB=0
    rhr: rhr_register, // R
    thr: thr_register, // W
    // 0b000, DLAB=1
    dll: dll_register, // R/W

    // 0b001, DLAB=0
    ier: ier_register, // R/W
    // 0b001, DLAB=1
    dlm: dlm_register, // R/W

    // 0b010, DLAB=X
    isr: isr_register, // R
    fcr: fcr_register, // W

    // 0b011, DLAB=X
    lcr: lcr_register, // R/W

    // 0b100, DLAB=X
    mcr: mcr_register, // R/W

    // 0b101, DLAB=0
    lsr: lsr_register, // R
    // 0b101, DLAB=1
    psd: psd_register, // W

    // 0b110, DLAB=0
    msr: msr_register, // R

    // 0b111, DLAB=X
    spr: spr_register, // R/W

    pub fn init(f_clk_in: i128) uart {
        var temp = uart{
            .f_clk = f_clk_in,
            .f_baud = 0.0,
            .f_time_delta = 0.0,
            .f_time_last = time.nanoTimestamp(),
            .rhr = .{ .raw = 0x00 },
            .thr = .{ .raw = 0x00 },
            .dll = .{ .raw = 0x03 },
            .dlm = .{ .raw = 0x00 },
            .ier = .{ .raw = 0x00 },
            .isr = .{ .raw = 0x01 },
            .fcr = .{ .raw = 0x00 },
            .lcr = .{ .raw = 0x03 },
            .mcr = .{ .raw = 0x00 },
            .lsr = .{ .raw = 0x60 },
            .psd = .{ .raw = 0x00 },
            .msr = .{ .raw = 0x00 },
            .spr = .{ .raw = 0x00 },
        };
        temp.computeBaud();
        return temp;
    }

    fn reset(self: *uart) void {
        self.*.f_baud = 0.0;
        self.*.f_time_delta = 0.0;
        self.*.f_time_last = time.nanoTimestamp();
        self.*.rhr.raw = 0x00;
        self.*.thr.raw = 0x00;
        self.*.dll.raw = 0x03;
        self.*.dlm.raw = 0x00;
        self.*.ier.raw = 0x00;
        self.*.isr.raw = 0x01;
        self.*.fcr.raw = 0x00;
        self.*.lcr.raw = 0x03;
        self.*.mcr.raw = 0x00;
        self.*.lsr.raw = 0x60;
        self.*.psd.raw = 0x00;
        self.*.msr.raw = 0x00;
        self.*.spr.raw = 0x00;
    }

    fn computeBaud(self: *uart) void {
        const compounded = extern union {
            parts: packed struct {
                dll: u8,
                dlm: u8,
            },
            whole_num: u32,
        };

        const dl: compounded = .{
            .parts = .{
                .dll = self.*.dll.raw,
                .dlm = self.*.dlm.raw,
            },
        };
        const baudrate = @as(
            f64,
            @floatFromInt(@divTrunc(
                self.*.f_clk,
                (16 * (self.*.psd.raw + 1) * (dl.whole_num + 1)),
            )),
        );

        // Store the baudrate for posterity (I don't think we'll ever use this directly except for debugging information)
        self.*.f_baud = baudrate;

        // Number of bits per word, adjustable by the first two least significant bits of the lcr register
        const word_length_array: [4]f32 = .{ 5.0, 6.0, 7.0, 8.0 };

        // 1 billion nanoseconds in one second, divided by the baudrate to get the time for a single bit.
        // Multiply that by the number of bits per word (since we can only output one u8 at a time instead of individual bits)
        // and you get the difference in time between transmissions.
        const time_delta = @as(i128, @intFromFloat(time.ns_per_s / baudrate * word_length_array[self.*.lcr.interp.word_length]));
        self.*.f_time_delta = time_delta;
    }

    pub fn printUartRegs(self: *uart) void {
        std.debug.print(
            \\f_baud: {d:.1}
            \\f_clk: {}
            \\f_time_last: {}
            \\f_time_delta: {}
            \\Reg_0 (0b000) - THR   (W): {x}, RHR (R): {x}
            \\Reg_1 (0b001) - IER (R/W): {x}
            \\Reg_2 (0b010) - FCR   (W): {x}, ISR (R): {x}
            \\Reg_3 (0b011) - LCR (R/W): {x}
            \\Reg_4 (0b100) - MCR (R/W): {x}
            \\Reg_5 (0b101) - LSR   (R): {x}
            \\Reg_6 (0b110) - MSR   (R): {x}
            \\Reg_7 (0b111) - SPR (R/W): {x}
            \\
        , .{
            self.*.f_baud,
            self.*.f_clk,
            self.*.f_time_last,
            self.*.f_time_delta,
            self.*.thr.raw,
            self.*.rhr.raw,
            self.*.ier.raw,
            self.*.fcr.raw,
            self.*.isr.raw,
            self.*.lcr.raw,
            self.*.mcr.raw,
            self.*.lsr.raw,
            self.*.msr.raw,
            self.*.spr.raw,
        });
    }

    pub fn writeUartReg(self: *uart, reg_select: u3, input: u8) void {
        switch (reg_select) {
            0b000 => switch (self.*.lcr.interp.dlab) {
                0 => {
                    self.*.thr.raw = input;
                    self.*.lsr.interp.thr_empty = 0;
                    self.*.lsr.interp.transmitter_empty = 0;
                },
                1 => {
                    self.*.dll.raw = input;
                    self.*.computeBaud();
                },
            },
            0b001 => switch (self.*.lcr.interp.dlab) {
                0 => self.*.ier.raw = input,
                1 => {
                    self.*.dlm.raw = input;
                    self.*.computeBaud();
                },
            },
            0b010 => self.*.fcr.raw = input,
            0b011 => self.*.lcr.raw = input,
            0b100 => self.*.mcr.raw = input,
            0b101 => switch (self.*.lcr.interp.dlab) {
                0 => return,
                1 => {
                    self.*.psd.raw = input;
                    self.*.computeBaud();
                },
            },
            0b110 => return,
            0b111 => self.*.spr.raw = input,
        }
    }

    pub fn readUartReg(self: *uart, reg_select: u3) u8 {
        return switch (reg_select) {
            0b000 => switch (self.*.lcr.interp.dlab) {
                0 => enc: {
                    self.*.lsr.interp.data_ready = 0;
                    break :enc self.*.rhr.raw;
                },
                1 => self.*.dll.raw,
            },
            0b001 => switch (self.*.lcr.interp.dlab) {
                0 => self.*.ier.raw,
                1 => self.*.dlm.raw,
            },
            0b010 => self.*.isr.raw,
            0b011 => self.*.lcr.raw,
            0b100 => self.*.mcr.raw,
            0b101 => self.*.lsr.raw,
            0b110 => self.*.msr.raw,
            0b111 => self.*.spr.raw,
        };
    }

    pub fn transmit(self: *uart) !void {
        const current_time = time.nanoTimestamp();
        // Only transmit if current time is greater than or equal to time_last + time_delta
        if (current_time < self.*.f_time_last + self.*.f_time_delta) return;
        // Hold register is empt*.y, return early
        if (self.*.lsr.interp.thr_empty == 1 and self.*.lsr.interp.transmitter_empty == 1) return;
        // Push contents of THR register onto stdout
        try stdprint.print("{c}", .{self.*.thr.raw});
        // Clear the THR register
        self.*.thr.raw = 0;
        // Set the empty bits in LSR register for transmission
        self.*.lsr.interp.transmitter_empty = 1;
        self.*.lsr.interp.thr_empty = 1;

        // Reset time_last and begin cycle anew
        self.*.f_time_last = current_time;
    }
};

test "instantiate" {
    const u = uart.init(7372800);
    std.debug.print("Calc'ed Baudrate: {d:.2}\n", .{u.f_baud});
    std.debug.print("Time Delta: {}\nLast Time: {}\n", .{ u.f_time_delta, u.f_time_last });
    try std.testing.expect(u.f_baud == u.f_baud);
}
