const std = @import("std");
const types = @import("rv_types.zig");
const uart = @import("16550.zig").uart;

//pub const bus = struct {
//    mem_size: u32,
//    mem: []u8,
//
//    pub fn init(comptime mem_s: u32, mem: []u8) bus {
//        return bus{
//            .mem_size = mem_s,
//            .mem = mem,
//        };
//    }
//
//    pub fn writeByte(self: *const bus, address: u32, byte: u8) void {
//        switch (address) {
//            //0x8000_0000...end: {
//            //    break :end 0x8000_0000 + self.mem_size;
//            //} => self.mem[address - 0x8000_0000] = byte,
//            0x8000_0000...0x8000_0000 + self.mem_size => self.mem[address - 0x8000_0000] = byte,
//            else => return,
//        }
//    }
//
//    pub fn readByte(self: *const bus, address: u32) u8 {
//        return switch (address) {
//            0x8000_0000...0x8000_0000 + self.mem_size => self.mem[address - 0x8000_0000],
//            else => return 0,
//        };
//    }
//};

pub fn Bus(comptime mem_size: usize) type {
    return struct {
        const Self = @This();
        mem: []u8,
        uart: *uart,

        pub fn writeByte(self: Self, address: u32, byte: u8) void {
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000)), byte),
                0x8000_0000...0x8000_0000 + mem_size - 1 => self.mem[address - 0x8000_0000] = byte,
                else => return,
            }
        }

        pub fn readByte(self: Self, address: u32) u8 {
            return switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000))),
                0x8000_0000...0x8000_0000 + mem_size - 1 => self.mem[address - 0x8000_0000],
                else => return 0,
            };
        }
    };
}
