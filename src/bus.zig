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

        pub fn write8(self: Self, address: u32, byte: u8) void {
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000)), byte),
                0x8000_0000...0x8000_0000 + mem_size - 1 => self.mem[address - 0x8000_0000] = byte,
                else => return,
            }
        }

        pub fn write16(self: Self, address: u32, half: u16) void {
            const un: types.component = .{ .dword = half };
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => {
                    if (address <= 0x1000_0000 + 0x8 - 2) {
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)), un.byte[0]);
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)), un.byte[1]);
                    } else {
                        self.write8(address - 0x1000_0000 + 0, un.byte[0]);
                        self.write8(address - 0x1000_0000 + 1, un.byte[1]);
                    }
                },
                0x8000_0000...0x8000_0000 + mem_size - 1 => {
                    if (address <= mem_size - 2) {
                        const raw_ptr: usize = @intFromPtr(self.mem.ptr + (address - 0x8000_0000));
                        const ptr: *u16 = @ptrFromInt(raw_ptr);
                        ptr.* = half;
                    } else {
                        self.write8(address + 0, un.byte[0]);
                        self.write8(address + 1, un.byte[1]);
                    }
                },
                else => return,
            }
        }

        pub fn write32(self: Self, address: u32, word: u32) void {
            const un: types.component = .{ .dword = word };
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => {
                    if (address <= 0x1000_0000 + 0x8 - 4) {
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)), un.byte[0]);
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)), un.byte[1]);
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 2)), un.byte[2]);
                        self.uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 3)), un.byte[3]);
                    } else {
                        self.write16(address - 0x1000_0000 + 0, un.short[0]);
                        self.write16(address - 0x1000_0000 + 2, un.short[1]);
                    }
                },
                0x8000_0000...0x8000_0000 + mem_size - 1 => {
                    if (address <= mem_size - 4) {
                        const raw_ptr: usize = @intFromPtr(self.mem.ptr + (address - 0x8000_0000));
                        const ptr: *u32 = @ptrFromInt(raw_ptr);
                        ptr.* = word;
                    } else {
                        self.write16(address + 0, un.short[0]);
                        self.write16(address + 2, un.short[1]);
                    }
                },
                else => return,
            }
        }

        pub fn read8(self: Self, address: u32) u8 {
            return switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000))),
                0x8000_0000...0x8000_0000 + mem_size - 1 => self.mem[address - 0x8000_0000],
                else => return 0,
            };
        }

        pub fn read16(self: Self, address: u32) u16 {
            var un: types.component = .{ .dword = 0 };
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => {
                    if (address <= 0x1000_0000 + 0x8 - 2) {
                        un.byte[0] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)));
                        un.byte[1] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)));
                    } else {
                        un.byte[0] = self.read8(address - 0x1000_0000 + 0);
                        un.byte[1] = self.read8(address - 0x1000_0000 + 1);
                    }
                },
                0x8000_0000...0x8000_0000 + mem_size - 1 => {
                    if (address <= 0x8000_0000 + mem_size - 2) {
                        const raw_ptr: usize = @intFromPtr(self.mem.ptr + (address - 0x8000_0000));
                        const ptr: *u16 = @ptrFromInt(raw_ptr);
                        un.short[0] = ptr.*;
                    } else {
                        un.byte[0] = self.read8(address + 0);
                        un.byte[1] = self.read8(address + 1);
                    }
                },
                else => return 0,
            }
            return un.short[0];
        }

        pub fn read32(self: Self, address: u32) u32 {
            var un: types.component = .{ .dword = 0 };
            switch (address) {
                0x1000_0000...0x1000_0000 + 0x8 - 1 => {
                    if (address <= 0x1000_0000 + 0x8 - 4) {
                        un.byte[0] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)));
                        un.byte[1] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)));
                        un.byte[2] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 2)));
                        un.byte[3] = self.uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 3)));
                    } else {
                        un.short[0] = self.read16(address - 0x1000_0000 + 0);
                        un.short[1] = self.read16(address - 0x1000_0000 + 2);
                    }
                },
                0x8000_0000...0x8000_0000 + mem_size - 1 => {
                    if (address <= 0x8000_0000 + mem_size - 4) {
                        const raw_ptr: usize = @intFromPtr(self.mem.ptr + (address - 0x8000_0000));
                        const ptr: *u32 = @ptrFromInt(raw_ptr);
                        un.word[0] = ptr.*;
                    } else {
                        un.short[0] = self.read16(address + 0);
                        un.short[1] = self.read16(address + 2);
                    }
                },
                else => return 0,
            }
            return un.word[0];
        }
    };
}
