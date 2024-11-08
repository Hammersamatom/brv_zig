const std = @import("std");
const types = @import("rv_types.zig");
const uart = @import("16550.zig").uart;

pub const Bus = struct {
    m_mem: *const []u8,
    m_uart: *uart,

    //pub fn write8(self: Self, address: u32, byte: u8) void {
    pub fn write8(self: Bus, address: u32, byte: u8) void {
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000)), byte);
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            self.m_mem.*[address - 0x8000_0000] = byte;
        }
    }

    pub fn write16(self: Bus, address: u32, half: u16) void {
        const un: types.component = .{ .dword = half };
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            if (address <= 0x1000_0000 + 0x8 - 2) {
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)), un.byte[0]);
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)), un.byte[1]);
            } else {
                self.write8(address - 0x1000_0000 + 0, un.byte[0]);
                self.write8(address - 0x1000_0000 + 1, un.byte[1]);
            }
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            if (address <= self.m_mem.len - 2) {
                const raw_ptr: usize = @intFromPtr(self.m_mem.ptr + (address - 0x8000_0000));
                const ptr: *u16 = @ptrFromInt(raw_ptr);
                ptr.* = half;
            } else {
                self.write8(address + 0, un.byte[0]);
                self.write8(address + 1, un.byte[1]);
            }
        }
    }

    pub fn write32(self: Bus, address: u32, word: u32) void {
        const un: types.component = .{ .dword = word };
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            if (address <= 0x1000_0000 + 0x8 - 4) {
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)), un.byte[0]);
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)), un.byte[1]);
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 2)), un.byte[2]);
                self.m_uart.*.writeUartReg(@as(u3, @truncate(address - 0x1000_0000 + 3)), un.byte[3]);
            } else {
                self.write16(address - 0x1000_0000 + 0, un.short[0]);
                self.write16(address - 0x1000_0000 + 2, un.short[1]);
            }
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            if (address <= self.m_mem.len - 4) {
                const raw_ptr: usize = @intFromPtr(self.m_mem.ptr + (address - 0x8000_0000));
                const ptr: *u32 = @ptrFromInt(raw_ptr);
                ptr.* = word;
            } else {
                self.write16(address + 0, un.short[0]);
                self.write16(address + 2, un.short[1]);
            }
        }
    }

    pub fn read8(self: Bus, address: u32) u8 {
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            return self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000)));
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            return self.m_mem.*[address - 0x8000_0000];
        }
        return 0;
    }

    pub fn read16(self: Bus, address: u32) u16 {
        var un: types.component = .{ .dword = 0 };
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            if (address <= 0x1000_0000 + 0x8 - 2) {
                un.byte[0] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)));
                un.byte[1] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)));
            } else {
                un.byte[0] = self.read8(address - 0x1000_0000 + 0);
                un.byte[1] = self.read8(address - 0x1000_0000 + 1);
            }
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            if (address <= 0x8000_0000 + self.m_mem.len - 2) {
                const raw_ptr: usize = @intFromPtr(self.m_mem.ptr + (address - 0x8000_0000));
                const ptr: *u16 = @ptrFromInt(raw_ptr);
                un.short[0] = ptr.*;
            } else {
                un.byte[0] = self.read8(address + 0);
                un.byte[1] = self.read8(address + 1);
            }
        }
        return un.short[0];
    }

    pub fn read32(self: Bus, address: u32) u32 {
        var un: types.component = .{ .dword = 0 };
        if (address >= 0x1000_0000 and address < 0x1000_0000 + 0x8) {
            if (address <= 0x1000_0000 + 0x8 - 4) {
                un.byte[0] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 0)));
                un.byte[1] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 1)));
                un.byte[2] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 2)));
                un.byte[3] = self.m_uart.*.readUartReg(@as(u3, @truncate(address - 0x1000_0000 + 3)));
            } else {
                un.short[0] = self.read16(address - 0x1000_0000 + 0);
                un.short[1] = self.read16(address - 0x1000_0000 + 2);
            }
        } else if (address >= 0x8000_0000 and address < 0x8000_0000 + self.m_mem.len) {
            if (address <= 0x8000_0000 + self.m_mem.len - 4) {
                const raw_ptr: usize = @intFromPtr(self.m_mem.ptr + (address - 0x8000_0000));
                const ptr: *u32 = @ptrFromInt(raw_ptr);
                un.word[0] = ptr.*;
            } else {
                un.short[0] = self.read16(address + 0);
                un.short[1] = self.read16(address + 2);
            }
        }
        return un.word[0];
    }
};
