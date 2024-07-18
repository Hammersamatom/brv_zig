const std = @import("std");
const stdout = std.io.getStdOut().writer();

pub const uart_settings = struct {
    uart_status_address: u32,
    counted: u8,
    buffer_address: u32,
};

pub fn uart_transmit(properties: *uart_settings, memory: []u8) !void {
    //std.debug.print("uart_status_address = {x}\n", .{properties.*.uart_status_address});
    //std.debug.print("counted = {}\n", .{properties.*.counted});
    //std.debug.print("buffer_address = {x}\n", .{properties.*.buffer_address});

    //std.debug.print("Memory Size = {}\n", .{memory.len});

    const total_bytes: *u8 = &memory[0x7fff00];
    const counted: *u8 = &properties.*.counted;

    if (total_bytes.* == 0)
        return;

    // If we've already iterated over everything, set the memory address pointed to by total_bytes to 0
    //std.debug.print("counted.* = {}\n", .{counted.*});
    //std.debug.print("total_bytes.* = {}\n", .{total_bytes.*});
    try stdout.print("{c}", .{memory[properties.*.buffer_address + counted.*]});
    counted.* += 1;

    if (counted.* == total_bytes.*) {
        total_bytes.* = 0;
        counted.* = 0;
        return;
    }
}
