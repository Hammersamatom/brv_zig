const std = @import("std");
const stdout = std.io.getStdOut().writer();
const rv = @import("cpu.zig");
const types = @import("rv_types.zig");
const uart = @import("crappy_uart.zig");

pub fn main() !void {
    // Set up the allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    // Allocate space on the heap for storing arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Get a file handle
    const fileHandler = try std.fs.cwd().openFile(args[1], .{});
    defer fileHandler.close();

    // Allocate the memory on the heap. Stack isn't large enough (1 << 23) - (1 << 14).
    const mem_max: u32 = (1 << 23) - 0;
    const memory = try allocator.alloc(u8, mem_max);
    @memset(memory, 0);
    defer allocator.free(memory);

    // Read the file into the heap buffer
    const readBytes = try fileHandler.read(memory);
    _ = readBytes;

    var core_one: rv.cpu_state = .{ //rv.cpu_state{
        .pc_reg = 0,
        .gp_regs = [_]u32{0} ** 32,
        .halted = false,
    };
    var uart_prop: uart.uart_settings = .{ //uart.uart_settings{
        .uart_status_address = 0x7f_ff00,
        .buffer_address = 0x7f_ff04,
        .counted = 0,
    };

    while (true) {
        if (core_one.halted == false) {
            //std.debug.print("\npc_reg: {x:>10}", .{core_one.pc_reg});
            //for (core_one.gp_regs, 0..) |reg, i| {
            //    if (i % 2 == 0) std.debug.print("\n", .{});
            //    std.debug.print("{s:<6}: {:<10} 0x{x:<10}", .{ types.abi[i], reg, reg });
            //}
            //std.debug.print("\n", .{});
            try rv.step_cpu(&core_one, memory);
        }
        try uart.uart_transmit(&uart_prop, memory);
        //_ = try std.io.getStdIn().reader().readByte();
        if (core_one.halted == true and memory[uart_prop.uart_status_address] == 0)
            break;
    }

    const outFile = try std.fs.cwd().createFile("memory.bin", .{});
    defer outFile.close();

    _ = try outFile.write(memory);
}
