const std = @import("std");
const rv = @import("cores/rv32i.zig");
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
        .pc_reg = .{ .word = 0 },
        .gp_regs = [_]types.reg_un{types.reg_un{ .word = 0 }} ** 32,
    };
    var uart_prop: uart.uart_settings = .{ //uart.uart_settings{
        .uart_status_address = 0x7f_ff00,
        .buffer_address = 0x7f_ff04,
        .counted = 0,
    };

    var cycle_count: f64 = 0;

    var last_pc_reg: u32 = undefined;

    while (true) {
        try rv.step_cpu(&core_one, memory);
        cycle_count += 1;
        try uart.uart_transmit(&uart_prop, memory);

        // Exit if in a loop, and UART is empty
        if (core_one.pc_reg.word == last_pc_reg and memory[uart_prop.uart_status_address] == 0)
            break;
        // Save last pc_reg for break loop
        last_pc_reg = core_one.pc_reg.word;
    }

    const outFile = try std.fs.cwd().createFile("memory.bin", .{});
    defer outFile.close();

    _ = try outFile.write(memory);
}
