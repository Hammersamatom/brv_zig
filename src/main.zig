const std = @import("std");
const rv = @import("cores/rv32im.zig");
const types = @import("rv_types.zig");
const uart = @import("16550.zig");
const Bus = @import("bus.zig").Bus;

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
        .pc_reg = .{ .u = 0x8000_0000 },
        .gp_regs = [_]types.reg{types.reg{ .u = 0 }} ** 32,
    };

    var cycle_count: f64 = 0;
    var uart_local = uart.uart.init(7372800);
    const bus = Bus(mem_max){ .mem = memory, .uart = &uart_local };

    var last_pc_reg: u32 = undefined;

    while (true) {
        try rv.step_cpu(&core_one, &bus);
        cycle_count += 1;
        try uart_local.transmit();

        // Exit if in a loop, and UART is empty
        if (core_one.pc_reg.u == last_pc_reg and uart_local.lsr.raw == 0x60)
            break;
        // Save last pc_reg for break loop
        last_pc_reg = core_one.pc_reg.u;
    }

    const outFile = try std.fs.cwd().createFile("memory.bin", .{});
    defer outFile.close();

    _ = try outFile.write(memory);
}
