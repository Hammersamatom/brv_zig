const std = @import("std");
const stdout = std.io.getStdOut().writer();
const rv = @import("cpu.zig");

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
    const mem_max: u32 = (1 << 23);
    const memory = try allocator.alloc(u8, mem_max);
    defer allocator.free(memory);

    // Read the file into the heap buffer
    const readBytes = try fileHandler.read(memory);
    _ = readBytes;

    var core_one: rv.cpu_state = rv.cpu_state{ .pc_reg = 0, .gp_regs = [_]u32{0} ** 32 };

    while (true) {
        try rv.step_cpu(&core_one, memory);
    }
}
