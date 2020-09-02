
const std = @import("std");
const isSpace = std.ascii.isSpace;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

pub fn main() !void {
    const file = try std.fs.cwd().openFile(
        "readerTest.zig",
        .{ .read = true },
    );
    defer file.close();

    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    
    try file.reader().readAllArrayList(&list, 16 * 1024);

    var count : u16 = 0;
    var prev_is_space : bool = true;
    
    for (list.items) |c| {
        if (prev_is_space and isSpace(c) != true) {
            count += 1;
        }
        prev_is_space = isSpace(c);
    }

    std.debug.print("WC: {}\n", .{count});
}

