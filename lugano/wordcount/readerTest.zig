
const std = @import("std");
const expect = std.testing.expect;
const eql = std.mem.eql;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

test "io reader usage" {
    const file = try std.fs.cwd().createFile(
        "junk_file2.txt",
        .{ .read = true },
    );
    defer file.close();

    _ = try file.writeAll("Hello File!");
    try file.seekTo(0);

    var list = ArrayList(u8).init(test_allocator);
    defer list.deinit();
    try file.reader().readAllArrayList(&list, 16 * 1024);

    expect(eql(u8, list.items, "Hello File!"));
}


