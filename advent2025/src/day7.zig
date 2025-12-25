const std = @import("std");

const Pos = struct { x: usize, y: usize };

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day7/input.txt";

    // Open the file and create a buffered File.Reader.
    // Buffer size is kept small to demostrate delimiter spanning buffer boundaries.
    // In production, use a more sensible buffer size (e.g., 4KB or 8KB).
    var file = try std.fs.cwd().openFile(filename, .{ .mode = .read_only });
    defer file.close();
    var read_buf: [2]u8 = undefined;
    var f_reader: std.fs.File.Reader = file.reader(&read_buf);

    // Pointer to the std.Io.Reader interface to use the generic IO functions.
    // Since it's a problematic practice that has tripped up many people, it's better to
    // directly reference the f_reader.interface in usage than taking a pointer.
    // const reader = &f_reader.interface;

    // An accumulating writer to store data read from the file.
    var line = std.Io.Writer.Allocating.init(alloc);
    defer line.deinit();

    var inputs: std.ArrayList([]u8) = .empty;
    defer inputs.deinit(alloc);

    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        // std.debug.print("{s}\n", .{line.written()});
        const c = try alloc.dupe(u8, line.written());
        try inputs.append(alloc, c);
        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    const rows = inputs.items.len;
    const cols = inputs.items[0].len;
    const start = Pos{ .x = 70, .y = 1 };

    var beams: std.ArrayList(Pos) = .empty;
    defer beams.deinit(alloc);
    try beams.append(alloc, start);
    var currRow: usize = start.y;
    var result1: i128 = 0;

    var inputs1: std.ArrayList([]u8) = .empty;
    defer inputs1.deinit(alloc);

    for (inputs.items) |item| {
        const l = try alloc.dupe(u8, item);
        try inputs1.append(alloc, l);
    }

    while (true) {
        var nextBeams: std.ArrayList(Pos) = .empty;

        for (beams.items) |beam| {
            inputs.items[beam.y][beam.x] = '|';
        }

        // std.debug.print("row {}: {s}\n", .{ currRow, inputs.items[currRow]});

        for (inputs.items[currRow], 0..) |ch, idx| {
            if (ch != '|') continue;

            const split = (inputs.items[currRow + 1][idx] == '^');
            if (!split) {
                try nextBeams.append(alloc, Pos{ .x = idx, .y = currRow + 1 });
            } else {
                if (idx == 0) {
                    try nextBeams.append(alloc, Pos{ .x = idx + 1, .y = currRow + 1 });
                } else if (idx == cols - 1) {
                    try nextBeams.append(alloc, Pos{ .x = idx - 1, .y = currRow + 1 });
                } else {
                    try nextBeams.append(alloc, Pos{ .x = idx - 1, .y = currRow + 1 });
                    try nextBeams.append(alloc, Pos{ .x = idx + 1, .y = currRow + 1 });
                }
                result1 += 1;
            }
        }
        beams.deinit(alloc);
        beams = nextBeams;

        if (currRow == rows - 2)
            break;
        currRow += 1;
    }

    var groups = std.StringHashMap(u128).init(alloc);
    var init = try alloc.dupe(u8, inputs1.items[1]);
    init[start.x] = '|';
    try groups.put(init, 1);
    defer groups.deinit();

    currRow = 1;
    while (currRow < rows - 1) {
        var nextGroups = std.StringHashMap(u128).init(alloc);
        defer nextGroups.deinit();

        var iter = groups.keyIterator();
        while (iter.next()) |key| {
            var left = try alloc.dupe(u8, inputs1.items[currRow + 1]);
            var right = try alloc.dupe(u8, inputs1.items[currRow + 1]);
            // go left
            for (inputs1.items[currRow + 1], 0..) |ch, idx| {
                if (key.*[idx] != '|') continue;

                if (ch != '^') {
                    left[idx] = '|';
                    right[idx] = '|';
                } else {
                    if (idx == 0) {
                        right[idx + 1] = '|';
                    } else if (idx == cols - 1) {
                        left[idx - 1] = '|';
                    } else {
                        left[idx - 1] = '|';
                        right[idx + 1] = '|';
                    }
                }
            }
            // std.debug.print("key  : {s}\n", .{key.*});
            // std.debug.print("left : {s}\n", .{left});
            // std.debug.print("right: {s}\n", .{right});

            const currValue = groups.get(key.*) orelse 0;
            // std.debug.print("currValue {}\n", .{currValue});

            const equals = std.mem.eql(u8, left, right);

            if (nextGroups.getPtr(left)) |value| {
                value.* += currValue;
                alloc.free(left);
                // std.debug.print("left 1 with value {}\n", .{value.*});
            } else {
                try nextGroups.put(left, currValue);
                // std.debug.print("left 2 with value {}\n", .{currValue});
            }

            if (equals) {
                alloc.free(right);
            } else {
                if (nextGroups.getPtr(right)) |value| {
                    value.* += currValue;
                    alloc.free(right);
                    // std.debug.print("right 1 with value {}\n", .{value.*});
                } else {
                    try nextGroups.put(right, currValue);
                    // std.debug.print("right 2 with value {}\n", .{currValue});
                }
            }

            alloc.free(key.*);
        }

        groups.clearRetainingCapacity();
        var iter1 = nextGroups.iterator();

        while (iter1.next()) |entry| {
            // std.debug.print("{s}: {}\n", .{entry.key_ptr.*, entry.value_ptr.*});
            try groups.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        // std.debug.print("row {} next group size {}, counts {}\n", .{ currRow + 1, groups.count(), counts });

        currRow += 1;
    }

    for (0..rows) |row| {
        alloc.free(inputs.items[row]);
        alloc.free(inputs1.items[row]);
    }

    var iter = groups.keyIterator();
    var result2: u128 = 0;

    while (iter.next()) |key| {
        result2 += groups.get(key.*) orelse 0;
        alloc.free(key.*);
    }

    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
