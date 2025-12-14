const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day1/input.txt";
    const delimiter = "\n";

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

    var curr: i32 = 50;
    var count1: i32 = 0;
    var count2: i32 = 0;

    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, delimiter[0]) catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        std.debug.print("{s}\n", .{line.written()});

        const buffer = line.written();
        var step = try std.fmt.parseInt(i32, buffer[1..], 10);
        var next: i32 = 0;

        if (buffer[0] == 'L') {
            step = -step;
            next = @mod(curr + step, 100);
            count2 += -@divFloor(curr + step - 1, 100);
            if (curr == 0)
                count2 -= 1;
        } else {
            next = @mod(curr + step, 100);
            count2 += @divFloor(curr + step, 100);
        }

        if (next == 0)
            count1 += 1;

        // std.debug.print("curr {}, step {}, next {}, count1 {}, count2 {}\n", .{ curr, step, next, count1, count2});
        curr = next;

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    std.debug.print("result1: {}, result2: {}\n", .{ count1, count2 });
    std.debug.print("End of stream reached\n", .{});
}
