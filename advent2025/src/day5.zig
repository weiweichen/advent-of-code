const std = @import("std");

const Range = struct { begin: u128, end: u128 };

fn compareRange(context: void, a: Range, b: Range) bool {
    _ = context;
    return a.begin < b.begin;
}
pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day5/input.txt";

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

    var list: std.ArrayList(Range) = .empty;
    defer list.deinit(alloc);

    var phase: i32 = 0;
    var result1: u128 = 0;

    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        if (line.written().len == 0) {
            phase += 1;
            line.clearRetainingCapacity(); // reset the accumulating buffer.
            continue;
        }

        if (phase == 0) {
            // std.debug.print("{s}\n", .{line.written()});
            var it = std.mem.splitScalar(u8, line.written(), '-');
            const beginBuffer = it.next() orelse "";
            // const beginLen = beginBuffer.len;
            const begin = try std.fmt.parseInt(u128, beginBuffer, 10);
            const endBuffer = it.next() orelse "";
            const end = try std.fmt.parseInt(u128, endBuffer, 10);
            const fragment = Range{ .begin = begin, .end = end };
            try list.append(alloc, fragment);
            // std.debug.print("{},{}\n", .{ begin, end });
        } else {
            const val = try std.fmt.parseInt(u128, line.written(), 10);
            // std.debug.print("val: {}\n", .{val});
            for (list.items) |range| {
                if (val >= range.begin and val <= range.end) {
                    result1 += 1;
                    break;
                }
            }
        }

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    std.mem.sort(Range, list.items, {}, compareRange);
    var result2: u128 = 0;
    var prevEnd = list.items[0].end;
    for (list.items, 0..) |range, idx| {
        var begin = range.begin;
        if (idx > 0 and (range.begin <= prevEnd)) {
            begin = prevEnd + 1;
        }
        // std.debug.print("range.begin: {}, range.end: {}, begin: {}\n", .{ range.begin, range.end, begin});
        if (begin <= range.end) {
            result2 += range.end - begin + 1;
        }
        if (range.end > prevEnd) {
            prevEnd = range.end;
        }
        // std.debug.print("result2: {}\n", .{result2 });
    }

    std.debug.print("prob1: {}, prob2: {}\n", .{ result1, result2 });
    std.debug.print("End of stream reached\n", .{});
}
