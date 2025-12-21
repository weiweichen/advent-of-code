const std = @import("std");

pub fn bankMaxNHelper(bank: []u8, start: usize, end: usize) struct { pos: usize, val: u8 } {
    var max = bank[end];
    var p: usize = end;

    for (0..(end - start + 1)) |i| { // end is exclusive
        const c = end - i;
        if (bank[c] >= max) {
            p = c;
            max = bank[c];
        }
    }

    return .{ .pos = p, .val = max };
}

pub fn bankMaxN(bank: []u8, n: usize) i128 {
    var start: usize = 0;
    var end: usize = bank.len - n;
    var result: i128 = 0;

    for (0..n) |_| {
        const r = bankMaxNHelper(bank, start, end);
        end += 1;
        start = r.pos + 1;
        result = result * 10 + (@as(i128, r.val) - 48);
        // std.debug.print("r.pos: {}, r.val: {}\n", .{r.pos, r.val - 48});
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day3/input.txt";

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

    var result1: i128 = 0;
    var result2: i128 = 0;
    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        result1 += bankMaxN(line.written(), 2);
        result2 += bankMaxN(line.written(), 12);
        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    std.debug.print("prob1: {}, prob2: {}\n", .{ result1, result2 });
    std.debug.print("End of stream reached\n", .{});
}
