const std = @import("std");

pub fn isInvalid1(i: u128) !bool {
    var buf: [40]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{d}", .{i});
    const len = str.len;

    if (@mod(len, 2) != 0)
        return false;

    return std.mem.eql(u8, str[0 .. len / 2], str[len / 2 .. len]);
}

/// Returns true if all items in the iterator are equal.
fn allItemsEqual(it: anytype) bool {
    // Get the first item to compare others against
    const first = it.next() orelse return false; // Empty iterator is vacuously true
    while (it.next()) |item| {
        if (!std.mem.eql(u8, first, item)) {
            return false;
        }
    }
    return true;
}

pub fn ChunkIterator(comptime T: type) type {
    return struct {
        slice: []const T,
        chunk_size: usize,
        index: usize = 0,

        const Self = @This();

        pub fn next(self: *Self) ?[]const T {
            if (self.index >= self.slice.len) {
                return null;
            }

            const start = self.index;
            const end = @min(self.index + self.chunk_size, self.slice.len);
            self.index = end;

            return self.slice[start..end];
        }
    };
}

pub fn chunks(slice: anytype, chunk_size: usize) ChunkIterator(@TypeOf(slice[0])) {
    return .{
        .slice = slice,
        .chunk_size = chunk_size,
    };
}

pub fn isInvalid2(i: u128) !bool {
    var buf: [40]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{d}", .{i});
    const len = str.len;

    var p: usize = 1;
    while (p <= len / 2) : (p += 1) {
        if (@mod(len, p) != 0)
            continue;
        if (i == 123123) {
            std.debug.print("checking p: {} len: {}\n", .{ p, len });
        }
        var iter = chunks(str, p);
        if (allItemsEqual(&iter))
            return true;
    }

    return false;
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day2/input.txt";

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

    var result1: u128 = 0;
    var result2: u128 = 0;
    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, ',') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        // std.debug.print("{s}\n", .{line.written()});
        var it = std.mem.splitScalar(u8, line.written(), '-');
        const beginBuffer = it.next() orelse "";
        // const beginLen = beginBuffer.len;
        const begin = try std.fmt.parseInt(u128, beginBuffer, 10);
        // std.debug.print("{},{}\n", .{ begin, beginLen });
        const endBuffer = it.next() orelse "";
        //const endLen = endBuffer.len;
        const end = try std.fmt.parseInt(u128, endBuffer, 10);
        //std.debug.print("{},{}\n", .{ end, endLen });

        var i = begin;
        while (i <= end) {
            const value1 = try isInvalid1(i);
            const value2 = try isInvalid2(i);
            if (value1)
                result1 += i;
            if (value2)
                result2 += i;

            i += 1;
        }

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    std.debug.print("prob1: {}, prob2: {}\n", .{ result1, result2 });
    std.debug.print("End of stream reached\n", .{});
}
