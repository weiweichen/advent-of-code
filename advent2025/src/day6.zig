const std = @import("std");

const Range = struct { begin: u128, end: u128 };

fn compareRange(context: void, a: Range, b: Range) bool {
    _ = context;
    return a.begin < b.begin;
}

fn getNexOp(pos: usize, line: []const u8) struct { p: usize, op: u8 } {
    var p = pos;
    while (line[p] == ' ') {
        p -= 1;
    }
    return .{ .p = p, .op = line[p] };
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day6/input.txt";

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

    var list: std.ArrayList(std.ArrayList([]const u8)) = .empty;
    defer list.deinit(alloc);

    var ops: std.ArrayList(u8) = .empty;
    defer ops.deinit(alloc);

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

        var values: std.ArrayList([]const u8) = .empty;

        var it = std.mem.splitScalar(u8, line.written(), ' ');
        while (it.next()) |item| {
            if (item.len == 0) continue;
            if (item[0] == '*' or item[0] == '+') {
                try ops.append(alloc, item[0]);
                continue;
            }

            //const v = try std.fmt.parseInt(u128, item, 10);
            const c = try alloc.dupe(u8, item);
            try values.append(alloc, c);
        }

        if (ops.items.len == 0) {
            // std.debug.print("values len: {} {s}\n", .{ values.items.len, values.items[0] });
            try list.append(alloc, values);
            // std.debug.print("list len: {s}\n", .{list.items[list.items.len - 1].items[0]});
        }

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    const cols = list.items[0].items.len;
    var rows = list.items.len;

    var result1: u128 = 0;

    for (0..cols) |col| {
        var value: u128 = try std.fmt.parseInt(u128, list.items[0].items[col], 10);
        for (1..rows) |row| {
            const v: u128 = try std.fmt.parseInt(u128, list.items[row].items[col], 10);
            if (ops.items[col] == '*') {
                value = value * v;
            } else {
                value = value + v;
            }
        }
        result1 += value;
        // std.debug.print("col: {}, value: {}\n", .{ col, value });
    }

    for (0..rows) |row| {
        for (0..list.items[row].items.len) |idx| {
            alloc.free(list.items[row].items[idx]);
        }
        list.items[row].deinit(alloc);
    }

    var f_reader1: std.fs.File.Reader = file.reader(&read_buf);
    var inputs: std.ArrayList([]const u8) = .empty;
    defer inputs.deinit(alloc);

    while (true) {
        _ = f_reader1.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader1.interface.toss(1); // skip the delimiter byte.
        // std.debug.print("{s}\n", .{line.written()});
        const c = try alloc.dupe(u8, line.written());
        try inputs.append(alloc, c);
        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    rows = inputs.items.len;
    var result2: u128 = 0;
    const opsInput = inputs.items[rows - 1];
    var currOpPos = opsInput.len - 1;
    var currNumPos = opsInput.len - 1;

    while (true) {
        const nextOp = getNexOp(currOpPos, opsInput);
        var localValue: u128 = if (nextOp.op == '*') 1 else 0;

        while (true) {
            var value: u128 = 0;
            for (0..rows - 1) |row| {
                if (inputs.items[row][currNumPos] == ' ') continue;
                value = value * 10 + inputs.items[row][currNumPos] - 48;
            }
            if (value == 0) {
                currNumPos -= 1;
                break;
            }

            if (nextOp.op == '*') {
                localValue *= value;
            } else {
                localValue += value;
            }
            // std.debug.print("value: {}, currNumPos: {}, localValue {}\n", .{value, currNumPos, localValue});
            if (currNumPos == 0) break;
            currNumPos -= 1;
        }
        result2 += localValue;
        // std.debug.print("localValue: {}, result2: {}\n", .{localValue, result2});

        if (nextOp.p == 0) break;
        currOpPos = nextOp.p - 1;
    }

    for (0..rows) |row| {
        alloc.free(inputs.items[row]);
    }

    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
