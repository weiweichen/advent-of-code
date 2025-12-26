const std = @import("std");

const Point = struct { x: i128, y: i128 };
const Pair = struct { a: Point, b: Point };
const Line = struct { begin: i128, end: i128, shared: i128 };

fn area(a: Point, b: Point) u128 {
    const dx = @abs(a.x - b.x) + 1;
    const dy = @abs(a.y - b.y) + 1;
    return dx * dy;
}

fn compare(context: void, p1: Pair, p2: Pair) std.math.Order {
    _ = context;
    return std.math.order(area(p2.a, p2.b), area(p1.a, p1.b));
}

fn comparePX(context: void, a: Point, b: Point) bool {
    _ = context;
    if (a.x == b.x) return a.y < b.y;
    return a.x < b.x;
}

fn comparePY(context: void, a: Point, b: Point) bool {
    _ = context;
    if (a.y == b.y) return a.x < b.x;
    return a.y < b.y;
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day9/input.txt";

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

    var inputs: std.ArrayList(Point) = .empty;
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
        var it = std.mem.splitScalar(u8, line.written(), ',');

        const xBuf = it.next() orelse "";
        const x = try std.fmt.parseInt(i128, xBuf, 10);
        const yBuf = it.next() orelse "";
        const y = try std.fmt.parseInt(i128, yBuf, 10);
        try inputs.append(alloc, Point{ .x = x, .y = y });
        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    // Create a min-heap priority queue
    var pq = std.PriorityQueue(Pair, void, compare).init(alloc, {});
    defer pq.deinit();

    for (0..inputs.items.len) |i| {
        for (i + 1..inputs.items.len) |j| {
            const p = Pair{ .a = inputs.items[i], .b = inputs.items[j] };
            try pq.add(p);
        }
    }
    const p = pq.peek().?;
    std.debug.print("Smallest area pair: Point1 ({}, {}), Point2 ({}, {}), Area: {}\n", .{ p.a.x, p.a.y, p.b.x, p.b.y, area(p.a, p.b) });

    const result1: u128 = area(p.a, p.b);

    const numLines = @divFloor(inputs.items.len, 2);
    std.mem.sort(Point, inputs.items, {}, comparePX);
    var inputXSorted: std.ArrayList(Line) = .empty;
    for (0..numLines) |i| {
        const begin = inputs.items[i * 2].y;
        const end = inputs.items[i * 2 + 1].y;
        const shared = inputs.items[i * 2].x;
        try inputXSorted.append(alloc, Line{ .begin = begin, .end = end, .shared = shared });
    }
    // std.debug.print("After sorting by X {}:\n", .{inputXSorted});

    std.mem.sort(Point, inputs.items, {}, comparePY);
    var inputYSorted: std.ArrayList(Line) = .empty;
    for (0..numLines) |i| {
        const begin = inputs.items[i * 2].x;
        const end = inputs.items[i * 2 + 1].x;
        const shared = inputs.items[i * 2].y;
        try inputYSorted.append(alloc, Line{ .begin = begin, .end = end, .shared = shared });
    }
    // std.debug.print("After sorting by Y {}:\n", .{inputYSorted});

    defer inputXSorted.deinit(alloc);
    defer inputYSorted.deinit(alloc);

    var xMap = std.AutoHashMap(i128, usize).init(alloc);
    for (inputXSorted.items, 0..numLines) |l, idx| {
        try xMap.put(l.shared, idx);
    }
    var yMap = std.AutoHashMap(i128, usize).init(alloc);
    for (inputYSorted.items, 0..numLines) |l, idx| {
        try yMap.put(l.shared, idx);
    }

    defer xMap.deinit();
    defer yMap.deinit();

    var grids: std.ArrayList([]u8) = .empty;
    for (0..numLines) |_| {
        var str = try alloc.alloc(u8, numLines);
        for (0..numLines) |j| {
            str[j] = '.';
        }
        try grids.append(alloc, str);
    }
    defer grids.deinit(alloc);

    for (inputs.items) |point| {
        const xIdx = xMap.get(point.x).?;
        const yIdx = yMap.get(point.y).?;
        // std.debug.print("Point ({}, {}) mapped to grid ({}, {})\n", .{ point.x, point.y, xIdx, yIdx });
        grids.items[yIdx][xIdx] = '#';
    }

    for (inputXSorted.items) |l| {
        const xId = xMap.get(l.shared).?;
        const yId1 = yMap.get(l.begin).?;
        const yId2 = yMap.get(l.end).?;
        for (yId1 + 1..yId2) |col| {
            grids.items[col][xId] = '#';
        }
        // std.debug.print("Line X - begin: {}, end: {}, shared: {}, yId1: {}, yId2: {}\n", .{ l.begin, l.end, l.shared, yId1, yId2 });
    }
    for (inputYSorted.items) |l| {
        const yId = yMap.get(l.shared).?;
        const xId1 = xMap.get(l.begin).?;
        const xId2 = xMap.get(l.end).?;
        for (xId1 + 1..xId2) |row| {
            grids.items[yId][row] = '#';
        }
        // std.debug.print("Line X - begin: {}, end: {}, shared: {}, xId1: {}, xId2: {}\n", .{ l.begin, l.end, l.shared, xId1, xId2 });
    }

    //const inside = Point{ .x = 2, .y = 1 };
    const inside = Point{ .x = 128, .y = 1 };
    std.debug.print("inside point: {}\n", .{inside});

    var stack: std.ArrayList(Point) = .empty;
    defer stack.deinit(alloc);
    try stack.append(alloc, inside);
    while (stack.items.len > 0) {
        const curr = stack.pop().?;
        if (curr.x >= 0 and curr.x < numLines and curr.y >= 0 and curr.y < numLines) {
            const xId: usize = @intCast(curr.x);
            const yId: usize = @intCast(curr.y);
            if (grids.items[yId][xId] == '.') {
                grids.items[yId][xId] = 'O';
                try stack.append(alloc, Point{ .x = curr.x - 1, .y = curr.y });
                try stack.append(alloc, Point{ .x = curr.x + 1, .y = curr.y });
                try stack.append(alloc, Point{ .x = curr.x, .y = curr.y - 1 });
                try stack.append(alloc, Point{ .x = curr.x, .y = curr.y + 1 });
            }
        }
    }

    for (0..numLines) |row| {
        std.debug.print("{s}\n", .{grids.items[row]});
    }

    var result2: u128 = 0;

    for (0..inputs.items.len - 1) |i| {
        for (i + 1..inputs.items.len) |j| {
            const pi = inputs.items[i];
            const pj = inputs.items[j];
            const v = area(pi, pj);
            if (v <= result2)
                continue;
            const xId1 = xMap.get(pi.x).?;
            const xId2 = xMap.get(pj.x).?;
            const yId1 = yMap.get(pi.y).?;
            const yId2 = yMap.get(pj.y).?;
            const xIdmin = if (xId1 < xId2) xId1 else xId2;
            const xIdmax = if (xId1 < xId2) xId2 else xId1;
            const yIdmin = if (yId1 < yId2) yId1 else yId2;
            const yIdmax = if (yId1 < yId2) yId2 else yId1;
            var enclosed = true;
            for (yIdmin..yIdmax + 1) |yId| {
                if (enclosed == false) break;
                for (xIdmin..xIdmax + 1) |xId| {
                    if (grids.items[yId][xId] == '.') {
                        enclosed = false;
                        break;
                    }
                }
            }
            if (enclosed and v > result2) {
                result2 = v;
            }
        }
    }

    for (0..numLines) |row| {
        alloc.free(grids.items[row]);
    }

    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
