const std = @import("std");

const Point = struct { x: i128, y: i128, z: i128 };
const Pair = struct { a: Point, b: Point };

fn distance(a: Point, b: Point) i128 {
    const dx = (a.x - b.x);
    const dy = (a.y - b.y);
    const dz = (a.z - b.z);
    return dx * dx + dy * dy + dz * dz;
}

fn compare(context: void, p1: Pair, p2: Pair) std.math.Order {
    _ = context;
    return std.math.order(distance(p1.a, p1.b), distance(p2.a, p2.b));
}

fn compareSize(context: void, a: std.AutoHashMap(Point, void), b: std.AutoHashMap(Point, void)) bool {
    _ = context;
    return a.count() > b.count();
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day8/input.txt";

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
        const zBuf = it.next() orelse "";
        const z = try std.fmt.parseInt(i128, zBuf, 10);
        try inputs.append(alloc, Point{ .x = x, .y = y, .z = z });
        // std.debug.print("Point: ({}, {}, {})\n", .{ x, y, z });
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

    var groups: std.ArrayList(std.AutoHashMap(Point, void)) = .empty;
    defer groups.deinit(alloc);

    const steps = 1000;
    var idx: i32 = 0;
    // Remove elements by priority
    while (pq.removeOrNull()) |value| {
        // std.debug.print("Removed: {}\n", .{value});

        var groupA: ?*std.AutoHashMap(Point, void) = null;
        var groupB: ?*std.AutoHashMap(Point, void) = null;

        for (groups.items) |*group| {
            if (group.contains(value.a))
                groupA = group;
            if (group.contains(value.b))
                groupB = group;
        }

        if (groupA == null and groupB == null) {
            var newGroup = std.AutoHashMap(Point, void).init(alloc);
            try newGroup.put(value.a, {});
            try newGroup.put(value.b, {});
            try groups.append(alloc, newGroup);
            // std.debug.print("Add new group, new size {}\n", .{groups.items.len});
        } else if (groupA == null) {
            try groupB.?.put(value.a, {});
            // std.debug.print("1 add to exist group, size {}\n", .{groups.items.len});
        } else if (groupB == null) {
            try groupA.?.put(value.b, {});
            // std.debug.print(" add to exist group, size {}\n", .{groups.items.len});
        } else if (groupA != groupB) {
            // merge groupB into groupA
            var iter = groupB.?.iterator();
            while (iter.next()) |entry| {
                // std.debug.print("{s}: {}\n", .{entry.key_ptr.*, entry.value_ptr.*});
                try groupA.?.put(entry.key_ptr.*, entry.value_ptr.*);
            }
            // remove groupB from groups
            var newGroups: std.ArrayList(std.AutoHashMap(Point, void)) = .empty;
            defer newGroups.deinit(alloc);
            for (groups.items) |*group| {
                if (group != groupB.?) {
                    try newGroups.append(alloc, group.*);
                } else {
                    group.*.deinit();
                }
            }
            // std.debug.print("Merged groups, new size {}\n", .{newGroups.items.len});
            groups.clearRetainingCapacity();
            for (newGroups.items) |*group| {
                try groups.append(alloc, group.*);
            }
        }

        idx += 1;
        if (idx == steps)
            break;
    }

    std.mem.sort(std.AutoHashMap(Point, void), groups.items, {}, compareSize);
    const result1 = groups.items[0].count() * groups.items[1].count() * groups.items[2].count();

    for (groups.items) |*group| {
        // std.debug.print("Group size {}:\n", .{group.count()});
        group.deinit();
    }

    pq.clearRetainingCapacity();
    groups.clearRetainingCapacity();
    for (0..inputs.items.len) |i| {
        for (i + 1..inputs.items.len) |j| {
            const p = Pair{ .a = inputs.items[i], .b = inputs.items[j] };
            try pq.add(p);
        }
    }

    var result2: i128 = 0;
    idx = 0;

    while (pq.removeOrNull()) |value| {
        // std.debug.print("Removed: {}\n", .{value});
        var groupA: ?*std.AutoHashMap(Point, void) = null;
        var groupB: ?*std.AutoHashMap(Point, void) = null;

        for (groups.items) |*group| {
            if (group.contains(value.a))
                groupA = group;
            if (group.contains(value.b))
                groupB = group;
        }

        if (groupA == null and groupB == null) {
            var newGroup = std.AutoHashMap(Point, void).init(alloc);
            try newGroup.put(value.a, {});
            try newGroup.put(value.b, {});
            try groups.append(alloc, newGroup);
            //std.debug.print("Add new group, new size {}\n", .{groups.items.len});
        } else if (groupA == null) {
            try groupB.?.put(value.a, {});
            //std.debug.print("1 add to exist group, size {}\n", .{groups.items.len});
        } else if (groupB == null) {
            try groupA.?.put(value.b, {});
            // std.debug.print(" add to exist group, size {}\n", .{groups.items.len});
        } else if (groupA != groupB) {
            // merge groupB into groupA
            var iter = groupB.?.iterator();
            while (iter.next()) |entry| {
                // std.debug.print("{s}: {}\n", .{entry.key_ptr.*, entry.value_ptr.*});
                try groupA.?.put(entry.key_ptr.*, entry.value_ptr.*);
            }
            // remove groupB from groups
            var newGroups: std.ArrayList(std.AutoHashMap(Point, void)) = .empty;
            defer newGroups.deinit(alloc);
            for (groups.items) |*group| {
                if (group != groupB.?) {
                    try newGroups.append(alloc, group.*);
                } else {
                    group.*.deinit();
                }
            }
            // std.debug.print("Merged groups, new size {}\n", .{newGroups.items.len});
            groups.clearRetainingCapacity();
            for (newGroups.items) |*group| {
                try groups.append(alloc, group.*);
            }
        }
        if (groups.items.len == 1 and groups.items[0].count() == inputs.items.len) {
            std.debug.print("All points merged into one group at step {}, p1 {}, p2 {}\n", .{ idx + 1, value.a, value.b });
            result2 = value.a.x * value.b.x;
            break;
        }
        idx += 1;
        //if (idx == steps)
        //    break;
    }

    for (groups.items) |*group| {
        // std.debug.print("Group size {}:\n", .{group.count()});
        group.deinit();
    }

    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
