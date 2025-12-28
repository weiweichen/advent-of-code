const std = @import("std");

const Shape = struct { nodes: [][]u8, area: u32 };

const Region = struct {
    id: usize,
    width: u32,
    height: u32,
    config: std.ArrayList(u32),

    const SelfType = @This();
    pub fn area(self: SelfType) u32 {
        return self.width * self.height;
    }
};

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day12/test.txt";

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

    var lines: std.ArrayList([]u8) = .empty;
    defer lines.deinit(alloc);

    var regions: std.ArrayList(Region) = .empty;
    defer regions.deinit(alloc);

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
        try lines.append(alloc, try alloc.dupe(u8, line.written()));

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    for (30..lines.items.len) |i| {
        const l = lines.items[i];
        // std.debug.print("line {s}\n", .{lines.items[i]});

        var it1 = std.mem.splitScalar(u8, l, ':');
        const sizeBuffer = it1.next() orelse "";
        var it2 = std.mem.splitScalar(u8, sizeBuffer, 'x');
        const widthBuf = it2.next() orelse "";
        const width = try std.fmt.parseInt(u32, widthBuf, 10);
        const heightBuf = it2.next() orelse "";
        const height = try std.fmt.parseInt(u32, heightBuf, 10);
        try regions.append(alloc, Region{
            .id = regions.items.len,
            .width = width,
            .height = height,
            .config = .empty,
        });

        const configBuffer = it1.next() orelse "";
        var it3 = std.mem.splitScalar(u8, configBuffer[1..], ' ');
        while (it3.next()) |item| {
            const v = try std.fmt.parseInt(u32, item, 10);
            try regions.items[regions.items.len - 1].config.append(alloc, v);
        }
        // std.debug.print("Region: width {}, height {}, config len {}\n", .{ width, height, regions.items[regions.items.len - 1].config.items.len });

    }

    var shapes: std.ArrayList(Shape) = .empty;
    defer shapes.deinit(alloc);

    for (0..6) |i| {
        var shape_nodes: [][]u8 = try alloc.alloc([]u8, 3);
        var area: u32 = 0;
        const g = i * 5 + 1;
        for (0..3) |j| {
            shape_nodes[j] = try alloc.alloc(u8, 3);
            for (0..3) |k| {
                shape_nodes[j][k] = lines.items[g + j][k];
                if (shape_nodes[j][k] == '#') {
                    area += 1;
                }
            }
        }

        try shapes.append(alloc, Shape{
            .nodes = shape_nodes,
            .area = area,
        });

        // std.debug.print("Shape {any}:\n", .{shapes.items[shapes.items.len - 1]});
    }

    var result1: u128 = 0;
    for (regions.items) |region| {
        var totalOccupied: u32 = 0;
        var totalShapes: u32 = 0;

        for (region.config.items, 0..6) |num, idx| {
            totalOccupied += shapes.items[idx].area * num;
            totalShapes += num;
        }

        if (totalOccupied > region.area()) {
            // std.debug.print("Region {} doesn't fit\n", . {region.id});
            continue;
        }

        const numTilesW = @divFloor(region.width, 3);
        const numTilesH = @divFloor(region.height, 3);
        const totalTiles = numTilesW * numTilesH;
        if (totalTiles >= totalShapes) {
            // std.debug.print("Region {} easily fit\n", . {region.id});
            result1 += 1;
            continue;
        }

        std.debug.print("Region {} needs to be checked {any}\n", .{ region.id, region });
    }

    for (lines.items) |l| {
        alloc.free(l);
    }

    for (shapes.items) |s| {
        for (s.nodes) |row| {
            alloc.free(row);
        }
        alloc.free(s.nodes);
    }

    // Create a min-heap priority queue
    std.debug.print("result1: {}\n", .{result1});
    std.debug.print("The End!!\n", .{});
}
