const std = @import("std");

const Node = struct { name: []const u8, outputs: std.ArrayList(usize) };

fn dfs1(curr: usize, end: usize, path: *std.ArrayList(usize), pathNodes: *std.AutoHashMap(usize, void), nodes: std.ArrayList(Node), alloc: std.mem.Allocator) !i128 {
    if (curr == end) {
        // std.debug.print("Found path: {any}\n", .{path.*});
        return 1;
    }

    if (pathNodes.contains(curr))
        return 0;

    try path.append(alloc, curr);
    try pathNodes.put(curr, {});
    var res: i128 = 0;

    for (nodes.items[curr].outputs.items) |output| {
        res += try dfs1(output, end, path, pathNodes, nodes, alloc);
    }
    _ = path.pop();
    _ = pathNodes.remove(curr);

    return res;
}

fn printNodes(nodes: std.ArrayList(Node)) void {
    for (nodes.items, 0..) |node, idx| {
        std.debug.print("Node {}: {s} -> ", .{ idx, node.name });
        for (node.outputs.items) |output| {
            std.debug.print("{s}, ", .{nodes.items[output].name});
        }
        std.debug.print("\n", .{});
    }
}

fn solve1(nodes: std.ArrayList(Node), alloc: std.mem.Allocator) !i128 {
    // std.debug.print("nodes {}, nodeMap {}\n", .{ nodes.items.len, nodeMap.count() });
    var path: std.ArrayList(usize) = .empty;
    var pathNodes = std.AutoHashMap(usize, void).init(alloc);

    var startIdx: usize = 0;
    var endIdx: usize = 0;
    for (nodes.items, 0..nodes.items.len) |node, idx| {
        if (std.mem.eql(u8, node.name, "you")) {
            startIdx = idx;
        }
        if (std.mem.eql(u8, node.name, "out")) {
            endIdx = idx;
        }
    }

    // std.debug.print("node1: startIdx {}, endIdx {}\n", .{ startIdx, endIdx });

    return try dfs1(startIdx, endIdx, &path, &pathNodes, nodes, alloc);
}

const KeyType = struct {
    curr: usize,
    hasFFT: bool,
    hasDAC: bool,

    pub fn hash(self: KeyType) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, self.curr);
        std.hash.autoHash(&hasher, self.hasFFT);
        std.hash.autoHash(&hasher, self.hasDAC);
        return hasher.final();
    }

    pub fn eql(self: KeyType, other: KeyType) bool {
        return self.curr == other.curr and
            self.hasFFT == other.hasFFT and
            self.hasDAC == other.hasDAC;
    }
};

// const State2 = struct { config: KeyType, steps: u128 };
const CacheType = std.HashMap(KeyType, i128, struct {
    pub fn hash(_: @This(), key: KeyType) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: KeyType, b: KeyType) bool {
        return a.eql(b);
    }
}, std.hash_map.default_max_load_percentage);

fn dfs2(curr: usize, end: usize, fft: usize, dac: usize, hasFFT: bool, hasDAC: bool, cache: *CacheType, nodes: std.ArrayList(Node)) !i128 {
    if (cache.getPtr(KeyType{ .curr = curr, .hasFFT = hasFFT, .hasDAC = hasDAC })) |value| {
        return value.*;
    }

    if (curr == end and hasFFT and hasDAC) {
        return 1;
    }

    var res: i128 = 0;
    const currHasFFT = hasFFT or (curr == fft);
    const currHasDAC = hasDAC or (curr == dac);
    for (nodes.items[curr].outputs.items) |output| {
        res += try dfs2(output, end, fft, dac, currHasFFT, currHasDAC, cache, nodes);
    }

    try cache.put(KeyType{ .curr = curr, .hasFFT = hasFFT, .hasDAC = hasDAC }, res);
    return res;
}

fn solve2(nodes: std.ArrayList(Node), alloc: std.mem.Allocator) !i128 {
    // std.debug.print("nodes {}, nodeMap {}\n", .{ nodes.items.len, nodeMap.count() });
    var svr: usize = 0;
    var out: usize = 0;
    var dac: usize = 0;
    var fft: usize = 0;
    var you: usize = 0;

    for (nodes.items, 0..nodes.items.len) |node, idx| {
        if (std.mem.eql(u8, node.name, "svr")) {
            svr = idx;
        }
        if (std.mem.eql(u8, node.name, "you")) {
            you = idx;
        }
        if (std.mem.eql(u8, node.name, "out")) {
            out = idx;
        }

        if (std.mem.eql(u8, node.name, "fft")) {
            fft = idx;
        }

        if (std.mem.eql(u8, node.name, "dac")) {
            dac = idx;
        }
    }

    // std.debug.print("svr {}, out {}, n1 {}, n2 {} \n", .{ svr, out, fft, dac });
    var cache = CacheType.init(alloc);
    return try dfs2(svr, out, fft, dac, false, false, &cache, nodes);
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day11/input.txt";

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

    var nodeMap = std.StringHashMap(usize).init(alloc);
    defer nodeMap.deinit();

    var nodes: std.ArrayList(Node) = .empty;
    defer nodes.deinit(alloc);

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

        var it1 = std.mem.splitScalar(u8, line.written(), ':');
        const nameBuffer = it1.next() orelse "";

        const nodeIdx = nodeMap.get(nameBuffer);
        var idx: usize = 0;
        if (nodeIdx) |v| {
            idx = v;
        } else {
            idx = nodes.items.len;
            const name = try alloc.dupe(u8, nameBuffer);
            try nodeMap.put(name, idx);
            try nodes.append(alloc, Node{ .name = name, .outputs = .empty });
        }

        const outputsBuffer = it1.next() orelse "";

        var it2 = std.mem.splitScalar(u8, outputsBuffer[1..], ' ');
        while (it2.next()) |output| {
            // std.debug.print("output: {s}\n", .{output});
            const outIdxOpt = nodeMap.get(output);
            var outIdx: usize = 0;
            if (outIdxOpt) |v| {
                outIdx = v;
            } else {
                outIdx = nodes.items.len;
                const out = try alloc.dupe(u8, output);
                try nodeMap.put(out, outIdx);
                try nodes.append(alloc, Node{ .name = out, .outputs = .empty });
            }

            try nodes.items[idx].outputs.append(alloc, outIdx);
        }
        // std.debug.print("node {s} #outs {}\n",.{nodes.items[idx].name.*, nodes.items[idx].outputs.items.len});

        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    // printNodes(nodes);

    const result1 = try solve1(nodes, alloc);
    const result2 = try solve2(nodes, alloc);

    var iter = nodeMap.iterator();
    while (iter.next()) |entry| {
        alloc.free(entry.key_ptr.*);
    }

    // Create a min-heap priority queue
    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
