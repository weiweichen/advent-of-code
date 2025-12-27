// Got the idea from https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
// Naive implementation with
// zig build-exe day10.zig -O ReleaseFast
// usr/bin/time ./day10 takes
//        99.92 real         8.96 user        90.49 sys
// on my MPB M2 Pro

const std = @import("std");

const Point = struct { x: i128, y: i128 };

const Machine = struct { goal: []u8, buttons: std.ArrayList(std.ArrayList(usize)), joltage: []u16 };

const State1 = struct { config: []const u8, steps: u128 };

fn solve1(m: Machine, alloc: std.mem.Allocator) !u128 {
    // std.debug.print("goal:    {s}\n", .{m.goal});
    // std.debug.print("buttons: {any}\n", .{m.buttons});
    // std.debug.print("joltage: {any}\n", .{m.joltage});

    var configs = std.StringHashMap(u128).init(alloc);
    var queue: std.ArrayList(State1) = .empty;
    defer configs.deinit();
    defer queue.deinit(alloc);

    var init_config = try alloc.dupe(u8, m.goal);
    // std.debug.print("initial_config len: {}\n", .{init_config.len});
    for (0..init_config.len) |i| {
        init_config[i] = '.';
    }

    try configs.put(init_config, 0);
    try queue.append(alloc, State1{ .config = init_config, .steps = 0 });
    var found = false;
    var steps: u128 = 0;

    while (!found) {
        if (queue.items.len == 0) break;
        const state = queue.orderedRemove(0);

        for (m.buttons.items) |button| {
            var new_config = try alloc.dupe(u8, state.config);

            for (button.items) |pos| {
                if (new_config[pos] == '.') {
                    new_config[pos] = '#';
                } else {
                    new_config[pos] = '.';
                }
            }

            if (std.mem.eql(u8, new_config, m.goal)) {
                // std.debug.print("Reached goal config {s} in steps {}\n", .{ new_config, state.steps + 1 });
                alloc.free(new_config);
                steps = state.steps + 1;
                found = true;
                break;
            }

            const new_steps = state.steps + 1;

            if (configs.contains(new_config)) {
                const existing = configs.getEntry(new_config);
                if (new_steps < existing.?.value_ptr.*) {
                    existing.?.value_ptr.* = new_steps;
                    try queue.append(alloc, State1{ .config = existing.?.key_ptr.*, .steps = new_steps });
                }
                alloc.free(new_config);
            } else {
                try configs.put(new_config, new_steps);
                try queue.append(alloc, State1{ .config = new_config, .steps = new_steps });
            }
        }
    }

    var iter = configs.keyIterator();
    while (iter.next()) |key| {
        alloc.free(key.*);
    }

    return steps;
}

const KeyType = struct {
    // config: []u8,
    joltage: []u16,

    pub fn hash(self: KeyType) u64 {
        var hasher = std.hash.Wyhash.init(0);
        // std.hash.autoHashStrat(&hasher, self.config, .Deep);
        std.hash.autoHashStrat(&hasher, self.joltage, .Deep);
        return hasher.final();
    }

    pub fn eql(self: KeyType, other: KeyType) bool {
        // return std.mem.eql(u8, self.config, other.config) and std.mem.eql(u128, self.joltage, other.joltage);
        return std.mem.eql(u16, self.joltage, other.joltage);
    }
};

// const State2 = struct { config: KeyType, steps: u128 };
const PatternHashMapType = std.HashMap(KeyType, u32, struct {
    pub fn hash(_: @This(), key: KeyType) u64 {
        return key.hash();
    }
    pub fn eql(_: @This(), a: KeyType, b: KeyType) bool {
        return a.eql(b);
    }
}, std.hash_map.default_max_load_percentage);

fn getPatterns(m: Machine, alloc: std.mem.Allocator) !PatternHashMapType {
    // Use HashMap with custom hash/eql
    var patterns = PatternHashMapType.init(alloc);
    // std.debug.print("Getting patterns for machine {any}\n", .{m});
    const numButtons: u5 = @intCast(m.buttons.items.len);
    const one: u32 = 1;
    const ubound: u32 = one << numButtons;
    const zeros = KeyType{
        .joltage = try alloc.alloc(u16, m.goal.len),
    };
    @memset(zeros.joltage, 0);
    try patterns.put(zeros, 0);

    for (1..ubound) |idx| {
        var mask: u32 = 1;
        var pattern = KeyType{
            .joltage = try alloc.alloc(u16, m.goal.len),
        };
        @memset(pattern.joltage, 0);
        var count: u32 = 0;

        for (0..numButtons + 1) |bit| {
            if (idx & mask != 0) {
                for (m.buttons.items[bit].items) |pos| {
                    pattern.joltage[pos] += 1;
                }
                count += 1;
            }
            mask = mask << 1;
        }
        if (patterns.contains(pattern)) {
            const entry = patterns.getEntry(pattern);
            entry.?.value_ptr.* = @min(entry.?.value_ptr.*, count);
            alloc.free(pattern.joltage);
        } else {
            try patterns.put(pattern, count);
        }
    }

    return patterns;
}

fn solve2Help(resultCache: *PatternHashMapType, goal: []u16, patterns: *PatternHashMapType, alloc: std.mem.Allocator) !u128 {
    if (std.mem.allEqual(u16, goal, 0)) {
        alloc.free(goal);
        return 0;
    }

    if (resultCache.getPtr(KeyType{ .joltage = goal })) |value| {
        alloc.free(goal);
        return value.*;
    }

    // if (patterns.getPtr(KeyType{ .joltage = goal })) |value| {
    //     alloc.free(goal);
    //     return value.*;
    // }

    var result: u32 = 1000000000;
    var iter = patterns.iterator();
    while (iter.next()) |entry| {
        const pattern = entry.key_ptr.*;
        const cost = entry.value_ptr.*;
        var new_goal = try alloc.alloc(u16, goal.len);
        var recurse = true;

        for (0..pattern.joltage.len) |i| {
            const g = goal[i];
            const p = pattern.joltage[i];
            if (p > g) {
                recurse = false;
                break;
            }
            if (@mod(g, 2) != @mod(p, 2)) {
                recurse = false;
                break;
            }
        }
        if (recurse) {
            for (0..pattern.joltage.len) |i| {
                new_goal[i] = @divFloor((goal[i] - pattern.joltage[i]), 2);
            }
            result = @min(result, cost + 2 * (try solve2Help(resultCache, new_goal, patterns, alloc)));
        } else {
            alloc.free(new_goal);
        }
    }

    try resultCache.put(KeyType{ .joltage = goal }, result);
    return result;
}

fn solve2(m: Machine, alloc: std.mem.Allocator) !u128 {
    var patterns = try getPatterns(m, alloc);

    // std.debug.print("patterns size {any}\n", .{patterns.count()});

    var resultCache: PatternHashMapType = PatternHashMapType.init(alloc);
    defer resultCache.deinit();

    const goal = try alloc.dupe(u16, m.joltage);
    const result = try solve2Help(&resultCache, goal, &patterns, alloc);

    var iter = patterns.iterator();
    while (iter.next()) |entry| {
        // std.debug.print("Pattern: {any} count {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        alloc.free(entry.key_ptr.*.joltage);
    }
    patterns.deinit();

    var resultCacheIter = resultCache.iterator();
    while (resultCacheIter.next()) |entry| {
        // std.debug.print("Pattern: {any} count {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        alloc.free(entry.key_ptr.*.joltage);
    }

    return result;
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const filename = "../data/day10/input.txt";

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

    var machines: std.ArrayList(Machine) = .empty;
    defer machines.deinit(alloc);

    // Parsing input
    while (true) {
        _ = f_reader.interface.streamDelimiter(&line.writer, '\n') catch |err| {
            if (err == error.EndOfStream) {
                break;
            } else {
                return err;
            }
        };

        _ = f_reader.interface.toss(1); // skip the delimiter byte.
        std.debug.print("{s}\n", .{line.written()});

        var it = std.mem.splitScalar(u8, line.written(), ']');
        const gBuf = it.next() orelse "";
        const goal = try alloc.dupe(u8, gBuf[1..]); // skip the starting '['
        // std.debug.print("Goal: {s}\n", .{goal});

        var machine = Machine{ .goal = goal, .buttons = .empty, .joltage = try alloc.alloc(u16, goal.len) };

        it = std.mem.splitScalar(u8, it.next() orelse "", '{');
        const bBuf = it.next() orelse "";
        var iter1 = std.mem.splitScalar(u8, bBuf, ')');
        while (iter1.next()) |button_str| {
            if (button_str.len < 3) continue;
            try machine.buttons.append(alloc, .empty);
            var button_it = std.mem.splitScalar(u8, button_str[2..], ','); // skip starting '('
            while (button_it.next()) |pos_str| {
                const pos = try std.fmt.parseInt(usize, pos_str, 10);
                try machine.buttons.items[machine.buttons.items.len - 1].append(alloc, pos);
            }
        }
        // std.debug.print("buttons: {any}\n", .{machine.buttons.items});

        const jBuf = it.next() orelse "";
        var iter2 = std.mem.splitScalar(u8, jBuf[0 .. jBuf.len - 1], ',');
        var idx: usize = 0;
        while (iter2.next()) |jstr| {
            const joltage = try std.fmt.parseInt(u16, jstr, 10);
            machine.joltage[idx] = joltage;
            idx += 1;
        }

        // std.debug.print("joltage: {any}\n", .{machine.joltage});

        try machines.append(alloc, machine);
        line.clearRetainingCapacity(); // reset the accumulating buffer.
    }

    var result1: u128 = 0;
    var result2: u128 = 0;

    for (machines.items, 0..machines.items.len) |machine, idx| {
        const v1 = try solve1(machine, alloc);
        const v2 = try solve2(machine, alloc);
        result1 += v1;
        result2 += v2;
        std.debug.print("solving machine {}\n", .{idx});
    }

    // Free memory
    for (machines.items) |*machine| {
        alloc.free(machine.goal);
        for (machine.buttons.items) |*button| {
            button.deinit(alloc);
        }
        machine.buttons.deinit(alloc);
        alloc.free(machine.joltage);
    }

    // Create a min-heap priority queue
    std.debug.print("result1: {}, result2 {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
