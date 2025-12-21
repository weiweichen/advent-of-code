const std = @import("std");

pub fn readMatrix(allocator: std.mem.Allocator, filename: []const u8) !struct {
    data: []u8,
    rows: usize,
    cols: usize,
} {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    // Count rows and determine column width
    var rows: usize = 0;
    var cols: usize = 0;
    var iter = std.mem.splitScalar(u8, content, '\n');

    while (iter.next()) |line| {
        if (line.len > 0) {
            rows += 1;
            if (cols == 0) {
                cols = line.len;
            }
        }
    }

    // Allocate flattened matrix
    const data = try allocator.alloc(u8, rows * cols);

    // Fill matrix
    iter = std.mem.splitScalar(u8, content, '\n');
    var row_idx: usize = 0;
    while (iter.next()) |line| {
        if (line.len > 0) {
            for (line, 0..) |char, col_idx| {
                data[row_idx * cols + col_idx] = char;
            }
            row_idx += 1;
        }
    }

    return .{ .data = data, .rows = rows, .cols = cols };
}

pub fn flattenIndex(x: usize, y: usize, col: usize) usize {
    return y * col + x;
}

pub fn isInMatrix(x: i32, y: i32, row: usize, col: usize) bool {
    return (x >= 0 and x < col) and (y >= 0 and y < row);
}

pub fn check(matrix: []u8, x: usize, y: usize, row: usize, col: usize) bool {
    const idx = flattenIndex(x, y, col);
    if (matrix[idx] != '@')
        return false;

    var num: i32 = 0;

    for (0..3) |dx| {
        for (0..3) |dy| {
            if (dx == 1 and dy == 1) continue;
            const nx: i32 = @as(i32, @intCast(x)) + @as(i32, @intCast(dx)) - 1;
            const ny: i32 = @as(i32, @intCast(y)) + @as(i32, @intCast(dy)) - 1;
            if (isInMatrix(nx, ny, row, col)) {
                const nidx = flattenIndex(x + dx - 1, y + dy - 1, col);
                if (matrix[nidx] == '@') {
                    num += 1;
                }
            }
        }
    }
    // if(num < 4)
    //     std.debug.print("x: {}, y: {}, num: {}\n", .{ x, y, num });

    return num < 4;
}

const Point = struct {
    x: usize,
    y: usize,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const filename = "../data/day4/input.txt";

    const matrix = try readMatrix(allocator, filename);
    defer allocator.free(matrix.data);

    // Access element at [row][col]
    // const row = 0;
    // const col = 0;
    // const char = matrix.data[row * matrix.cols + col];
    // std.debug.print("matrix[{}][{}] = {c}\n", .{ row, col, char });

    // Print entire matrix
    // for (0..matrix.rows) |i| {
    //     for (0..matrix.cols) |j| {
    //         std.debug.print("{c}", .{matrix.data[i * matrix.cols + j]});
    //     }
    //     std.debug.print("\n", .{});
    // }

    var result1: i64 = 0;
    var result2: i64 = 0;

    //var list = std.array_list.Managed(Point).init(allocator);
    //defer list.deinit();
    var list: std.ArrayList(Point) = .empty;
    defer list.deinit(allocator);

    for (0..matrix.rows) |i| {
        for (0..matrix.cols) |j| {
            if (check(matrix.data, j, i, matrix.rows, matrix.cols)) {
                result1 += 1;
                const point = Point{ .x = j, .y = i };
                try list.append(allocator, point);
            }
        }
    }

    while (true) {
        var curr: i64 = 0;
        list.clearRetainingCapacity();
        for (0..matrix.rows) |i| {
            for (0..matrix.cols) |j| {
                if (check(matrix.data, j, i, matrix.rows, matrix.cols)) {
                    curr += 1;
                    const point = Point{ .x = j, .y = i };
                    try list.append(allocator, point);
                }
            }
        }
        result2 += curr;
        // std.debug.print("list size {}\n", .{list.items.len});
        for (list.items) |p| {
            const idx = flattenIndex(p.x, p.y, matrix.cols);
            matrix.data[idx] = 'x';
        }
        if (curr == 0)
            break;
    }

    std.debug.print("prob1: {}, prob2: {}\n", .{ result1, result2 });
    std.debug.print("The End!!\n", .{});
}
