var buf = try allocator.alloc(u8, $0);
defer allocator.free(buf);
