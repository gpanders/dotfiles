var buf = try allocator.alloc(u8, {%});
defer allocator.free(buf);
