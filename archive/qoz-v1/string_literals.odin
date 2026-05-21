package main

import "core:mem"

decode_string_literal_bytes :: proc(raw: string, allocator: mem.Allocator) -> []i8 {
    decoded := make([dynamic]i8, allocator)
    i := 0
    for i < len(raw) {
        c := raw[i]
        if c == '\\' && i + 1 < len(raw) {
            i += 1
            esc := raw[i]
            value: i8
            switch esc {
            case 'n': value = 10
            case 'r': value = 13
            case 't': value = 9
            case '\\': value = '\\'
            case '"': value = '"'
            case '\'': value = '\''
            case '0': value = 0
            case: value = cast(i8)esc
            }
            append(&decoded, value)
            i += 1
            continue
        }
        append(&decoded, cast(i8)c)
        i += 1
    }
    return decoded[:]
}
