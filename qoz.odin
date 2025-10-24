package main

import "core:mem"
import "core:fmt"
import "core:os"


main :: proc() {
	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}
	}

	ensure(len(os.args) == 2, "specify source name as the first positional argument")
	file_name := os.args[1]

	source_bytes, ok := os.read_entire_file(file_name)
	ensure(ok, fmt.tprintfln("cannot open %s", file_name))
    defer delete(source_bytes)
	source := string(source_bytes)

    tokens, err_tokenize := tokenize(source)
	ensure(err_tokenize == nil, fmt.tprint(err_tokenize))
    defer delete(tokens)

    root, err_parse := parse(tokens)
	ensure(err_parse == nil, fmt.tprint(err_parse))
    defer free_node(root)
    fmt.println(root)
}
