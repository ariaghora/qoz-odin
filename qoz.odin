package main

import "core:mem"
import "core:fmt"
import "core:os"

print_tree :: proc(node: ^Node, indent: int = 0) {
    if node == nil do return
    
    for _ in 0..<indent do fmt.print("  ")
    fmt.printfln("%v", node.node_kind)
    
    #partial switch node.node_kind {
    case .Program, .Statement_List:
        for child in node.payload.(Node_Statement_List).nodes {
            print_tree(child, indent + 1)
        }
    case .Var_Def:
        print_tree(node.payload.(Node_Var_Def).content, indent + 1)
    case .Fn_Def:
        for stmt in node.payload.(Node_Fn_Def).body {
            print_tree(stmt, indent + 1)
        }
    case .Print:
        print_tree(node.payload.(Node_Print).content, indent + 1)
    case .Fn_Call:
        print_tree(node.payload.(Node_Call).callee, indent + 1)
        for arg in node.payload.(Node_Call).args {
            print_tree(arg, indent + 1)
        }
    case .Bin_Op:
        print_tree(node.payload.(Node_Bin_Op).left, indent + 1)
        print_tree(node.payload.(Node_Bin_Op).right, indent + 1)
    }
}


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
    defer node_free(root)

    fmt.println(root)

    sem_ctx := semantic_analyze(root)
    fmt.println(sem_ctx.errors[:])

	print_tree(root)
}
