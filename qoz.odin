package main

import "core:path/filepath"
import "core:os/os2"
import "core:mem"
import vmem "core:mem/virtual"
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

	alloc_lexer, alloc_parser, alloc_semantic: vmem.Arena

	err_alloc_lexer := vmem.arena_init_growing(&alloc_lexer)
	ensure(err_alloc_lexer == nil)
	arena_lexer := vmem.arena_allocator(&alloc_lexer)

	err_alloc_parser := vmem.arena_init_growing(&alloc_parser)
	ensure(err_alloc_parser == nil)
	arena_parser := vmem.arena_allocator(&alloc_parser)

	err_alloc_semantic := vmem.arena_init_growing(&alloc_semantic)
	ensure(err_alloc_semantic == nil)
	arena_semantic := vmem.arena_allocator(&alloc_semantic)

	ensure(len(os.args) == 2, "specify entry package directory as the first positional argument")
	entry_dir := os.args[1]

	// Parse entire project (all packages)
	asts, err_parse := parse_project(entry_dir, arena_lexer, arena_parser)
	ensure(err_parse == nil, fmt.tprint(err_parse))
	
	fmt.printfln("Parsed %d files", len(asts))

	packages, err_deps := build_dependency_graph(asts, context.temp_allocator)
	ensure(err_deps == nil, fmt.tprint(err_deps))

	sorted_packages, err_sort := topological_sort_packages(packages, context.temp_allocator)
	ensure(err_sort == nil, fmt.tprint(err_sort))

	fmt.printfln("Package processing order: %v", sorted_packages)


	// Multi-file semantic analysis
	entry_package_asts := make([dynamic]^Node, context.temp_allocator)
	for file_path, ast in asts {
		file_dir := filepath.dir(file_path, context.temp_allocator)
		if file_dir == entry_dir {
			append(&entry_package_asts, ast)
		}
	}

	ensure(len(entry_package_asts) > 0, "No files in entry package")
	root := entry_package_asts[0]
	ctx_sem := semantic_analyze_project(asts, sorted_packages, entry_dir, arena_semantic)


	if len(ctx_sem.errors) > 0 {
		// TODO: Need to map errors back to files
		for err in ctx_sem.errors {
			fmt.eprintfln("[%d:%d] %s", err.span.start, err.span.end, err.message)
		}
		os.exit(1)
	}

	fmt.println("Semantic analysis passed")

	c_code, _ := codegen(asts, sorted_packages, &ctx_sem, context.temp_allocator)

	fmt.println(c_code)
	
	output_file := "output.c"
	os.write_entire_file(output_file, transmute([]byte)c_code) 
	defer os.remove(output_file)
	fmt.printfln("Generated C code written to %s", output_file)

	state, stdout, stderr, err := os2.process_exec({
		command = []string{"clang", "-std=c11", "-O3", "-o", "output", output_file},
	}, context.temp_allocator)

	if err != nil {
		fmt.eprintfln("Compilation failed: %v", err)
		os.exit(1)
	}

	if state.exit_code == 0 {
		fmt.println("Compilation successful")
		fmt.println("Run: ./output")
	} else {
		fmt.eprintfln("Compilation failed with exit code %d", state.exit_code)
		if len(stderr) > 0 {
			fmt.eprintfln("Error output:\n%s", string(stderr))
		}
		os.exit(1)
	}
}