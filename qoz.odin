package main

import "core:path/filepath"
import "core:os/os2"
import "core:mem"
import vmem "core:mem/virtual"
import "core:fmt"
import "core:os"
import "core:strings"

Compiler_Command :: enum {
	Build,
	Run,
}

Compiler_Options :: struct {
	command:          Compiler_Command,
	entry_dir:        string,
	output_file:      string,
	optimization:     string,  // "0", "1", "2", "3"
	emit_c_only:      bool,
	verbose:          bool,
}

print_parse_error :: proc(err: Parse_Error) {
	fmt.eprintfln("%s:%d:%d: %s", err.file, err.line, err.column, err.message)
}

print_semantic_errors :: proc(errors: [dynamic]Semantic_Error, tokens_map: map[string][dynamic]Token) {
	for err in errors {
		if tokens, ok := tokens_map[err.file]; ok && err.span.start < len(tokens) {
			tok := tokens[err.span.start]
			fmt.eprintfln("%s:%d:%d: %s", err.file, tok.line, tok.column, err.message)
		} else {
			fmt.eprintfln("%s:[%d:%d] %s", err.file, err.span.start, err.span.end, err.message)
		}
	}
}

parse_cli_args :: proc() -> (Compiler_Options, bool) {
	opts := Compiler_Options{
		command = .Build,
		optimization = "3",
	}

	if len(os.args) < 2 {
		print_usage()
		return opts, false
	}

	i := 1
	
	// First argument: command (build or run) or help
	arg := os.args[i]
	if arg == "-h" || arg == "--help" {
		print_usage()
		return opts, false
	} else if arg == "build" {
		opts.command = .Build
		i += 1
	} else if arg == "run" {
		opts.command = .Run
		i += 1
	} else {
		fmt.eprintfln("Error: Command required. Use 'build' or 'run'")
		print_usage()
		return opts, false
	}
	
	// Parse remaining arguments
	for i < len(os.args) {
		arg := os.args[i]
		
		if arg == "-h" || arg == "--help" {
			print_usage()
			return opts, false
		} else if arg == "--emit-c" {
			opts.emit_c_only = true
		} else if arg == "--verbose" || arg == "-v" {
			opts.verbose = true
		} else if arg == "-o" || arg == "--output" {
			i += 1
			if i >= len(os.args) {
				fmt.eprintfln("Error: %s requires an argument", arg)
				return opts, false
			}
			opts.output_file = os.args[i]
		} else if strings.has_prefix(arg, "-O") {
			opt_level := arg[2:]
			if opt_level == "0" || opt_level == "1" || opt_level == "2" || opt_level == "3" {
				opts.optimization = opt_level
			} else {
				fmt.eprintfln("Error: Invalid optimization level '%s'. Use -O0, -O1, -O2, or -O3", arg)
				return opts, false
			}
		} else if !strings.has_prefix(arg, "-") {
			// Positional argument - entry directory
			opts.entry_dir = arg
		} else {
			fmt.eprintfln("Error: Unknown option '%s'", arg)
			print_usage()
			return opts, false
		}
		
		i += 1
	}

	if opts.entry_dir == "" {
		fmt.eprintfln("Error: No entry package directory specified")
		print_usage()
		return opts, false
	}

	// Set default output name to directory basename if not specified
	if opts.output_file == "" {
		abs_path, ok := filepath.abs(opts.entry_dir, context.temp_allocator)
		if !ok {
			fmt.eprintfln("Error: Failed to resolve entry directory path")
			return opts, false
		}
		opts.output_file = filepath.base(abs_path)
	}

	return opts, true
}

print_usage :: proc() {
	fmt.eprintln("Usage: qoz <command> [OPTIONS] <entry_package_dir>")
	fmt.eprintln()
	fmt.eprintln("Commands:")
	fmt.eprintln("  build                  Compile and create executable (default)")
	fmt.eprintln("  run                    Compile, run, then remove executable")
	fmt.eprintln()
	fmt.eprintln("Options:")
	fmt.eprintln("  -o, --output <file>    Set output executable name (default: directory basename)")
	fmt.eprintln("  -O0, -O1, -O2, -O3     Set optimization level (default: -O3)")
	fmt.eprintln("  --emit-c               Generate C code and stop (don't compile)")
	fmt.eprintln("  -v, --verbose          Print generated C code")
	fmt.eprintln("  -h, --help             Show this help message")
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

	// Parse command-line arguments
	opts, ok := parse_cli_args()
	if !ok {
		os.exit(1)
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

	// Parse entire project (all packages)
	asts, tokens_map, err_parse := parse_project(opts.entry_dir, arena_lexer, arena_parser)
	if err_parse != nil {
		print_parse_error(err_parse.(Parse_Error))
		os.exit(1)
	}
	
	packages, err_deps := build_dependency_graph(asts, opts.entry_dir, context.temp_allocator)
	if err_deps != nil {
		print_parse_error(err_deps.(Parse_Error))
		os.exit(1)
	}

	sorted_packages, err_sort := topological_sort_packages(packages, context.temp_allocator)
	if err_sort != nil {
		print_parse_error(err_sort.(Parse_Error))
		os.exit(1)
	}

	// Multi-file semantic analysis
	entry_package_asts := make([dynamic]^Node, context.temp_allocator)
	for file_path, ast in asts {
		file_dir := filepath.dir(file_path, context.temp_allocator)
		if file_dir == opts.entry_dir {
			append(&entry_package_asts, ast)
		}
	}

	ensure(len(entry_package_asts) > 0, "No files in entry package")
	ctx_sem := semantic_analyze_project(asts, sorted_packages, opts.entry_dir, arena_semantic)

	if len(ctx_sem.errors) > 0 {
		print_semantic_errors(ctx_sem.errors, tokens_map)
		os.exit(1)
	}

	if opts.verbose {
		fmt.println("Semantic analysis passed")
	}

	c_code, _ := codegen(asts, sorted_packages, &ctx_sem, context.temp_allocator)

	if opts.verbose || opts.emit_c_only {
		fmt.println(c_code)
	}
	
	output_c_file := fmt.tprintf("%s.c", opts.output_file)
	os.write_entire_file(output_c_file, transmute([]byte)c_code)
	
	if opts.verbose {
		fmt.printfln("Generated C code written to %s", output_c_file)
	}

	// If only emitting C, stop here
	if opts.emit_c_only {
		fmt.printfln("C code written to %s", output_c_file)
		return
	}
	
	defer os.remove(output_c_file)

	// Build compiler command with link flags
	opt_flag := fmt.tprintf("-O%s", opts.optimization)
	compiler_args := make([dynamic]string, context.temp_allocator)
	append(&compiler_args, "clang", "-std=c11", opt_flag, "-o", opts.output_file, output_c_file)
	
	// Add link directives as linker flags
	for link_directive in ctx_sem.link_directives {
		if strings.has_prefix(link_directive, "framework:") {
			// Framework: framework:Name -> -framework Name
			framework_name := strings.trim_prefix(link_directive, "framework:")
			append(&compiler_args, "-framework", framework_name)
		} else if strings.has_suffix(link_directive, ".a") {
			// Static library: just add the path as-is
			append(&compiler_args, link_directive)
		} else if strings.has_suffix(link_directive, ".dylib") {
			// Dynamic library: -L<dir> -lname
			dir := filepath.dir(link_directive, context.temp_allocator)
			base := filepath.base(link_directive)
			name := strings.trim_prefix(strings.trim_suffix(base, ".dylib"), "lib")
			append(&compiler_args, fmt.tprintf("-L%s", dir), fmt.tprintf("-l%s", name))
		} else {
			// Raw linker flag (like -lm, -pthread, etc.)
			append(&compiler_args, link_directive)
		}
	}
	
	if opts.verbose {
		fmt.printfln("Compiler command: %v", compiler_args[:])
	}
	
	state, stdout, stderr, err := os2.process_exec({
		command = compiler_args[:],
	}, context.temp_allocator)

	if err != nil {
		fmt.eprintfln("Compilation failed: %v", err)
		os.exit(1)
	}

	if state.exit_code == 0 {
		if opts.verbose {
			fmt.println("Compilation successful")
		}
		
		if opts.command == .Build {
			fmt.printfln("Compiled successfully: ./%s", opts.output_file)
		} else if opts.command == .Run {
			// Run the executable
			if opts.verbose {
				fmt.printfln("Running ./%s", opts.output_file)
			}
			
			run_state, run_stdout, run_stderr, run_err := os2.process_exec({
				command = []string{fmt.tprintf("./%s", opts.output_file)},
			}, context.temp_allocator)
			
			if run_err != nil {
				fmt.eprintfln("Failed to run executable: %v", run_err)
				os.remove(opts.output_file)
				os.exit(1)
			}
			
			// Print output from the program
			if len(run_stdout) > 0 {
				fmt.print(string(run_stdout))
			}
			if len(run_stderr) > 0 {
				fmt.eprint(string(run_stderr))
			}
			
			// Remove the executable after running
			os.remove(opts.output_file)
			
			// Exit with the same code as the program
			if run_state.exit_code != 0 {
				os.exit(run_state.exit_code)
			}
		}
	} else {
		fmt.eprintfln("Compilation failed with exit code %d", state.exit_code)
		if len(stderr) > 0 {
			fmt.eprintfln("Error output:\n%s", string(stderr))
		}
		os.exit(1)
	}
}