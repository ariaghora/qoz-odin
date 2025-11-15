package main

import "core:path/filepath"
import "core:fmt"
import "core:mem"
import "core:strings"

Codegen_Context :: struct {
    output_buf: strings.Builder,
    indent_level: int,
    ctx_sem: ^Semantic_Context,
    func_nesting_depth: int, // TODO(Aria): track depth for lambda lifting
    loop_counter: int,
    temp_counter: int, // Counter for generating unique temporary variable names
    in_for_header: bool, 
    current_pkg_name: string,
    skip_struct_defs: bool,
    generated_array_wrappers: map[string]bool, // Track which array wrapper structs we've generated
    generated_tuple_types: map[string]string, // Track tuple types: type signature -> typedef name
    current_function_locals: map[string]bool, // Track parameters and local variables in current function
    current_function_return_type: Maybe(Type_Info), // Track current function's return type for union wrapping
    defer_stack: [dynamic][dynamic]^Node, // Stack of defer lists per scope
}

MANGLE_PREFIX :: "qoz__"

should_mangle :: proc(name: string, is_external: bool) -> bool {
    if name == "main" do return false
    if is_external do return false
    return true
}

mangle_name :: proc(name: string) -> string {
    return fmt.tprintf("%s%s", MANGLE_PREFIX, name)
}

// Generate a unique name for array wrapper struct (handles nested arrays recursively)
get_array_wrapper_name :: proc(elem_type: ^Type_Info, size: int) -> string {
    type_name: string
    #partial switch t in elem_type^ {
    case Primitive_Type:
        #partial switch t {
        case .I8:  type_name = "i8"
        case .U8:  type_name = "u8"
        case .I32: type_name = "i32"
        case .I64: type_name = "i64"
        case .F32: type_name = "f32"
        case .F64: type_name = "f64"
        case: type_name = "unknown"
        }
    case Array_Type:
        // Nested array: recursively get inner array's wrapper name
        inner_wrapper := get_array_wrapper_name(t.element_type, t.size)
        // Strip the prefix to avoid double prefixing
        if strings.has_prefix(inner_wrapper, MANGLE_PREFIX) {
            type_name = inner_wrapper[len(MANGLE_PREFIX):]
        } else {
            type_name = inner_wrapper
        }
    case Pointer_Type:
        type_name = "ptr"
    case Named_Type:
        type_name, _ = strings.replace_all(t.name, ".", "_", context.temp_allocator)
    case:
        type_name = "unknown"
    }
    return fmt.tprintf("%sArray_%s_%d", MANGLE_PREFIX, type_name, size)
}

// Generate a unique signature string for tuple type
get_tuple_signature :: proc(tuple_type: Tuple_Type) -> string {
    parts := make([dynamic]string, context.temp_allocator)
    append(&parts, "tuple")
    for tuple_elem in tuple_type.types {
        #partial switch t in tuple_elem {
        case Primitive_Type:
            #partial switch t {
            case .I8:  append(&parts, "i8")
            case .U8:  append(&parts, "u8")
            case .I32: append(&parts, "i32")
            case .I64: append(&parts, "i64")
            case .F32: append(&parts, "f32")
            case .F64: append(&parts, "f64")
            case .Bool: append(&parts, "bool")
            case .Cstring: append(&parts, "cstring")
            case: append(&parts, "unknown")
            }
        case Named_Type:
            // Use the name, but replace dots with underscores
            name, _ := strings.replace_all(t.name, ".", "_", context.temp_allocator)
            append(&parts, name)
        case: append(&parts, "unknown")
        }
    }
    return strings.join(parts[:], "_", context.temp_allocator)
}

// Generate typedef for tuple type
codegen_tuple_typedef :: proc(ctx_cg: ^Codegen_Context, tuple_type: Tuple_Type) -> string {
    signature := get_tuple_signature(tuple_type)
    
    // Check if we've already generated this tuple type
    if typedef_name, exists := ctx_cg.generated_tuple_types[signature]; exists {
        return typedef_name
    }
    
    // Generate a new typedef name
    typedef_name := fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, signature)
    ctx_cg.generated_tuple_types[signature] = typedef_name
    
    // Generate the typedef
    strings.write_string(&ctx_cg.output_buf, "typedef struct {")
    for tuple_elem, i in tuple_type.types {
        if i > 0 {
            strings.write_string(&ctx_cg.output_buf, " ")
        }
        codegen_type(ctx_cg, tuple_elem)
        fmt.sbprintf(&ctx_cg.output_buf, " field_%d;", i)
    }
    strings.write_string(&ctx_cg.output_buf, "} ")
    strings.write_string(&ctx_cg.output_buf, typedef_name)
    strings.write_string(&ctx_cg.output_buf, ";\n")
    
    return typedef_name
}

// Generate a unique name for vec wrapper struct
get_vec_wrapper_name :: proc(elem_type: ^Type_Info) -> string {
    type_name: string
    #partial switch t in elem_type^ {
    case Primitive_Type:
        #partial switch t {
        case .I8:  type_name = "i8"
        case .U8:  type_name = "u8"
        case .I32: type_name = "i32"
        case .I64: type_name = "i64"
        case .F32: type_name = "f32"
        case .F64: type_name = "f64"
        case: type_name = "unknown"
        }
    case Pointer_Type:
        type_name = "ptr"
    case Named_Type:
        type_name, _ = strings.replace_all(t.name, ".", "_", context.temp_allocator)
    case:
        type_name = "unknown"
    }
    return fmt.tprintf("%sVec_%s", MANGLE_PREFIX, type_name)
}

// Check if a type contains any named struct types
type_contains_struct :: proc(t: Type_Info) -> bool {
    #partial switch typ in t {
    case Named_Type:
        return true // Could be a struct
    case Array_Type:
        return type_contains_struct(typ.element_type^)
    case Pointer_Type:
        return type_contains_struct(typ.pointee^)
    case:
        return false
    }
}

// Generate struct wrapper for array type (needed for returning arrays by value in C)
// Recursively generates wrappers for nested arrays
// only_primitives: if true, only generate wrappers for arrays of primitives (skip struct-based)
codegen_array_wrapper_struct :: proc(ctx_cg: ^Codegen_Context, arr_type: Array_Type, only_primitives := false) {
    // If element type is also an array, generate its wrapper first
    if nested_arr, is_nested := arr_type.element_type^.(Array_Type); is_nested {
        codegen_array_wrapper_struct(ctx_cg, nested_arr, only_primitives)
    }
    
    wrapper_name := get_array_wrapper_name(arr_type.element_type, arr_type.size)
    
    // Check if we already generated this wrapper
    if wrapper_name in ctx_cg.generated_array_wrappers {
        return
    }
    
    // Skip struct-based arrays if only_primitives is true
    if only_primitives && type_contains_struct(arr_type.element_type^) {
        return
    }
    
    ctx_cg.generated_array_wrappers[wrapper_name] = true
    
    strings.write_string(&ctx_cg.output_buf, "typedef struct ")
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, " {\n    ")
    codegen_type(ctx_cg, arr_type.element_type^)
    fmt.sbprintf(&ctx_cg.output_buf, " data[%d];\n} ", arr_type.size)
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, ";\n\n")
}

// Generate struct wrapper for vec type
// only_primitives: if true, skip generation (vec always depends on Allocator struct)
codegen_vec_wrapper_struct :: proc(ctx_cg: ^Codegen_Context, vec_type: Vec_Type, only_primitives := false) {
    wrapper_name := get_vec_wrapper_name(vec_type.element_type)
    
    // Check if we already generated this wrapper
    if wrapper_name in ctx_cg.generated_array_wrappers {  // Reuse same tracking map
        return
    }
    
    // Skip vec wrappers if only_primitives is true
    // Vec always depends on mem.Allocator struct, so must wait for Pass 2.5
    if only_primitives {
        return
    }
    
    ctx_cg.generated_array_wrappers[wrapper_name] = true
    
    // Note: mem.Allocator struct should already be defined by the struct pass
    // We only generate vec wrappers in Pass 2.5, after all structs are defined
    
    // typedef struct qoz__Vec_i32 {
    //     i32* data;
    //     int64_t len;
    //     int64_t cap;
    //     Qoz_Allocator allocator;
    // } qoz__Vec_i32;
    
    strings.write_string(&ctx_cg.output_buf, "typedef struct ")
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, " {\n    ")
    codegen_type(ctx_cg, vec_type.element_type^)
    strings.write_string(&ctx_cg.output_buf, "* data;\n    int64_t len;\n    int64_t cap;\n    ")
    // Use the actual Qoz mem.Allocator type (mangled)
    strings.write_string(&ctx_cg.output_buf, "qoz__mem__Allocator allocator;\n} ")
    strings.write_string(&ctx_cg.output_buf, wrapper_name)
    strings.write_string(&ctx_cg.output_buf, ";\n\n")
}

// Generate element-wise unary operation for arrays
codegen_array_unop :: proc(ctx_cg: ^Codegen_Context, unop: Node_Un_Op, arr_type: Array_Type) {
    // Generate: (ArrayType){{op operand.data[0], op operand.data[1], ...}}
    
    strings.write_string(&ctx_cg.output_buf, "(")
    codegen_type(ctx_cg, arr_type)
    strings.write_string(&ctx_cg.output_buf, "){{")
    
    for i in 0..<arr_type.size {
        if i > 0 {
            strings.write_string(&ctx_cg.output_buf, ", ")
        }
        strings.write_string(&ctx_cg.output_buf, "(")
        strings.write_string(&ctx_cg.output_buf, unop.op.source)
        codegen_node(ctx_cg, unop.operand)
        fmt.sbprintf(&ctx_cg.output_buf, ".data[%d]", i)
        strings.write_string(&ctx_cg.output_buf, ")")
    }
    strings.write_string(&ctx_cg.output_buf, "}}")
}

// Generate element-wise binary operation for arrays
codegen_array_binop :: proc(ctx_cg: ^Codegen_Context, binop: Node_Bin_Op, arr_type: Array_Type) {
    // Generate: (ArrayType){{left.data[0] op right.data[0], left.data[1] op right.data[1], ...}}
    
    strings.write_string(&ctx_cg.output_buf, "(")
    codegen_type(ctx_cg, arr_type)
    strings.write_string(&ctx_cg.output_buf, "){{")
    
    for i in 0..<arr_type.size {
        if i > 0 {
            strings.write_string(&ctx_cg.output_buf, ", ")
        }
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, binop.left)
        fmt.sbprintf(&ctx_cg.output_buf, ".data[%d]", i)
        strings.write_string(&ctx_cg.output_buf, binop.op.source)
        codegen_node(ctx_cg, binop.right)
        fmt.sbprintf(&ctx_cg.output_buf, ".data[%d])", i)
    }
    
    strings.write_string(&ctx_cg.output_buf, "}}")
}

// Generate element-wise operation for array-scalar or scalar-array
codegen_array_scalar_binop :: proc(ctx_cg: ^Codegen_Context, binop: Node_Bin_Op, arr_type: Array_Type, scalar_first: bool) {
    // Generate: (ArrayType){{arr.data[0] op scalar, arr.data[1] op scalar, ...}}
    // or:       (ArrayType){{scalar op arr.data[0], scalar op arr.data[1], ...}}
    
    strings.write_string(&ctx_cg.output_buf, "(")
    codegen_type(ctx_cg, arr_type)
    strings.write_string(&ctx_cg.output_buf, "){{")
    
    for i in 0..<arr_type.size {
        if i > 0 {
            strings.write_string(&ctx_cg.output_buf, ", ")
        }
        strings.write_string(&ctx_cg.output_buf, "(")
        
        if scalar_first {
            // scalar op array[i]
            codegen_node(ctx_cg, binop.left)
            strings.write_string(&ctx_cg.output_buf, binop.op.source)
            codegen_node(ctx_cg, binop.right)
            fmt.sbprintf(&ctx_cg.output_buf, ".data[%d])", i)
        } else {
            // array[i] op scalar
            codegen_node(ctx_cg, binop.left)
            fmt.sbprintf(&ctx_cg.output_buf, ".data[%d]", i)
            strings.write_string(&ctx_cg.output_buf, binop.op.source)
            codegen_node(ctx_cg, binop.right)
            strings.write_string(&ctx_cg.output_buf, ")")
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "}}")
}

// Generate element-wise comparison for arrays
codegen_array_comparison :: proc(ctx_cg: ^Codegen_Context, binop: Node_Bin_Op, arr_type: Array_Type) {
    // Generate: (left.data[0] == right.data[0] && left.data[1] == right.data[1] && ...)
    // For !=, we use || instead of &&
    
    use_and := binop.op.kind == .Eq_Eq
    connector := use_and ? "&&" : "||"
    
    strings.write_string(&ctx_cg.output_buf, "(")
    
    for i in 0..<arr_type.size {
        if i > 0 {
            strings.write_string(&ctx_cg.output_buf, " ")
            strings.write_string(&ctx_cg.output_buf, connector)
            strings.write_string(&ctx_cg.output_buf, " ")
        }
        
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, binop.left)
        fmt.sbprintf(&ctx_cg.output_buf, ".data[%d]", i)
        strings.write_string(&ctx_cg.output_buf, binop.op.source)
        codegen_node(ctx_cg, binop.right)
        fmt.sbprintf(&ctx_cg.output_buf, ".data[%d])", i)
    }
    
    strings.write_string(&ctx_cg.output_buf, ")")
}

// Returns true if the built-in was handled, false otherwise
codegen_builtin_function :: proc(ctx_cg: ^Codegen_Context, name: string, args: []^Node) -> bool {
    switch name {
    case "len":
        if len(args) != 1 do return false
        
        arg := args[0]
        arg_type := arg.inferred_type.? or_else panic("Type not inferred for len() argument")
        
        #partial switch t in arg_type {
        case Array_Type:
            // Array length is compile-time constant
            fmt.sbprintf(&ctx_cg.output_buf, "%d", t.size)
        case Vec_Type:
            // Vec length is runtime field access
            strings.write_string(&ctx_cg.output_buf, "(")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ").len")
        case Pointer_Type:
            // Check if pointer to vec
            if _, is_vec := t.pointee^.(Vec_Type); is_vec {
                // Pointer to vec: use ->len
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, arg)
                strings.write_string(&ctx_cg.output_buf, ")->len")
            } else {
                panic("len() called on non-vec pointer")
            }
        case Named_Type:
            if t.name == "string" {
                // String length is runtime field access
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, arg)
                strings.write_string(&ctx_cg.output_buf, ").len")
            } else {
                panic("len() called on invalid type in codegen")
            }
        case:
            panic("len() called on invalid type in codegen")
        }
        return true
    
    case "append":
        // append(vec: *vec<T>, item: T)
        if len(args) != 2 do return false
        
        vec_ptr := args[0]
        item := args[1]
        
        vec_ptr_type := vec_ptr.inferred_type.? or_else Primitive_Type.Void
        ptr_type, is_ptr := vec_ptr_type.(Pointer_Type)
        if !is_ptr do return false
        
        vec_type, is_vec := ptr_type.pointee^.(Vec_Type)
        if !is_vec do return false
        
        // Generate append code with growth logic
        // if ((*v)->len >= (*v)->cap) {
        //     int64_t new_cap = (*v)->cap == 0 ? 8 : (*v)->cap * 2;
        //     T* new_data = (T*)(*v)->allocator.alloc((*v)->allocator.data, new_cap * sizeof(T));
        //     if ((*v)->data) memcpy(new_data, (*v)->data, (*v)->len * sizeof(T));
        //     if ((*v)->data) (*v)->allocator.free((*v)->allocator.data, (*v)->data);
        //     (*v)->data = new_data;
        //     (*v)->cap = new_cap;
        // }
        // (*v)->data[(*v)->len++] = item;
        
        strings.write_string(&ctx_cg.output_buf, "{ if ((*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").len >= (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").cap) { int64_t _new_cap = (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").cap == 0 ? 8 : (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").cap * 2; ")
        
        codegen_type(ctx_cg, vec_type.element_type^)
        strings.write_string(&ctx_cg.output_buf, "* _new_data = (")
        codegen_type(ctx_cg, vec_type.element_type^)
        strings.write_string(&ctx_cg.output_buf, "*)(*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").allocator.alloc((*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").allocator.data, _new_cap * sizeof(")
        codegen_type(ctx_cg, vec_type.element_type^)
        strings.write_string(&ctx_cg.output_buf, ")); if ((*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").data) { for (int64_t _i = 0; _i < (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").len; _i++) _new_data[_i] = (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").data[_i]; (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").allocator.free((*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").allocator.data, (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").data); } (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").data = _new_data; (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").cap = _new_cap; } (*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").data[(*")
        codegen_node(ctx_cg, vec_ptr)
        strings.write_string(&ctx_cg.output_buf, ").len++] = ")
        codegen_node(ctx_cg, item)
        strings.write_string(&ctx_cg.output_buf, "; }")
        
        return true
    
    case "format":
        // format(fmt: string literal, args: ..., alloc: mem.Allocator): string
        if len(args) < 2 do return false
        
        // First arg must be string literal
        if args[0].node_kind != .Literal_String do return false
        
        fmt_lit := args[0].payload.(Node_Literal_String)
        fmt_str := strings.trim(fmt_lit.content.source, "\"")
        
        // Last arg is allocator
        alloc_arg := args[len(args) - 1]
        format_args := args[1:len(args) - 1]
        
        // Generate: ({ int _size = snprintf(NULL, 0, fmt, ...); char* _buf = alloc.alloc(alloc.data, _size + 1); snprintf(_buf, _size + 1, fmt, ...); (Qoz_String){.data = _buf, .len = _size}; })
        
        decoded_fmt := decode_string_literal_bytes(strings.trim(fmt_lit.content.source, "\""), ctx_cg.ctx_sem.allocator)
         
        strings.write_string(&ctx_cg.output_buf, "({ int _fmt_size = snprintf(NULL, 0, ")
        emit_c_string_literal(ctx_cg, decoded_fmt)
         
        // Emit format arguments
        for arg in format_args {
            strings.write_string(&ctx_cg.output_buf, ", ")
            codegen_node(ctx_cg, arg)
        }
        strings.write_string(&ctx_cg.output_buf, "); char* _fmt_buf = (char*)")
        codegen_node(ctx_cg, alloc_arg)
        strings.write_string(&ctx_cg.output_buf, ".alloc(")
        codegen_node(ctx_cg, alloc_arg)
        strings.write_string(&ctx_cg.output_buf, ".data, _fmt_size + 1); snprintf(_fmt_buf, _fmt_size + 1, ")
        emit_c_string_literal(ctx_cg, decoded_fmt)
         
        // Emit format arguments again
        for arg in format_args {
            strings.write_string(&ctx_cg.output_buf, ", ")
            codegen_node(ctx_cg, arg)
        }
        strings.write_string(&ctx_cg.output_buf, "); (Qoz_String){.data = (const char*)_fmt_buf, .len = _fmt_size}; })")
        
        return true
    
    // TODO(Aria): Add more built-ins here:
    // case "size_of":
    //     ...
    //     return true
    // case "typeof":
    //     ...
    //     return true
    
    case:
        return false
    }
}

codegen :: proc(asts: map[string]^Node, sorted_packages: []string, ctx_sem: ^Semantic_Context, allocator := context.allocator) -> (res: string, err: mem.Allocator_Error) {
    sb := strings.builder_make(allocator) or_return
    ctx_cg := Codegen_Context { 
        output_buf = sb, 
        indent_level = 0, 
        ctx_sem = ctx_sem,
        temp_counter = 0,
        generated_array_wrappers = make(map[string]bool, allocator=allocator),
        generated_tuple_types = make(map[string]string, allocator=allocator),
    }

    strings.write_string(&ctx_cg.output_buf, string(#load("qoz_runtime.c")))
    strings.write_string(&ctx_cg.output_buf, "\n\n")
    strings.write_string(&ctx_cg.output_buf, "#include <stdio.h>\n")
    strings.write_string(&ctx_cg.output_buf, "#include <stdlib.h>\n")
    strings.write_string(&ctx_cg.output_buf, "#include <string.h>\n")

    // Pass 1: Struct forward declarations for ALL packages
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_struct_forward_decl(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 1.5: Collect and forward-declare all array wrappers
    array_wrapper_names := make([dynamic]string, context.temp_allocator)
    forward_decl_map := make(map[string]bool, context.temp_allocator)
    ctx_cg.generated_array_wrappers = forward_decl_map
    for pkg_dir in sorted_packages {
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_collect_array_wrappers(&ctx_cg, ast, &array_wrapper_names)
            }
        }
    }
    // Forward declare array wrappers
    for name in array_wrapper_names {
        fmt.sbprintf(&ctx_cg.output_buf, "typedef struct %s %s;\n", name, name)
    }
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Reset the map for actual generation
    ctx_cg.generated_array_wrappers = make(map[string]bool, allocator)
    
    // Pass 1.75: Generate array wrapper bodies for primitive types ONLY
    // (These are needed by struct fields before struct definitions)
    for pkg_dir in sorted_packages {
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_scan_array_returns(&ctx_cg, ast, only_primitives = true)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 2: Struct definitions for ALL packages (complete typedefs with bodies)
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_struct_defs(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 2.5: Generate array wrapper bodies for struct-based arrays
    // (These can now reference fully defined struct types)
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_scan_array_returns(&ctx_cg, ast, only_primitives = false)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 3: Function forward declarations for ALL packages
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_forward_decl(&ctx_cg, ast)
            }
        }
    }
    
    strings.write_string(&ctx_cg.output_buf, "\n")
    
    // Pass 4: Function implementations for ALL packages (skip struct defs)
    ctx_cg.skip_struct_defs = true
    for pkg_dir in sorted_packages {
        ctx_cg.current_pkg_name = filepath.base(pkg_dir)
        for file_path, ast in asts {
            file_dir := filepath.dir(file_path, context.temp_allocator)
            if file_dir == pkg_dir {
                codegen_node(&ctx_cg, ast)
            }
        }
    }

    return strings.to_string(ctx_cg.output_buf), nil
}

indented :: proc(content: string, n_space: int) -> string {
    return fmt.tprintf("%s%s", strings.repeat(" ", n_space), content)
}

codegen_indent :: proc(ctx_cg: ^Codegen_Context, level: int) {
    strings.write_string(&ctx_cg.output_buf, strings.repeat(" ", ctx_cg.indent_level * 4, context.temp_allocator))
}

// =============================================================================
// CENTRALIZED COMPOSITE TYPE HANDLING
// =============================================================================

// Generate forward declaration for any composite type (struct, union, etc.)
codegen_composite_forward_decl :: proc(ctx_cg: ^Codegen_Context, type_info: Type_Info, name: string) {
    #partial switch t in type_info {
    case Struct_Type, Union_Type:
        strings.write_string(&ctx_cg.output_buf, "typedef struct ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, " ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ";\n")
    }
}

// Generate full definition for any composite type (struct, union, etc.)
codegen_composite_definition :: proc(ctx_cg: ^Codegen_Context, type_info: Type_Info, name: string) {
    #partial switch t in type_info {
    case Struct_Type:
        strings.write_string(&ctx_cg.output_buf, "typedef struct ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name) 
        strings.write_string(&ctx_cg.output_buf, " {\n")
        ctx_cg.indent_level += 1
        
        for field in t.fields {
            codegen_indent(ctx_cg, ctx_cg.indent_level)

            // Resolve the field type to handle type aliases
            resolved_field_type := field.type
            if named_type, is_named := field.type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    resolved_field_type = resolved
                }
            }

            if _, is_fn := resolved_field_type.(Function_Type); is_fn {
                codegen_type(ctx_cg, field.type, field.name)
            } else {
                codegen_type(ctx_cg, field.type)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, field.name)
            }

            strings.write_string(&ctx_cg.output_buf, ";\n")
        }
        
        ctx_cg.indent_level -= 1
        strings.write_string(&ctx_cg.output_buf, "} ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ";\n\n")

    case Union_Type:
        // Generate C tagged union: struct with tag + union data
        strings.write_string(&ctx_cg.output_buf, "typedef struct ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name) 
        strings.write_string(&ctx_cg.output_buf, " {\n")
        ctx_cg.indent_level += 1
        
        // Generate tag field
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "int tag;\n")
        
        // Generate union data field
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "union {\n")
        ctx_cg.indent_level += 1
        
        for union_type, i in t.types {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            
            // Resolve the type to handle type aliases
            resolved_type := union_type
            if named_type, is_named := union_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    resolved_type = resolved
                }
            }
            
            // Generate field name based on type index
            field_name := fmt.tprintf("variant_%d", i)
            
            if _, is_fn := resolved_type.(Function_Type); is_fn {
                codegen_type(ctx_cg, union_type, field_name)
            } else {
                codegen_type(ctx_cg, union_type)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, field_name)
            }
            
            strings.write_string(&ctx_cg.output_buf, ";\n")
        }
        
        ctx_cg.indent_level -= 1
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "} data;\n")
        
        ctx_cg.indent_level -= 1
        strings.write_string(&ctx_cg.output_buf, "} ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ";\n\n")
    }
}

// Generate type alias for any composite type (struct, union, etc.)
codegen_composite_alias :: proc(ctx_cg: ^Codegen_Context, type_info: Type_Info, name: string) {
    #partial switch t in type_info {
    case Struct_Type, Union_Type:
        strings.write_string(&ctx_cg.output_buf, "typedef struct ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, " ")
        strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
        strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
        strings.write_string(&ctx_cg.output_buf, "__")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ";\n")
    }
}

// Check if a type is a composite type (struct, union, future enums, etc.)
is_composite_type :: proc(type_info: Type_Info) -> bool {
    #partial switch t in type_info {
    case Struct_Type, Union_Type, Tuple_Type:
        return true
    }
    return false
}

// Recursively scan composite types for processing
codegen_scan_composite_types :: proc(ctx_cg: ^Codegen_Context, type_info: Type_Info, only_primitives := false) {
    #partial switch t in type_info {
    case Array_Type:
        codegen_array_wrapper_struct(ctx_cg, t, only_primitives)
        codegen_scan_composite_types(ctx_cg, t.element_type^, only_primitives)
    case Vec_Type:
        codegen_vec_wrapper_struct(ctx_cg, t, only_primitives)
        codegen_scan_composite_types(ctx_cg, t.element_type^, only_primitives)
    case Pointer_Type:
        codegen_scan_composite_types(ctx_cg, t.pointee^, only_primitives)
    case Struct_Type:
        for field in t.fields {
            codegen_scan_composite_types(ctx_cg, field.type, only_primitives)
        }
    case Union_Type:
        for union_type in t.types {
            codegen_scan_composite_types(ctx_cg, union_type, only_primitives)
        }
    case Function_Type:
        codegen_scan_composite_types(ctx_cg, t.return_type^, only_primitives)
        for param in t.params {
            codegen_scan_composite_types(ctx_cg, param, only_primitives)
        }
    }
}

// =============================================================================

// Recursively scan a type for arrays and collect wrapper names (no generation)
codegen_collect_type_array_names :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, names: ^[dynamic]string) {
    #partial switch t in type {
    case Array_Type:
        wrapper_name := get_array_wrapper_name(t.element_type, t.size)
        if wrapper_name not_in ctx_cg.generated_array_wrappers {
            append(names, wrapper_name)
            ctx_cg.generated_array_wrappers[wrapper_name] = true
        }
        // Recurse for nested arrays
        codegen_collect_type_array_names(ctx_cg, t.element_type^, names)
    case Vec_Type:
        wrapper_name := get_vec_wrapper_name(t.element_type)
        if wrapper_name not_in ctx_cg.generated_array_wrappers {
            append(names, wrapper_name)
            ctx_cg.generated_array_wrappers[wrapper_name] = true
        }
        codegen_collect_type_array_names(ctx_cg, t.element_type^, names)
    case Pointer_Type:
        codegen_collect_type_array_names(ctx_cg, t.pointee^, names)
    case Struct_Type:
        for field in t.fields {
            codegen_collect_type_array_names(ctx_cg, field.type, names)
        }
    case Union_Type:
        for union_type in t.types {
            codegen_collect_type_array_names(ctx_cg, union_type, names)
        }
    case Function_Type:
        codegen_collect_type_array_names(ctx_cg, t.return_type^, names)
        for param in t.params {
            codegen_collect_type_array_names(ctx_cg, param, names)
        }
    }
}

// Collect all array wrapper names without generating
codegen_collect_array_wrappers :: proc(ctx_cg: ^Codegen_Context, node: ^Node, names: ^[dynamic]string) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_collect_array_wrappers(ctx_cg, stmt, names)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        if var_def.content.node_kind == .Fn_Def {
            fn_def := var_def.content.payload.(Node_Fn_Def)
            codegen_collect_type_array_names(ctx_cg, fn_def.return_type, names)
            // Scan function body for local variables with array types
            for stmt in fn_def.body {
                codegen_collect_array_wrappers(ctx_cg, stmt, names)
            }
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct definition
            type_expr := var_def.content.payload.(Node_Type_Expr)
            codegen_collect_type_array_names(ctx_cg, type_expr.type_info, names)
        } else {
            // Local variable - check if it has an explicit array type
            if explicit_type, has_explicit := var_def.explicit_type.?; has_explicit {
                codegen_collect_type_array_names(ctx_cg, explicit_type, names)
            }
            // Also check the inferred type from the initialization expression
            if inferred_type, has_inferred := var_def.content.inferred_type.?; has_inferred {
                codegen_collect_type_array_names(ctx_cg, inferred_type, names)
            }
        }
    }
}

// Recursively scan a type for arrays and vecs and generate wrappers
codegen_scan_type_for_arrays :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, only_primitives := false) {
    #partial switch t in type {
    case Array_Type:
        codegen_array_wrapper_struct(ctx_cg, t, only_primitives)
    case Vec_Type:
        codegen_vec_wrapper_struct(ctx_cg, t, only_primitives)
    case Pointer_Type:
        codegen_scan_type_for_arrays(ctx_cg, t.pointee^, only_primitives)
    case Struct_Type:
        for field in t.fields {
            codegen_scan_type_for_arrays(ctx_cg, field.type, only_primitives)
        }
    case Union_Type:
        for union_type in t.types {
            codegen_scan_type_for_arrays(ctx_cg, union_type, only_primitives)
        }
    case Function_Type:
        codegen_scan_type_for_arrays(ctx_cg, t.return_type^, only_primitives)
        for param in t.params {
            codegen_scan_type_for_arrays(ctx_cg, param, only_primitives)
        }
    }
}

// Scan AST for functions that return arrays and generate wrapper structs
codegen_scan_array_returns :: proc(ctx_cg: ^Codegen_Context, node: ^Node, only_primitives := false) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_scan_array_returns(ctx_cg, stmt, only_primitives)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        if var_def.content.node_kind == .Fn_Def {
            fn_def := var_def.content.payload.(Node_Fn_Def)
            codegen_scan_type_for_arrays(ctx_cg, fn_def.return_type, only_primitives)
            // Scan function body for local variables with array types
            for stmt in fn_def.body {
                codegen_scan_array_returns(ctx_cg, stmt, only_primitives)
            }
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct definition
            type_expr := var_def.content.payload.(Node_Type_Expr)
            codegen_scan_type_for_arrays(ctx_cg, type_expr.type_info, only_primitives)
        } else {
            // Local variable - scan both explicit and inferred types
            if explicit_type, has_explicit := var_def.explicit_type.?; has_explicit {
                codegen_scan_type_for_arrays(ctx_cg, explicit_type, only_primitives)
            }
            if inferred_type, has_inferred := var_def.content.inferred_type.?; has_inferred {
                codegen_scan_type_for_arrays(ctx_cg, inferred_type, only_primitives)
            }
        }
    }
}

// Walk AST and generate only struct forward declarations (typedef struct X X;)
codegen_struct_forward_decl :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_struct_forward_decl(ctx_cg, stmt)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        
        if var_def.content.node_kind == .Type_Expr {
            type_expr := var_def.content.payload.(Node_Type_Expr)
            
            if is_composite_type(type_expr.type_info) {
                if len(var_def.names) > 0 {
                    codegen_composite_forward_decl(ctx_cg, type_expr.type_info, var_def.names[0])
                }
            }
        }
    }
}

// Walk AST and generate only struct type definitions
codegen_struct_defs :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_struct_defs(ctx_cg, stmt)
        }
    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)
        
        if var_def.content.node_kind == .Type_Expr {
            type_expr := var_def.content.payload.(Node_Type_Expr)
                
            if is_composite_type(type_expr.type_info) {
                if len(var_def.names) > 0 {
                    codegen_composite_definition(ctx_cg, type_expr.type_info, var_def.names[0])
                }
            }
        }
    }
}

codegen_emit_defers :: proc(ctx_cg: ^Codegen_Context) {
    if len(ctx_cg.defer_stack) == 0 do return
    
    defer_list := ctx_cg.defer_stack[len(ctx_cg.defer_stack) - 1]
    
    // Execute defers in reverse order (LIFO)
    for i := len(defer_list) - 1; i >= 0; i -= 1 {
        defer_node := defer_list[i].payload.(Node_Defer)
        for stmt in defer_node.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
    }
}

// Helper to check if a type is "simple" (can use printf format specifier)
is_simple_print_type :: proc(type_info: Type_Info, ctx_sem: ^Semantic_Context, pkg_name: string) -> bool {
    #partial switch t in type_info {
    case Primitive_Type, Untyped_Int, Untyped_Float, Untyped_String, Pointer_Type:
        return true
    case Named_Type:
        if t.name == "string" {
            return true
        }
        // Resolve to check if it's a struct/array/vec
        if resolved, ok := resolve_named_type(ctx_sem, t, pkg_name); ok {
            return is_simple_print_type(resolved, ctx_sem, pkg_name)
        }
        return false
    case:
        return false
    }
}

// Generate code to print a value from a C expression string (for array/struct elements)
codegen_print_value_from_expr :: proc(ctx_cg: ^Codegen_Context, base_expr: string, index: int, type_info: Type_Info, is_first: bool, is_last: bool) {
    // Build the full expression
    expr := base_expr
    if index >= 0 {
        expr = fmt.tprintf("%s[%d]", base_expr, index)
    } else {
        // Use base_expr as-is (may already contain [_i] for vecs)
        expr = base_expr
    }
    
    // Resolve named types first
    resolved_type := type_info
    if named, is_named := type_info.(Named_Type); is_named {
        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
            resolved_type = resolved
        }
    }
    
    #partial switch t in resolved_type {
    case Primitive_Type:
        switch t {
        case .I8, .U8, .I32, .I64:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%d\", (int)%s);\n", expr)
        case .F32, .F64:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%f\", (double)%s);\n", expr)
        case .Bool:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%s\", %s ? \"true\" : \"false\");\n", expr)
        case .Cstring:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%s\", %s);\n", expr)
        case .Void:
            panic("Cannot print void")
        }
    
    case Untyped_Int:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%d\", (int)%s);\n", expr)
    
    case Untyped_Float:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%f\", (double)%s);\n", expr)
    
    case Untyped_String:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%.*s\", (int)(%s).len, (%s).data);\n", expr, expr)
    
    case Named_Type:
        if t.name == "string" {
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%.*s\", (int)(%s).len, (%s).data);\n", expr, expr)
        } else {
            // Recursively resolve and print
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, t, ctx_cg.current_pkg_name); ok {
                codegen_print_value_from_expr(ctx_cg, expr, -1, resolved, is_first, is_last)
            } else {
                panic(fmt.tprintf("Cannot resolve type: %s", t.name))
            }
        }
    
    case Pointer_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%%p\", (void*)%s);\n", expr)
    
    case Array_Type:
        // Recursively print array element
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"[\");\n")
        for i in 0..<t.size {
            if i > 0 {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
                strings.write_string(&ctx_cg.output_buf, "printf(\", \");\n")
            }
            elem_expr := fmt.tprintf("%s.data[%d]", expr, i)
            codegen_print_value_from_expr(ctx_cg, elem_expr, -1, t.element_type^, true, true)
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"]\");\n")
    
    case Vec_Type:
        // Recursively print vec element
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"[\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
        fmt.sbprintf(&ctx_cg.output_buf, "typeof(%s) _vec_elem = %s;\n", expr, expr)
        codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
        strings.write_string(&ctx_cg.output_buf, "for (int _j = 0; _j < _vec_elem.len; _j++) {\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 4)
        strings.write_string(&ctx_cg.output_buf, "if (_j > 0) printf(\", \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 4)
        elem_expr := fmt.tprintf("_vec_elem.data[_j]")
        codegen_print_value_from_expr(ctx_cg, elem_expr, -1, t.element_type^, true, true)
        codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
        strings.write_string(&ctx_cg.output_buf, "}\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "}\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"]\");\n")
    
    case Struct_Type:
        // Recursively print struct fields
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"{\");\n")
        for field, i in t.fields {
            if i > 0 {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
                strings.write_string(&ctx_cg.output_buf, "printf(\", \");\n")
            }
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%s: \");\n", field.name)
            field_expr := fmt.tprintf("%s.%s", expr, field.name)
            codegen_print_value_from_expr(ctx_cg, field_expr, -1, field.type, true, true)
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "printf(\"}\");\n")
    
    case Union_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "switch (")
        strings.write_string(&ctx_cg.output_buf, expr)
        strings.write_string(&ctx_cg.output_buf, ".tag) {\n")
        for variant_type, i in t.types {
            codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
            fmt.sbprintf(&ctx_cg.output_buf, "case %d:\n", i)
            codegen_indent(ctx_cg, ctx_cg.indent_level + 4)
            variant_expr := fmt.tprintf("%s.data.variant_%d", expr, i)
            codegen_print_value_from_expr(ctx_cg, variant_expr, -1, variant_type, true, true)
            codegen_indent(ctx_cg, ctx_cg.indent_level + 4)
            strings.write_string(&ctx_cg.output_buf, "break;\n")
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case:
        panic(fmt.tprintf("Cannot print type: %v", type_info))
    }
}

// Generate code to print a value of any type recursively, with original Named_Type preserved
codegen_print_value_with_named_type :: proc(ctx_cg: ^Codegen_Context, arg: ^Node, resolved_type: Type_Info, original_named: Named_Type, is_first: bool, is_last: bool) {
    #partial switch t in resolved_type {
    case Struct_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"{\");\n")
        // Generate temporary to avoid evaluating the expression multiple times
        temp_name := fmt.tprintf("_struct_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        // Use the original Named_Type to get the mangled name
        struct_type_name := original_named.name
        // Qualify if needed
        if !strings.contains(struct_type_name, ".") && ctx_cg.current_pkg_name != "" {
            struct_type_name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, struct_type_name)
        }
        // Get mangled name
        mangled_name := fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, struct_type_name)
        if strings.contains(struct_type_name, ".") {
            // Qualified name - extract package and type
            dot_idx := strings.index(struct_type_name, ".")
            pkg_part := struct_type_name[:dot_idx]
            type_part := struct_type_name[dot_idx+1:]
            mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, pkg_part, type_part)
        }
        fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", mangled_name, temp_name)
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ";\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        for field, i in t.fields {
            if i > 0 {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "printf(\", \");\n")
            }
            codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%s: \");\n", field.name)
            codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
            // Print field using C expression
            field_expr := fmt.tprintf("%s.%s", temp_name, field.name)
            codegen_print_value_from_expr(ctx_cg, field_expr, -1, field.type, true, true)
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "printf(\"}\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    case:
        // For non-struct types, just use the regular function
        codegen_print_value(ctx_cg, arg, resolved_type, is_first, is_last)
    }
}

// Generate code to print a value of any type recursively
codegen_print_value :: proc(ctx_cg: ^Codegen_Context, arg: ^Node, type_info: Type_Info, is_first: bool, is_last: bool) {
    // Resolve named types first
    resolved_type := type_info
    if named, is_named := type_info.(Named_Type); is_named {
        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
            resolved_type = resolved
        }
    }
    
    #partial switch t in resolved_type {
    case Primitive_Type:
        switch t {
        case .I8, .U8, .I32, .I64:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            strings.write_string(&ctx_cg.output_buf, "printf(\"%d\", (int)")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        case .F32, .F64:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            strings.write_string(&ctx_cg.output_buf, "printf(\"%f\", (double)")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        case .Bool:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            strings.write_string(&ctx_cg.output_buf, "printf(\"%s\", ")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, " ? \"true\" : \"false\");\n")
        case .Cstring:
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            strings.write_string(&ctx_cg.output_buf, "printf(\"%s\", ")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        case .Void:
            panic("Cannot print void")
        }
    
    case Untyped_Int:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"%d\", (int)")
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ");\n")
    
    case Untyped_Float:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"%f\", (double)")
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ");\n")
    
    case Untyped_String:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"%.*s\", (int)(")
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ").len, (")
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ").data);\n")
    
    case Named_Type:
        if t.name == "string" {
            if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
            strings.write_string(&ctx_cg.output_buf, "printf(\"%.*s\", (int)(")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ").len, (")
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ").data);\n")
        } else {
            // Recursively resolve and print, but preserve the Named_Type for struct printing
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, t, ctx_cg.current_pkg_name); ok {
                // Special case: "string" resolves to a Struct_Type but should be printed specially
                // If resolved to a struct (and not "string"), we need the original Named_Type for mangled name
                if struct_type, is_struct := resolved.(Struct_Type); is_struct && t.name != "string" {
                    codegen_print_value_with_named_type(ctx_cg, arg, resolved, t, is_first, is_last)
                } else {
                    codegen_print_value(ctx_cg, arg, resolved, is_first, is_last)
                }
            } else {
                panic(fmt.tprintf("Cannot resolve type: %s", t.name))
            }
        }
    
    case Pointer_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"%p\", (void*)")
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ");\n")
    
    case Array_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        strings.write_string(&ctx_cg.output_buf, "printf(\"[\");\n")
        // Generate temporary variable name for the array
        temp_name := fmt.tprintf("_arr_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        // Get array wrapper name for the type
        wrapper_name := get_array_wrapper_name(t.element_type, t.size)
        fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", wrapper_name, temp_name)
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ";\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        for i in 0..<t.size {
            if i > 0 {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "printf(\", \");\n")
            }
            codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
            // Print element using C expression
            elem_expr := fmt.tprintf("%s.data[%d]", temp_name, i)
            codegen_print_value_from_expr(ctx_cg, elem_expr, -1, t.element_type^, true, true)
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "printf(\"]\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case Vec_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        // Generate temporary variable name for the vec
        temp_name := fmt.tprintf("_vec_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        // Get vec wrapper name for the type
        vec_wrapper_name := get_vec_wrapper_name(t.element_type)
        fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", vec_wrapper_name, temp_name)
        codegen_node(ctx_cg, arg)
        strings.write_string(&ctx_cg.output_buf, ";\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "printf(\"[\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        fmt.sbprintf(&ctx_cg.output_buf, "for (int _i = 0; _i < %s.len; _i++) {\n", temp_name)
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        strings.write_string(&ctx_cg.output_buf, "if (_i > 0) printf(\", \");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
        // Print element using C expression
        elem_expr := fmt.tprintf("%s.data[_i]", temp_name)
        codegen_print_value_from_expr(ctx_cg, elem_expr, -1, t.element_type^, true, true)
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "}\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "printf(\"]\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case Struct_Type:
        // Check if this is the built-in string type (has data: *i8 and len: i64 fields)
        is_string_type := len(t.fields) == 2 && 
            t.fields[0].name == "data" && t.fields[1].name == "len"
        if is_string_type {
            // Check if data field is *i8 and len is i64
            data_ok := false
            len_ok := false
            if ptr, is_ptr := t.fields[0].type.(Pointer_Type); is_ptr {
                if prim, ok := ptr.pointee^.(Primitive_Type); ok && prim == .I8 {
                    data_ok = true
                }
            }
            if prim, ok := t.fields[1].type.(Primitive_Type); ok && prim == .I64 {
                len_ok = true
            }
            if data_ok && len_ok {
                // This is the built-in string type - print it as a string, not a struct
                if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
                strings.write_string(&ctx_cg.output_buf, "printf(\"%.*s\", (int)(")
                codegen_node(ctx_cg, arg)
                strings.write_string(&ctx_cg.output_buf, ").len, (")
                codegen_node(ctx_cg, arg)
                strings.write_string(&ctx_cg.output_buf, ").data);\n")
                return
            }
        }
        
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        // For structs, we need to know the type name. If it's a Named_Type, use that.
        // Otherwise, we'll need to access fields directly from the expression.
        // For now, let's access fields directly without a temporary variable.
        strings.write_string(&ctx_cg.output_buf, "printf(\"{\");\n")
        // Generate temporary to avoid evaluating the expression multiple times
        temp_name := fmt.tprintf("_struct_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        // We need the struct type name - get it from the original type_info if it was Named_Type
        struct_type_name := ""
        if named, is_named := type_info.(Named_Type); is_named {
            struct_type_name = named.name
            // Qualify if needed
            if !strings.contains(struct_type_name, ".") && ctx_cg.current_pkg_name != "" {
                struct_type_name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, struct_type_name)
            }
            // Get mangled name
            mangled_name := fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, struct_type_name)
            if strings.contains(struct_type_name, ".") {
                // Qualified name - extract package and type
                dot_idx := strings.index(struct_type_name, ".")
                pkg_part := struct_type_name[:dot_idx]
                type_part := struct_type_name[dot_idx+1:]
                mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, pkg_part, type_part)
            }
            fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", mangled_name, temp_name)
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ";\n")
        } else {
            // Anonymous structs cannot be printed (no type name for C variable declaration)
            temp_name = ""
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        for field, i in t.fields {
            if i > 0 {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "printf(\", \");\n")
            }
            codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
            fmt.sbprintf(&ctx_cg.output_buf, "printf(\"%s: \");\n", field.name)
            codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
            // Print field using C expression
            field_expr: string
            if temp_name != "" {
                field_expr = fmt.tprintf("%s.%s", temp_name, field.name)
            } else {
                // Access directly from original expression - but we don't have it here
                // This is a limitation - we'd need to pass the original expression
                // For now, panic if we hit this case
                panic("Cannot print anonymous struct - struct must have a name")
            }
            codegen_print_value_from_expr(ctx_cg, field_expr, -1, field.type, true, true)
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "printf(\"}\");\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case Union_Type:
        if !is_first do strings.write_string(&ctx_cg.output_buf, "printf(\" \");\n")
        // Generate temporary to avoid evaluating the expression multiple times
        temp_name := fmt.tprintf("_union_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        strings.write_string(&ctx_cg.output_buf, "{\n")
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        // Get union type name for C variable declaration
        union_type_name := ""
        if named, is_named := type_info.(Named_Type); is_named {
            union_type_name = named.name
            if !strings.contains(union_type_name, ".") && ctx_cg.current_pkg_name != "" {
                union_type_name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, union_type_name)
            }
            mangled_name := fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, union_type_name)
            if strings.contains(union_type_name, ".") {
                dot_idx := strings.index(union_type_name, ".")
                pkg_part := union_type_name[:dot_idx]
                type_part := union_type_name[dot_idx+1:]
                mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, pkg_part, type_part)
            }
            fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", mangled_name, temp_name)
            codegen_node(ctx_cg, arg)
            strings.write_string(&ctx_cg.output_buf, ";\n")
        } else {
            // Anonymous union - can't declare variable, access directly
            temp_name = ""
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "switch (")
        if temp_name != "" {
            strings.write_string(&ctx_cg.output_buf, temp_name)
        } else {
            codegen_node(ctx_cg, arg)
        }
        strings.write_string(&ctx_cg.output_buf, ".tag) {\n")
        for variant_type, i in t.types {
            codegen_indent(ctx_cg, ctx_cg.indent_level + 2)
            fmt.sbprintf(&ctx_cg.output_buf, "case %d:\n", i)
            codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
            if temp_name != "" {
                variant_expr := fmt.tprintf("%s.data.variant_%d", temp_name, i)
                codegen_print_value_from_expr(ctx_cg, variant_expr, -1, variant_type, true, true)
            } else {
                // Anonymous union - can't easily access variant
                panic("Cannot print anonymous union")
            }
            codegen_indent(ctx_cg, ctx_cg.indent_level + 3)
            strings.write_string(&ctx_cg.output_buf, "break;\n")
        }
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        strings.write_string(&ctx_cg.output_buf, "}\n")
        if temp_name != "" {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            strings.write_string(&ctx_cg.output_buf, "}\n")
        }
    
    case:
        panic(fmt.tprintf("Cannot print type: %v", type_info))
    }
}

codegen_print_impl :: proc(ctx_cg: ^Codegen_Context, args: [dynamic]^Node, add_newline: bool) {
    if len(args) == 0 {
        if add_newline {
            strings.write_string(&ctx_cg.output_buf, "printf(\"\\n\");\n")
        }
        return
    }
    
    // Print each argument
    for arg, i in args {
        expr_type := arg.inferred_type.? or_else panic("Type not annotated")
        codegen_print_value(ctx_cg, arg, expr_type, i == 0, i == len(args) - 1)
    }
    
    if add_newline {
        strings.write_string(&ctx_cg.output_buf, "printf(\"\\n\");\n")
    }
}

codegen_node :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_node(ctx_cg, stmt)
        }

    case .Import: // NOTE(Aria): Imports are metadata for the compiler, no C code generated
    case .Link:   // NOTE(Aria): Link directives are metadata for the compiler, no C code generated
    case .Expr_Statement:
        codegen_node(ctx_cg, node.payload.(Node_Expr_Statement).expr)
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Assignment:
        assign := node.payload.(Node_Assign)
        
        // For compound assignments, generate: target = target op value
        if op_kind, has_op := assign.compound_op.?; has_op {
            codegen_node(ctx_cg, assign.target)
            strings.write_string(&ctx_cg.output_buf, " = ")
            
            // Create a synthetic binop for array operations
            // Check if target is an array type
            target_type := assign.target.inferred_type.? or_else Primitive_Type.Void
            if named_type, is_named := target_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    target_type = resolved
                }
            }
            
            if arr_type, is_arr := target_type.(Array_Type); is_arr {
                // Generate array operation using our helper functions
                op_source := ""
                #partial switch op_kind {
                case .Plus:  op_source = "+"
                case .Minus: op_source = "-"
                case .Star:  op_source = "*"
                case .Slash: op_source = "/"
                }
                
                synthetic_binop := Node_Bin_Op{
                    left = assign.target,
                    right = assign.value,
                    op = Token{kind = op_kind, source = op_source, line = 0, column = 0},
                }
                
                // Check if it's array-array or array-scalar
                value_type := assign.value.inferred_type.? or_else Primitive_Type.Void
                if named_type, is_named := value_type.(Named_Type); is_named {
                    if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                        value_type = resolved
                    }
                }
                
                _, value_is_arr := value_type.(Array_Type)
                
                if value_is_arr {
                    codegen_array_binop(ctx_cg, synthetic_binop, arr_type)
                } else {
                    codegen_array_scalar_binop(ctx_cg, synthetic_binop, arr_type, false)
                }
            } else {
                // Regular scalar compound assignment
                codegen_node(ctx_cg, assign.target)
                
                #partial switch op_kind {
                case .Plus:  strings.write_string(&ctx_cg.output_buf, "+")
                case .Minus: strings.write_string(&ctx_cg.output_buf, "-")
                case .Star:  strings.write_string(&ctx_cg.output_buf, "*")
                case .Slash: strings.write_string(&ctx_cg.output_buf, "/")
                }
                
                codegen_node(ctx_cg, assign.value)
            }
        } else {
            // Regular assignment
            codegen_node(ctx_cg, assign.target)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, assign.value)
        }
        
        if !ctx_cg.in_for_header {
            strings.write_string(&ctx_cg.output_buf, ";\n")
        }
        
    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        
        // Check if this is an array operation
        result_type := node.inferred_type.? or_else Primitive_Type.Void
        
        // Resolve named types
        resolved_result_type := result_type
        if named_type, is_named := result_type.(Named_Type); is_named {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                resolved_result_type = resolved
            }
        }
        
        if arr_type, is_arr := resolved_result_type.(Array_Type); is_arr {
            // Array operation - could be array-array or array-scalar
            // Check if either operand is a scalar
            left_type := binop.left.inferred_type.? or_else Primitive_Type.Void
            right_type := binop.right.inferred_type.? or_else Primitive_Type.Void
            
            // Resolve named types
            if named_type, is_named := left_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    left_type = resolved
                }
            }
            if named_type, is_named := right_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    right_type = resolved
                }
            }
            
            _, left_is_arr := left_type.(Array_Type)
            _, right_is_arr := right_type.(Array_Type)
            
            if left_is_arr && right_is_arr {
                // Array-array operation
                codegen_array_binop(ctx_cg, binop, arr_type)
            } else if left_is_arr && !right_is_arr {
                // Array-scalar operation
                codegen_array_scalar_binop(ctx_cg, binop, arr_type, false)
            } else if !left_is_arr && right_is_arr {
                // Scalar-array operation
                codegen_array_scalar_binop(ctx_cg, binop, arr_type, true)
            } else {
                // Shouldn't happen - fallback to regular operation
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, binop.left)
                strings.write_string(&ctx_cg.output_buf, binop.op.source)
                codegen_node(ctx_cg, binop.right)
                strings.write_string(&ctx_cg.output_buf, ")")
            }
        } else if binop.op.kind == .Eq_Eq || binop.op.kind == .Not_Eq {
            // Check if operands are arrays (comparison returns bool, not array)
            left_type := binop.left.inferred_type.? or_else Primitive_Type.Void
            if named_type, is_named := left_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    left_type = resolved
                }
            }
            
            if arr_type, is_arr := left_type.(Array_Type); is_arr {
                // Array comparison - generate element-wise comparison
                codegen_array_comparison(ctx_cg, binop, arr_type)
            } else {
                // Regular scalar operation
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, binop.left)
                strings.write_string(&ctx_cg.output_buf, binop.op.source)
                codegen_node(ctx_cg, binop.right)
                strings.write_string(&ctx_cg.output_buf, ")")
            }
        } else {
            // Regular scalar operation
            strings.write_string(&ctx_cg.output_buf, "(")
            codegen_node(ctx_cg, binop.left)
            strings.write_string(&ctx_cg.output_buf, binop.op.source)
            codegen_node(ctx_cg, binop.right)
            strings.write_string(&ctx_cg.output_buf, ")")
        }
    
    case .Cast:
        cast_node := node.payload.(Node_Cast)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_type(ctx_cg, cast_node.target_type)
        strings.write_string(&ctx_cg.output_buf, ")(")
        codegen_node(ctx_cg, cast_node.expr)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Un_Op:
        unop := node.payload.(Node_Un_Op)
        
        // Check if this is an array unary operation
        if unop.op.kind == .Plus || unop.op.kind == .Minus {
            operand_type := unop.operand.inferred_type.? or_else Primitive_Type.Void
            
            // Resolve named types
            if named_type, is_named := operand_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                    operand_type = resolved
                }
            }
            
            // Handle array unary operations
            if arr_type, is_array := operand_type.(Array_Type); is_array {
                codegen_array_unop(ctx_cg, unop, arr_type)
                return
            }
        }
        
        // Default unary operation (for primitives and other operators)
        strings.write_string(&ctx_cg.output_buf, unop.op.source)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, unop.operand)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Try:
        try_node := node.payload.(Node_Try)
        var_def_node := try_node.var_def
        var_def := var_def_node.payload.(Node_Var_Def)
        
        // Check the variable definition's content (the expression)
        // try statement requires a function call that returns a tuple
        if var_def.content.node_kind != .Fn_Call {
            strings.write_string(&ctx_cg.output_buf, "/* Error: try statement requires a function call */\n")
            return
        }
        
        // Get the expression type
        expr_type := var_def.content.inferred_type.? or_else Primitive_Type.Void
        resolved_expr_type := expr_type
        if named, is_named := expr_type.(Named_Type); is_named {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                resolved_expr_type = resolved
            }
        }
        
        tuple_type, is_tuple := resolved_expr_type.(Tuple_Type)
        if !is_tuple {
            strings.write_string(&ctx_cg.output_buf, "/* Error: try statement requires tuple */\n")
            return
        }
        
        temp_name := fmt.tprintf("_try_%d", ctx_cg.temp_counter)
        ctx_cg.temp_counter += 1
        
        typedef_name := codegen_tuple_typedef(ctx_cg, tuple_type)
        fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", typedef_name, temp_name)
        codegen_node(ctx_cg, var_def.content)
        strings.write_string(&ctx_cg.output_buf, ";\n")
        
        error_index := len(tuple_type.types) - 1
        error_field_type := tuple_type.types[error_index]
        
        resolved_error_type := error_field_type
        if named_error, is_named := error_field_type.(Named_Type); is_named {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_error, ctx_cg.current_pkg_name); ok {
                resolved_error_type = resolved
            }
        }
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "if (")
        
        // Check error: union Success variant, bool false, or pointer non-nil
        if union_error, is_union := resolved_error_type.(Union_Type); is_union {
            success_variant_index := -1
            bool_variant_index := -1
            for variant_type, i in union_error.types {
                if named_variant, is_named := variant_type.(Named_Type); is_named {
                    if named_variant.name == "Success" {
                        success_variant_index = i
                    }
                }
                if prim, is_prim := variant_type.(Primitive_Type); is_prim && prim == .Bool {
                    bool_variant_index = i
                }
            }
            
            if success_variant_index >= 0 {
                fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d.tag != %d", temp_name, error_index, success_variant_index)
                if bool_variant_index >= 0 {
                    strings.write_string(&ctx_cg.output_buf, " && (")
                    fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d.tag != %d || !%s.field_%d.data.variant_%d", 
                        temp_name, error_index, bool_variant_index,
                        temp_name, error_index, bool_variant_index)
                    strings.write_string(&ctx_cg.output_buf, ")")
                }
            } else if bool_variant_index >= 0 {
                fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d.tag == %d && !%s.field_%d.data.variant_%d", 
                    temp_name, error_index, bool_variant_index,
                    temp_name, error_index, bool_variant_index)
            } else {
                fmt.sbprintf(&ctx_cg.output_buf, "1")
            }
        } else if prim, is_prim := resolved_error_type.(Primitive_Type); is_prim && prim == .Bool {
            fmt.sbprintf(&ctx_cg.output_buf, "!%s.field_%d", temp_name, error_index)
        } else if ptr, is_ptr := resolved_error_type.(Pointer_Type); is_ptr {
            fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d != NULL", temp_name, error_index)
        } else {
            fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d", temp_name, error_index)
        }
        
        strings.write_string(&ctx_cg.output_buf, ") {\n")
        
        codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
        
        if expected_ret_type, has_ret_type := ctx_cg.current_function_return_type.?; has_ret_type {
            resolved_ret_type := expected_ret_type
            if named_ret, is_named := expected_ret_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_ret, ctx_cg.current_pkg_name); ok {
                    resolved_ret_type = resolved
                }
            }
            
            if ret_tuple, is_ret_tuple := resolved_ret_type.(Tuple_Type); is_ret_tuple {
                fmt.sbprintf(&ctx_cg.output_buf, "_ret.field_%d = %s.field_%d;\n", len(ret_tuple.types) - 1, temp_name, error_index)
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "return _ret;\n")
            } else {
                strings.write_string(&ctx_cg.output_buf, "_ret = ")
                fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d", temp_name, error_index)
                strings.write_string(&ctx_cg.output_buf, ";\n")
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "return _ret;\n")
            }
        } else {
            strings.write_string(&ctx_cg.output_buf, "return ")
            fmt.sbprintf(&ctx_cg.output_buf, "%s.field_%d", temp_name, error_index)
            strings.write_string(&ctx_cg.output_buf, ";\n")
        }
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
        
        // Extract value(s) into variable(s) - handle destructuring
        if len(var_def.names) == 1 {
            // Single variable: extract first N-1 fields
            if len(tuple_type.types) == 2 {
                codegen_type(ctx_cg, tuple_type.types[0])
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, var_def.names[0])
                fmt.sbprintf(&ctx_cg.output_buf, " = %s.field_0;\n", temp_name)
                
                if ctx_cg.func_nesting_depth > 0 {
                    ctx_cg.current_function_locals[var_def.names[0]] = true
                }
            } else if len(tuple_type.types) > 2 {
                result_types := make([dynamic]Type_Info, ctx_cg.ctx_sem.allocator)
                reserve(&result_types, len(tuple_type.types) - 1)
                for i in 0..<len(tuple_type.types) - 1 {
                    append(&result_types, tuple_type.types[i])
                }
                result_tuple := Tuple_Type{types = result_types}
                result_typedef := codegen_tuple_typedef(ctx_cg, result_tuple)
                codegen_type(ctx_cg, result_tuple)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, var_def.names[0])
                strings.write_string(&ctx_cg.output_buf, " = (")
                strings.write_string(&ctx_cg.output_buf, result_typedef)
                strings.write_string(&ctx_cg.output_buf, "){")
                for i in 0..<len(tuple_type.types) - 1 {
                    if i > 0 {
                        strings.write_string(&ctx_cg.output_buf, ", ")
                    }
                    fmt.sbprintf(&ctx_cg.output_buf, ".field_%d = %s.field_%d", i, temp_name, i)
                }
                strings.write_string(&ctx_cg.output_buf, "};\n")
                
                if ctx_cg.func_nesting_depth > 0 {
                    ctx_cg.current_function_locals[var_def.names[0]] = true
                }
            }
        } else if len(var_def.names) > 1 {
            // Multiple variables: destructure all fields (including error, but error is checked above)
            for name, i in var_def.names {
                if i < len(tuple_type.types) {
                    element_type := tuple_type.types[i]
                    codegen_type(ctx_cg, element_type)
                    strings.write_string(&ctx_cg.output_buf, " ")
                    strings.write_string(&ctx_cg.output_buf, name)
                    fmt.sbprintf(&ctx_cg.output_buf, " = %s.field_%d;\n", temp_name, i)
                    
                    if ctx_cg.func_nesting_depth > 0 {
                        ctx_cg.current_function_locals[name] = true
                    }
                }
            }
        }
    
    case .Error_Prop:
        // Reserved for future use - should not be reached
        error_prop := node.payload.(Node_Error_Prop)
        strings.write_string(&ctx_cg.output_buf, "/* Error: ? operator is reserved for future use. Use 'try' statement instead. */\n")
    
    case .Field_Access:
        field_access := node.payload.(Node_Field_Access)
        
            
        // Check if this is a qualified name (package.symbol)
        if field_access.object.node_kind == .Identifier {
            iden := field_access.object.payload.(Node_Identifier)
            // Check if this identifier is a package alias
            if pkg_dir, is_pkg := ctx_cg.ctx_sem.import_aliases[iden.name]; is_pkg {
                // Get actual package name from directory path
                actual_pkg_name := filepath.base(pkg_dir)  
                symbol_name := field_access.field_name
                
                if symbol_name == "main" {
                    strings.write_string(&ctx_cg.output_buf, symbol_name)
                } else {
                    // Check if this is an external function
                    qualified_symbol_name := fmt.tprintf("%s.%s", actual_pkg_name, symbol_name)
                    is_external := qualified_symbol_name in ctx_cg.ctx_sem.external_functions
                    
                    if is_external {
                        // Look up the symbol to get the external_name
                        external_c_name := symbol_name  // Default to symbol name
                        if pkg_info, pkg_exists := ctx_cg.ctx_sem.packages[pkg_dir]; pkg_exists {
                            if sym, sym_exists := pkg_info.symbols[symbol_name]; sym_exists {
                                if ext_name, has_ext := sym.external_name.?; has_ext {
                                    external_c_name = ext_name
                                }
                            }
                        }
                        strings.write_string(&ctx_cg.output_buf, external_c_name)
                    } else {
                        // Regular package symbols get mangled
                        fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, actual_pkg_name, symbol_name)
                    }
                }
                return
            }
        }

        object_type := field_access.object.inferred_type.? or_else panic("Internal error: type not annotated")

        // Resolve Named_Type if needed
        actual_type := object_type
        if named, is_named := object_type.(Named_Type); is_named {
            sym, _ := semantic_lookup_symbol(ctx_cg.ctx_sem, named.name)
            actual_type = sym.type
        }

        codegen_node(ctx_cg, field_access.object)
        if _, is_ptr := actual_type.(Pointer_Type); is_ptr {
            strings.write_string(&ctx_cg.output_buf, "->")
        } else {
            strings.write_string(&ctx_cg.output_buf, ".")
        }

        strings.write_string(&ctx_cg.output_buf, field_access.field_name)

    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)

    case .Fn_Call:
        fn_call := node.payload.(Node_Call)
        
        // Check if this is a built-in function
        handled := false
        if fn_call.callee.node_kind == .Identifier {
            callee_name := fn_call.callee.payload.(Node_Identifier).name
            handled = codegen_builtin_function(ctx_cg, callee_name, fn_call.args[:])
        }
        
        // If not a built-in, generate regular function call
        if !handled {
            // Get function type to check parameter types
            callee_type := fn_call.callee.inferred_type.? or_else Primitive_Type.Void
            fn_type, is_fn := callee_type.(Function_Type)
            
            codegen_node(ctx_cg, fn_call.callee)
            strings.write_string(&ctx_cg.output_buf, "(")
            for arg, i in fn_call.args {
                // Check if we need to wrap argument in union constructor
                if is_fn && i < len(fn_type.params) {
                    param_type := fn_type.params[i]
                    arg_type := arg.inferred_type.? or_else Primitive_Type.Void
                    
                    // Resolve parameter type if it's a named type
                    resolved_param_type := param_type
                    if named_param, is_named := param_type.(Named_Type); is_named {
                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_param, ctx_cg.current_pkg_name); ok {
                            resolved_param_type = resolved
                        }
                    }
                    
                    if union_param, is_union := resolved_param_type.(Union_Type); is_union {
                        // Match by name first (empty structs resolve to same type)
                        variant_index := -1
                        arg_type_name := ""
                        if arg.node_kind == .Struct_Literal {
                            struct_lit := arg.payload.(Node_Struct_Literal)
                            arg_type_name = struct_lit.type_name
                        } else if named_arg, is_named := arg_type.(Named_Type); is_named {
                            arg_type_name = named_arg.name
                        }
                        
                        if arg_type_name != "" {
                            for variant_type, idx in union_param.types {
                                if named_variant, is_named_variant := variant_type.(Named_Type); is_named_variant {
                                    if arg_type_name == named_variant.name {
                                        variant_index = idx
                                        break
                                    }
                                }
                            }
                        }
                        
                        // Fallback to type comparison for primitives
                        if variant_index == -1 {
                            resolved_arg_type := arg_type
                            if named_arg, is_named := arg_type.(Named_Type); is_named {
                                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_arg, ctx_cg.current_pkg_name); ok {
                                    resolved_arg_type = resolved
                                }
                            }
                            
                            for variant_type, idx in union_param.types {
                                if _, is_named_variant := variant_type.(Named_Type); is_named_variant {
                                    continue
                                }
                                if types_compatible(variant_type, resolved_arg_type, ctx_cg.ctx_sem) {
                                    variant_index = idx
                                    break
                                }
                            }
                        }
                        
                        if variant_index >= 0 {
                            strings.write_string(&ctx_cg.output_buf, "(")
                            codegen_type(ctx_cg, param_type)
                            fmt.sbprintf(&ctx_cg.output_buf, "){{.tag = %d, .data.variant_%d = ", variant_index, variant_index)
                            codegen_node(ctx_cg, arg)
                            strings.write_string(&ctx_cg.output_buf, "}")
                        } else {
                            codegen_node(ctx_cg, arg)
                        }
                    } else {
                        codegen_node(ctx_cg, arg)
                    }
                } else {
                    codegen_node(ctx_cg, arg)
                }
                if i < len(fn_call.args)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
            }
            
            strings.write_string(&ctx_cg.output_buf, ")")
        }
    
    case .For_C:
        for_c := node.payload.(Node_For_C)
        
        strings.write_string(&ctx_cg.output_buf, "for (")
        
        ctx_cg.in_for_header = true
        codegen_node(ctx_cg, for_c.init)
        strings.write_string(&ctx_cg.output_buf, "; ")
        codegen_node(ctx_cg, for_c.condition)
        strings.write_string(&ctx_cg.output_buf, "; ")
        codegen_node(ctx_cg, for_c.post)
        ctx_cg.in_for_header = false
        
        strings.write_string(&ctx_cg.output_buf, ") {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in for_c.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body (while still at correct indent level)
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope after emitting
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")

    case .For_In:
        for_in := node.payload.(Node_For_In)
        
        iterable_type := for_in.iterable.inferred_type.? or_else panic("Type not annotated")
        
        // Generate unique index variable
        loop_idx := ctx_cg.loop_counter
        ctx_cg.loop_counter += 1
        
        strings.write_string(&ctx_cg.output_buf, "for (int32_t __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, " = 0; __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, " < ")
        
        // Check if it's array, vec, or *vec
        #partial switch t in iterable_type {
        case Array_Type:
            fmt.sbprintf(&ctx_cg.output_buf, "%d", t.size)
        case Vec_Type:
            strings.write_string(&ctx_cg.output_buf, "(")
            codegen_node(ctx_cg, for_in.iterable)
            strings.write_string(&ctx_cg.output_buf, ").len")
        case Pointer_Type:
            if _, is_vec := t.pointee^.(Vec_Type); is_vec {
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, for_in.iterable)
                strings.write_string(&ctx_cg.output_buf, ")->len")
            } else {
                panic("Unsupported pointer type in for-in codegen")
            }
        case:
            panic("Unsupported iterable type in for-in codegen")
        }
        
        strings.write_string(&ctx_cg.output_buf, "; __i")
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, "++) {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        
        // Get element type and data access
        element_type: ^Type_Info
        use_arrow := false
        #partial switch t in iterable_type {
        case Array_Type:
            element_type = t.element_type
        case Vec_Type:
            element_type = t.element_type
        case Pointer_Type:
            if vec_t, is_vec := t.pointee^.(Vec_Type); is_vec {
                element_type = vec_t.element_type
                use_arrow = true
            } else {
                panic("Unsupported pointer type in for-in element access")
            }
        case:
            panic("Unsupported iterable type in for-in element access")
        }
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        codegen_type(ctx_cg, element_type^)
        strings.write_string(&ctx_cg.output_buf, " ")
        strings.write_string(&ctx_cg.output_buf, for_in.iterator)
        strings.write_string(&ctx_cg.output_buf, " = (")
        codegen_node(ctx_cg, for_in.iterable)
        if use_arrow {
            strings.write_string(&ctx_cg.output_buf, ")->data[__i")
        } else {
            strings.write_string(&ctx_cg.output_buf, ").data[__i")
        }
        fmt.sbprintf(&ctx_cg.output_buf, "%d", loop_idx)
        strings.write_string(&ctx_cg.output_buf, "];\n")
        
        for stmt in for_in.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")
    
    case .While:
        while_loop := node.payload.(Node_While)
        
        strings.write_string(&ctx_cg.output_buf, "while (")
        codegen_node(ctx_cg, while_loop.condition)
        strings.write_string(&ctx_cg.output_buf, ") {\n")
        
        // Push new defer scope for loop body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in while_loop.body {
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting loop body (while still at correct indent level)
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1
        
        // Pop defer scope after emitting
        pop(&ctx_cg.defer_stack)
        
        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}\n")

    case .Switch:
        switch_stmt := node.payload.(Node_Switch)
        value_type := switch_stmt.value.inferred_type.? or_else Primitive_Type.Void
        
        // Resolve named types
        resolved_value_type := value_type
        if named, is_named := value_type.(Named_Type); is_named {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                resolved_value_type = resolved
            }
        }
        
        union_type, is_union := resolved_value_type.(Union_Type)
        
        if is_union {
            // Union switch: generate switch on .tag
            // Get union type name for C variable declaration
            union_type_name := ""
            if named, is_named := value_type.(Named_Type); is_named {
                union_type_name = named.name
                if !strings.contains(union_type_name, ".") && ctx_cg.current_pkg_name != "" {
                    union_type_name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, union_type_name)
                }
            }
            
            // Generate temporary variable for union value
            temp_name := fmt.tprintf("_switch_%d", ctx_cg.temp_counter)
            ctx_cg.temp_counter += 1
            
            strings.write_string(&ctx_cg.output_buf, "{\n")
            ctx_cg.indent_level += 1
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            
            // Declare temporary union variable
            if union_type_name != "" {
                mangled_name := fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, union_type_name)
                if strings.contains(union_type_name, ".") {
                    dot_idx := strings.index(union_type_name, ".")
                    pkg_part := union_type_name[:dot_idx]
                    type_part := union_type_name[dot_idx+1:]
                    mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, pkg_part, type_part)
                }
                fmt.sbprintf(&ctx_cg.output_buf, "%s %s = ", mangled_name, temp_name)
            } else {
                panic("Switch codegen: union type must be named")
            }
            
            codegen_node(ctx_cg, switch_stmt.value)
            strings.write_string(&ctx_cg.output_buf, ";\n")
            
            // Generate switch on tag
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            strings.write_string(&ctx_cg.output_buf, "switch (")
            strings.write_string(&ctx_cg.output_buf, temp_name)
            strings.write_string(&ctx_cg.output_buf, ".tag) {\n")
            
            // Push new defer scope for switch body
            append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
            
            // Generate cases
            for case_item in switch_stmt.cases {
                type_pattern, is_type_pattern := case_item.pattern.(Type_Pattern)
                if !is_type_pattern {
                    continue // Skip invalid cases
                }
                
                // Match by name first (empty structs resolve to same type)
                variant_index := -1
                pattern_named, is_pattern_named := type_pattern.type.(Named_Type)
                if is_pattern_named {
                    for variant_type, i in union_type.types {
                        if named_variant, is_named_variant := variant_type.(Named_Type); is_named_variant {
                            if pattern_named.name == named_variant.name {
                                variant_index = i
                                break
                            }
                        }
                    }
                }
                
                // Fallback to type comparison for primitives
                if variant_index == -1 {
                    resolved_pattern_type := type_pattern.type
                    if is_pattern_named {
                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, pattern_named, ctx_cg.current_pkg_name); ok {
                            resolved_pattern_type = resolved
                        }
                    }
                    
                    for variant_type, i in union_type.types {
                        if _, is_named_variant := variant_type.(Named_Type); is_named_variant {
                            continue
                        }
                        if types_equal(resolved_pattern_type, variant_type) {
                            variant_index = i
                            break
                        }
                    }
                }
                
                if variant_index == -1 {
                    continue // Skip invalid cases (semantic analysis should have caught this)
                }
                
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                fmt.sbprintf(&ctx_cg.output_buf, "case %d:\n", variant_index)
                
                // Wrap case body in braces to allow variable declarations
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "{\n")
                ctx_cg.indent_level += 1
                
                // If pattern has a name, extract variant and assign to variable
                if pattern_name, has_name := type_pattern.var_name.?; has_name {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_type(ctx_cg, type_pattern.type)
                    strings.write_string(&ctx_cg.output_buf, " ")
                    strings.write_string(&ctx_cg.output_buf, pattern_name)
                    fmt.sbprintf(&ctx_cg.output_buf, " = %s.data.variant_%d;\n", temp_name, variant_index)
                }
                
                // Generate case body
                for stmt in case_item.body {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_node(ctx_cg, stmt)
                }
                
                // Emit defers before exiting case
                codegen_emit_defers(ctx_cg)
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "break;\n")
                ctx_cg.indent_level -= 1
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "}\n")
            }
            
            // Generate default case if present
            if default_body, has_default := switch_stmt.default_case.?; has_default {
                codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                strings.write_string(&ctx_cg.output_buf, "default:\n")
                
                ctx_cg.indent_level += 1
                for stmt in default_body {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_node(ctx_cg, stmt)
                }
                
                // Emit defers before exiting default
                codegen_emit_defers(ctx_cg)
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "break;\n")
                ctx_cg.indent_level -= 1
            }
            
            // Pop defer scope
            pop(&ctx_cg.defer_stack)
            
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            strings.write_string(&ctx_cg.output_buf, "}\n")
            
            ctx_cg.indent_level -= 1
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            strings.write_string(&ctx_cg.output_buf, "}\n")
        } else {
            // Integer/value switch: generate regular C switch
            // Check if it's a string type (struct) - C doesn't support switch on structs
            is_string_switch := false
            if named, is_named := value_type.(Named_Type); is_named {
                if named.name == "string" {
                    is_string_switch = true
                }
            } else if _, is_struct := resolved_value_type.(Struct_Type); is_struct {
                // Check if it's the string struct
                if str_struct, is_str_struct := resolved_value_type.(Struct_Type); is_str_struct {
                    if len(str_struct.fields) == 2 && 
                       str_struct.fields[0].name == "data" && 
                       str_struct.fields[1].name == "len" {
                        is_string_switch = true
                    }
                }
            }
            
            if is_string_switch {
                // String switch: generate if-else chain (C doesn't support switch on structs)
                // Push new defer scope for switch body
                append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
                
                // Generate if-else chain
                for case_item, i in switch_stmt.cases {
                    value_pattern, is_value_pattern := case_item.pattern.(Value_Pattern)
                    if !is_value_pattern {
                        continue // Skip invalid cases
                    }
                    
                    if i == 0 {
                        strings.write_string(&ctx_cg.output_buf, "if (")
                    } else {
                        codegen_indent(ctx_cg, ctx_cg.indent_level)
                        strings.write_string(&ctx_cg.output_buf, "else if (")
                    }
                    
                    // Compare strings: check both len and data
                    strings.write_string(&ctx_cg.output_buf, "(")
                    codegen_node(ctx_cg, switch_stmt.value)
                    strings.write_string(&ctx_cg.output_buf, ").len == (")
                    codegen_node(ctx_cg, value_pattern.value)
                    strings.write_string(&ctx_cg.output_buf, ").len && ")
                    strings.write_string(&ctx_cg.output_buf, "memcmp((")
                    codegen_node(ctx_cg, switch_stmt.value)
                    strings.write_string(&ctx_cg.output_buf, ").data, (")
                    codegen_node(ctx_cg, value_pattern.value)
                    strings.write_string(&ctx_cg.output_buf, ").data, (")
                    codegen_node(ctx_cg, switch_stmt.value)
                    strings.write_string(&ctx_cg.output_buf, ").len) == 0")
                    strings.write_string(&ctx_cg.output_buf, ") {\n")
                    
                    // Generate case body
                    ctx_cg.indent_level += 1
                    for stmt in case_item.body {
                        codegen_indent(ctx_cg, ctx_cg.indent_level)
                        codegen_node(ctx_cg, stmt)
                    }
                    
                    // Emit defers before exiting case
                    codegen_emit_defers(ctx_cg)
                    
                    ctx_cg.indent_level -= 1
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "}\n")
                }
                
                // Generate default case if present
                if default_body, has_default := switch_stmt.default_case.?; has_default {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "else {\n")
                    
                    ctx_cg.indent_level += 1
                    for stmt in default_body {
                        codegen_indent(ctx_cg, ctx_cg.indent_level)
                        codegen_node(ctx_cg, stmt)
                    }
                    
                    // Emit defers before exiting default
                    codegen_emit_defers(ctx_cg)
                    
                    ctx_cg.indent_level -= 1
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "}\n")
                }
                
                // Pop defer scope
                pop(&ctx_cg.defer_stack)
            } else {
                // Integer/value switch: generate regular C switch
                strings.write_string(&ctx_cg.output_buf, "switch (")
                codegen_node(ctx_cg, switch_stmt.value)
                strings.write_string(&ctx_cg.output_buf, ") {\n")
                
                // Push new defer scope for switch body
                append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
                
                // Generate cases
                for case_item in switch_stmt.cases {
                    value_pattern, is_value_pattern := case_item.pattern.(Value_Pattern)
                    if !is_value_pattern {
                        continue // Skip invalid cases
                    }
                    
                    codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                    strings.write_string(&ctx_cg.output_buf, "case ")
                    codegen_node(ctx_cg, value_pattern.value)
                    strings.write_string(&ctx_cg.output_buf, ":\n")
                    
                    // Generate case body (wrap in braces to allow variable declarations)
                    codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                    strings.write_string(&ctx_cg.output_buf, "{\n")
                    ctx_cg.indent_level += 1
                    for stmt in case_item.body {
                        codegen_indent(ctx_cg, ctx_cg.indent_level)
                        codegen_node(ctx_cg, stmt)
                    }
                    
                    // Emit defers before exiting case
                    codegen_emit_defers(ctx_cg)
                    
                    ctx_cg.indent_level -= 1
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "break;\n")
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "}\n")
                }
                
                // Generate default case if present
                if default_body, has_default := switch_stmt.default_case.?; has_default {
                    codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                    strings.write_string(&ctx_cg.output_buf, "default:\n")
                    codegen_indent(ctx_cg, ctx_cg.indent_level + 1)
                    strings.write_string(&ctx_cg.output_buf, "{\n")
                    
                    ctx_cg.indent_level += 1
                    for stmt in default_body {
                        codegen_indent(ctx_cg, ctx_cg.indent_level)
                        codegen_node(ctx_cg, stmt)
                    }
                    
                    // Emit defers before exiting default
                    codegen_emit_defers(ctx_cg)
                    
                    ctx_cg.indent_level -= 1
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "break;\n")
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    strings.write_string(&ctx_cg.output_buf, "}\n")
                }
                
                // Pop defer scope
                pop(&ctx_cg.defer_stack)
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "}\n")
            }
        }

    case .If: 
        if_node := node.payload.(Node_If)

        strings.write_string(&ctx_cg.output_buf, "if (")
        codegen_node(ctx_cg, if_node.condition)
        strings.write_string(&ctx_cg.output_buf, ") {\n")

        // Push new defer scope for if body
        append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
        
        ctx_cg.indent_level += 1
        for stmt in if_node.if_body {
            codegen_indent(ctx_cg, ctx_cg.indent_level) 
            codegen_node(ctx_cg, stmt)
        }
        
        // Emit defers before exiting if body
        codegen_emit_defers(ctx_cg)
        
        ctx_cg.indent_level -= 1

        // Pop defer scope
        pop(&ctx_cg.defer_stack)

        codegen_indent(ctx_cg, ctx_cg.indent_level)
        strings.write_string(&ctx_cg.output_buf, "}")

        if len(if_node.else_body) > 0 {
            if len(if_node.else_body) == 1 && if_node.else_body[0].node_kind == .If {
                strings.write_string(&ctx_cg.output_buf, " else ")
                codegen_node(ctx_cg, if_node.else_body[0]) 
            } else {
                strings.write_string(&ctx_cg.output_buf, " else {\n")
                
                // Push new defer scope for else body
                append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
                
                ctx_cg.indent_level += 1
                for stmt in if_node.else_body {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)
                    codegen_node(ctx_cg, stmt)
                }
                
                // Emit defers before exiting else body
                codegen_emit_defers(ctx_cg)
                
                ctx_cg.indent_level -= 1
                
                // Pop defer scope
                pop(&ctx_cg.defer_stack)
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "}\n")
            }
        } else {
            strings.write_string(&ctx_cg.output_buf, "\n")
        }

    case .Identifier:
        iden := node.payload.(Node_Identifier)

        // Identifiers should be mangled at some conditions.
        // 1) Don't mangle main or external functions
        if iden.name == "main" || iden.name in ctx_cg.ctx_sem.external_functions {
            strings.write_string(&ctx_cg.output_buf, iden.name)
            return
        }
        
        // 2) Check if it's a local variable or parameter (no mangling)
        if iden.name in ctx_cg.current_function_locals {
            strings.write_string(&ctx_cg.output_buf, iden.name)
            return
        }
        
        // 3) Check if it's a package-level symbol (needs mangling)
        // Look in current package's symbols
        pkg_dir := ""
        for dir, pkg_info in ctx_cg.ctx_sem.packages {
            if filepath.base(dir) == ctx_cg.current_pkg_name {
                pkg_dir = dir
                break
            }
        }
        
        is_package_symbol := false
        if pkg_dir != "" {
            if pkg_info, ok := ctx_cg.ctx_sem.packages[pkg_dir]; ok {
                if _, found := pkg_info.symbols[iden.name]; found {
                    is_package_symbol = true
                }
            }
        }
        
        if is_package_symbol || iden.name in ctx_cg.ctx_sem.global_symbols {
            // Mangle with package name
            pkg_name := ctx_cg.current_pkg_name
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, pkg_name, iden.name)
            return
        }
        
        // Local variable or parameter - don't mangle
        strings.write_string(&ctx_cg.output_buf, iden.name)
    
    case .Index:
        index_node := node.payload.(Node_Index)
        
        // Check if we're indexing an array, vec, or pointer-to-vec
        obj_type := index_node.object.inferred_type.? or_else Primitive_Type.Void
        
        // Resolve named types first
        resolved_obj_type := obj_type
        if named_type, is_named := obj_type.(Named_Type); is_named {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                resolved_obj_type = resolved
            }
        }
        
        is_array := false
        is_vec_value := false
        is_vec_ptr := false
        
        #partial switch t in resolved_obj_type {
        case Array_Type:
            is_array = true
        case Vec_Type:
            is_vec_value = true
        case Pointer_Type:
            // Check if it's a pointer to vec
            if vec_t, is_vec := t.pointee^.(Vec_Type); is_vec {
                is_vec_ptr = true
            }
        }
        
        if is_array || is_vec_value {
            // Access through .data field of wrapper struct
            strings.write_string(&ctx_cg.output_buf, "(")
            codegen_node(ctx_cg, index_node.object)
            strings.write_string(&ctx_cg.output_buf, ").data[")
            codegen_node(ctx_cg, index_node.index)
            strings.write_string(&ctx_cg.output_buf, "]")
        } else if is_vec_ptr {
            // Pointer to vec: use ->data
            strings.write_string(&ctx_cg.output_buf, "(")
            codegen_node(ctx_cg, index_node.object)
            strings.write_string(&ctx_cg.output_buf, ")->data[")
            codegen_node(ctx_cg, index_node.index)
            strings.write_string(&ctx_cg.output_buf, "]")
        } else {
            // Regular pointer/array indexing
            codegen_node(ctx_cg, index_node.object)
            strings.write_string(&ctx_cg.output_buf, "[")
            codegen_node(ctx_cg, index_node.index)
            strings.write_string(&ctx_cg.output_buf, "]")
        }
    
    case .Len:
        len_node := node.payload.(Node_Len)
        value_type := len_node.value.inferred_type.? or_else panic("Type not inferred")
        
        #partial switch t in value_type {
        case Array_Type:
            // Array length is compile-time constant
            fmt.sbprintf(&ctx_cg.output_buf, "%d", t.size)
        case Named_Type:
            if t.name == "string" {
                // String length is runtime field access
                strings.write_string(&ctx_cg.output_buf, "(")
                codegen_node(ctx_cg, len_node.value)
                strings.write_string(&ctx_cg.output_buf, ").len")
            } else {
                panic("len() called on invalid type")
            }
        case:
            panic("len() called on invalid type")
        }

    case .Literal_Arr:
        arr_lit := node.payload.(Node_Array_Literal)
        arr_type_info_opt := node.inferred_type
        if arr_type_info_opt == nil {
            // Fall back to explicit type from the literal payload if inference was deferred
            if explicit, ok := arr_lit.explicit_type.?; ok {
                arr_type_info_opt = explicit
            } else {
                panic("Array literal missing type information")
            }
        }

        arr_type_info := arr_type_info_opt.?

        arr_type: Maybe(Array_Type)
        if as_array, ok := arr_type_info.(Array_Type); ok {
            arr_type = as_array
        } else if named, ok := arr_type_info.(Named_Type); ok {
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                if resolved_arr, ok := resolved.(Array_Type); ok {
                    arr_type = resolved_arr
                }
            }
        }

        if arr_type == nil {
            panic("Array literal type must resolve to Array_Type")
        }

        resolved_arr := arr_type.?
        wrapper_name := get_array_wrapper_name(resolved_arr.element_type, resolved_arr.size)
        
        // Generate: (WrapperStruct){{elem1, elem2, ...}}
        strings.write_string(&ctx_cg.output_buf, "(")
        strings.write_string(&ctx_cg.output_buf, wrapper_name)
        strings.write_string(&ctx_cg.output_buf, "){{")
        for elem, i in arr_lit.elements {
            codegen_node(ctx_cg, elem)
            if i < len(arr_lit.elements) - 1 {
                strings.write_string(&ctx_cg.output_buf, ", ")
            }
        }
        strings.write_string(&ctx_cg.output_buf, "}}")

    case .Literal_Number:
        lit := node.payload.(Node_Literal_Number)
        src := lit.content.source
        strings.write_string(&ctx_cg.output_buf, src)
    
    case .Literal_Nil:
        strings.write_string(&ctx_cg.output_buf, "NULL")
    
    case .Literal_String:
        lit := node.payload.(Node_Literal_String)
    
        raw_content := strings.trim(lit.content.source, "\"")
        decoded := decode_string_literal_bytes(raw_content, ctx_cg.ctx_sem.allocator)
        
        // Check inferred type - if cstring, emit C string literal, otherwise Qoz_String
        inferred_type := node.inferred_type.? or_else Untyped_String{}
        
        is_cstring := false
        if prim, ok := inferred_type.(Primitive_Type); ok && prim == .Cstring {
            is_cstring = true
        }
        
        if is_cstring {
            emit_c_string_literal(ctx_cg, decoded)
        } else {
            // Emit Qoz_String compound literal
            strings.write_string(&ctx_cg.output_buf, "(Qoz_String){.data = ")
            emit_c_string_literal(ctx_cg, decoded)
            strings.write_string(&ctx_cg.output_buf, ", .len = ")
            fmt.sbprintf(&ctx_cg.output_buf, "%d", len(decoded))
            strings.write_string(&ctx_cg.output_buf, "}")
        }
    
    case .Literal_Bool:
        lit := node.payload.(Node_Literal_Bool)
        src := lit.content.source
        strings.write_string(&ctx_cg.output_buf, src)
    
    case .Defer:
        // Collect defer, don't emit yet
        defer_node := node.payload.(Node_Defer)
        if len(ctx_cg.defer_stack) > 0 {
            append(&ctx_cg.defer_stack[len(ctx_cg.defer_stack) - 1], node)
        }
    
    case .Return:
        ret := node.payload.(Node_Return)
        
        // Emit all defers in current scope in reverse order
        codegen_emit_defers(ctx_cg)
        
        strings.write_string(&ctx_cg.output_buf, "return")
        
        if len(ret.values) > 0 {
            strings.write_string(&ctx_cg.output_buf, " ")
            
            // Check if we need to wrap return values in tuple or union constructor
            if expected_ret_type, has_ret_type := ctx_cg.current_function_return_type.?; has_ret_type {
                // Resolve return type if it's a named type
                resolved_ret_type := expected_ret_type
                if named_ret, is_named := expected_ret_type.(Named_Type); is_named {
                    if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_ret, ctx_cg.current_pkg_name); ok {
                        resolved_ret_type = resolved
                    }
                }
                
                // Check if return type is a tuple
                if tuple_ret, is_tuple := resolved_ret_type.(Tuple_Type); is_tuple {
                    // Multiple return values - generate tuple struct literal
                    strings.write_string(&ctx_cg.output_buf, "(")
                    codegen_type(ctx_cg, expected_ret_type)
                    strings.write_string(&ctx_cg.output_buf, "){")
                    for value, i in ret.values {
                        if i > 0 {
                            strings.write_string(&ctx_cg.output_buf, ", ")
                        }
                        fmt.sbprintf(&ctx_cg.output_buf, ".field_%d = ", i)
                        
                        // Wrap union elements in union constructor
                        if i < len(tuple_ret.types) {
                            element_type := tuple_ret.types[i]
                            value_type := value.inferred_type.? or_else Primitive_Type.Void
                            
                            resolved_element_type := element_type
                            if named_element, is_named := element_type.(Named_Type); is_named {
                                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_element, ctx_cg.current_pkg_name); ok {
                                    resolved_element_type = resolved
                                }
                            }
                            
                            if union_element, is_union := resolved_element_type.(Union_Type); is_union {
                                // Match by name first (empty structs resolve to same type)
                                variant_index := -1
                                value_type_name := ""
                                if value.node_kind == .Struct_Literal {
                                    struct_lit := value.payload.(Node_Struct_Literal)
                                    value_type_name = struct_lit.type_name
                                } else if named_value, is_named := value_type.(Named_Type); is_named {
                                    value_type_name = named_value.name
                                }
                                
                                if value_type_name != "" {
                                    for variant_type, idx in union_element.types {
                                        if named_variant, is_named_variant := variant_type.(Named_Type); is_named_variant {
                                            if value_type_name == named_variant.name {
                                                variant_index = idx
                                                break
                                            }
                                        }
                                    }
                                }
                                
                                // Fallback to type comparison for primitives
                                if variant_index == -1 {
                                    resolved_value_type := value_type
                                    if named_value, is_named := value_type.(Named_Type); is_named {
                                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_value, ctx_cg.current_pkg_name); ok {
                                            resolved_value_type = resolved
                                        }
                                    }
                                    
                                    for variant_type, idx in union_element.types {
                                        if _, is_named_variant := variant_type.(Named_Type); is_named_variant {
                                            continue
                                        }
                                        if types_compatible(variant_type, resolved_value_type, ctx_cg.ctx_sem) {
                                            variant_index = idx
                                            break
                                        }
                                    }
                                }
                                
                                if variant_index >= 0 {
                                    strings.write_string(&ctx_cg.output_buf, "(")
                                    codegen_type(ctx_cg, element_type)
                                    fmt.sbprintf(&ctx_cg.output_buf, "){{.tag = %d, .data.variant_%d = ", variant_index, variant_index)
                                    codegen_node(ctx_cg, value)
                                    strings.write_string(&ctx_cg.output_buf, "}")
                                } else {
                                    codegen_node(ctx_cg, value)
                                }
                            } else {
                                codegen_node(ctx_cg, value)
                            }
                        } else {
                            codegen_node(ctx_cg, value)
                        }
                    }
                    strings.write_string(&ctx_cg.output_buf, "}")
                } else if len(ret.values) == 1 {
                    ret_value_type := ret.values[0].inferred_type.? or_else Primitive_Type.Void
                    
                    if union_ret, is_union := resolved_ret_type.(Union_Type); is_union {
                        variant_index := -1
                        for union_type, idx in union_ret.types {
                            if types_compatible(union_type, ret_value_type, ctx_cg.ctx_sem) {
                                variant_index = idx
                                break
                            }
                        }
                        
                        if variant_index >= 0 {
                            strings.write_string(&ctx_cg.output_buf, "(")
                            codegen_type(ctx_cg, expected_ret_type)
                            fmt.sbprintf(&ctx_cg.output_buf, "){{.tag = %d, .data.variant_%d = ", variant_index, variant_index)
                            codegen_node(ctx_cg, ret.values[0])
                            strings.write_string(&ctx_cg.output_buf, "}")
                        } else {
                            codegen_node(ctx_cg, ret.values[0])
                        }
                    } else {
                        codegen_node(ctx_cg, ret.values[0])
                    }
                } else {
                    // Multiple values but not a tuple - error (should be caught in semantic)
                    for value, i in ret.values {
                        if i > 0 {
                            strings.write_string(&ctx_cg.output_buf, ", ")
                        }
                        codegen_node(ctx_cg, value)
                    }
                }
            } else {
                // No expected type - just emit values
                for value, i in ret.values {
                    if i > 0 {
                        strings.write_string(&ctx_cg.output_buf, ", ")
                    }
                    codegen_node(ctx_cg, value)
                }
            }
        }
        strings.write_string(&ctx_cg.output_buf, ";\n")

    case .Print:
        print := node.payload.(Node_Print)
        codegen_print_impl(ctx_cg, print.args, false)
    
    case .Println:
        println := node.payload.(Node_Println)
        codegen_print_impl(ctx_cg, println.args, true)

    case .Size_Of:
        sizeof_op := node.payload.(Node_Size_Of)
        strings.write_string(&ctx_cg.output_buf, "sizeof(")
        codegen_node(ctx_cg, sizeof_op.type)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Struct_Literal:
        struct_lit := node.payload.(Node_Struct_Literal)
        
        // Use type_name if set (preserves struct type when coerced to union)
        strings.write_string(&ctx_cg.output_buf, "(")
        type_to_generate: Type_Info
        if struct_lit.type_name != "" {
            if !strings.contains(struct_lit.type_name, ".") && ctx_cg.current_pkg_name != "" {
                type_to_generate = Named_Type{name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, struct_lit.type_name)}
            } else {
                type_to_generate = Named_Type{name = struct_lit.type_name}
            }
        } else if inferred, ok := node.inferred_type.?; ok {
            resolved_inferred := inferred
            if named_inferred, is_named := inferred.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_inferred, ctx_cg.current_pkg_name); ok {
                    resolved_inferred = resolved
                }
            }
            
            if _, is_union := resolved_inferred.(Union_Type); is_union {
                panic("Struct literal inferred as union type but type_name not set")
            }
            
            if named, is_named := inferred.(Named_Type); is_named {
                if !strings.contains(named.name, ".") && ctx_cg.current_pkg_name != "" {
                    type_to_generate = Named_Type{name = fmt.tprintf("%s.%s", ctx_cg.current_pkg_name, named.name)}
                } else {
                    type_to_generate = inferred
                }
            } else {
                type_to_generate = inferred
            }
        } else {
            panic("Struct literal has no type information")
        }
        codegen_type(ctx_cg, type_to_generate)
        strings.write_string(&ctx_cg.output_buf, ")")
        
        // Then the initializer list
        strings.write_string(&ctx_cg.output_buf, "{")
        for field_init, i in struct_lit.field_inits {
            strings.write_string(&ctx_cg.output_buf, ".")
            strings.write_string(&ctx_cg.output_buf, field_init.name)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, field_init.value)
            if i < len(struct_lit.field_inits) - 1 {
                strings.write_string(&ctx_cg.output_buf, ", ")
            }
        }
        strings.write_string(&ctx_cg.output_buf, "}")

    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)

        //| Special case, where variable is function definition
        //+----------------------------------------------------
        if var_def.content.node_kind == .Fn_Def {
            if ctx_cg.func_nesting_depth > 0 {
                panic("TODO(Aria): implement lambda lifting for nested function")
            }

            fn_def := var_def.content.payload.(Node_Fn_Def)

            if fn_def.is_external do return

            // Look up qualified type from current package's symbol table
            qualified_fn_type: Maybe(Function_Type)
            // Find the package dir that matches current package name
            for pkg_dir, pkg_info in ctx_cg.ctx_sem.packages {
                if filepath.base(pkg_dir) == ctx_cg.current_pkg_name {
                    if len(var_def.names) > 0 {
                        if sym, found := pkg_info.symbols[var_def.names[0]]; found {
                            if fn_type, is_fn := sym.type.(Function_Type); is_fn {
                                qualified_fn_type = fn_type
                                break
                            }
                        }
                    }
                }
            }

            ctx_cg.func_nesting_depth += 1
            ctx_cg.indent_level += 1
            
            // Track parameters and local variables in this function
            clear(&ctx_cg.current_function_locals)
            for param in fn_def.params {
                ctx_cg.current_function_locals[param.name] = true
            }
            
            // Track function return type for union wrapping
            old_return_type := ctx_cg.current_function_return_type
            ctx_cg.current_function_return_type = fn_def.return_type
            
            // Push a new defer scope for this function
            append(&ctx_cg.defer_stack, make([dynamic]^Node, ctx_cg.ctx_sem.allocator))
            
            if len(var_def.names) > 0 {
                codegen_func_signature(ctx_cg, var_def.names[0], fn_def, qualified_fn_type)
            }
            strings.write_string(&ctx_cg.output_buf, " {\n")
            
            // Create implicit return variable for error propagation
            has_explicit_return := false
            is_void_return := false
            if prim, is_prim := fn_def.return_type.(Primitive_Type); is_prim && prim == .Void {
                is_void_return = true
            }
            
            if !is_void_return {
                for stmt in fn_def.body {
                    if stmt.node_kind == .Return {
                        has_explicit_return = true
                        break
                    }
                }
                
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                codegen_type(ctx_cg, fn_def.return_type)
                strings.write_string(&ctx_cg.output_buf, " _ret = ")
                codegen_zero_value(ctx_cg, fn_def.return_type)
                strings.write_string(&ctx_cg.output_buf, ";\n")
            }
            
            for stmt in fn_def.body {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                codegen_node(ctx_cg, stmt)
            }
            
            codegen_emit_defers(ctx_cg)
            
            if !is_void_return && !has_explicit_return {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "return _ret;\n")
            } else if len(var_def.names) > 0 && var_def.names[0] == "main" {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                strings.write_string(&ctx_cg.output_buf, "return 0;\n")
            }

            strings.write_string(&ctx_cg.output_buf, "}\n\n")
            
            // Pop defer scope
            pop(&ctx_cg.defer_stack)
            
            // Restore previous return type
            ctx_cg.current_function_return_type = old_return_type
            
            ctx_cg.func_nesting_depth -= 1
            ctx_cg.indent_level -= 1
        } else if var_def.content.node_kind == .Type_Expr {
            // Struct typedef - skip if already generated in Pass 2
            if ctx_cg.skip_struct_defs do return
            
            type_expr := var_def.content.payload.(Node_Type_Expr)
                
            if struct_type, is_struct := type_expr.type_info.(Struct_Type); is_struct {
                strings.write_string(&ctx_cg.output_buf, "typedef struct ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                if len(var_def.names) > 0 {
                strings.write_string(&ctx_cg.output_buf, var_def.names[0])
            } 
                strings.write_string(&ctx_cg.output_buf, " {\n")
                ctx_cg.indent_level += 1
                
                for field in struct_type.fields {
                    codegen_indent(ctx_cg, ctx_cg.indent_level)

                    // Resolve the field type to handle type aliases
                    resolved_field_type := field.type
                    if named_type, is_named := field.type.(Named_Type); is_named {
                        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named_type, ctx_cg.current_pkg_name); ok {
                            resolved_field_type = resolved
                        }
                    }

                    if _, is_fn := resolved_field_type.(Function_Type); is_fn {
                        codegen_type(ctx_cg, field.type, field.name)
                    } else {
                        codegen_type(ctx_cg, field.type)
                        strings.write_string(&ctx_cg.output_buf, " ")
                        strings.write_string(&ctx_cg.output_buf, field.name)
                    }

                    strings.write_string(&ctx_cg.output_buf, ";\n")
                }
                
                ctx_cg.indent_level -= 1
                strings.write_string(&ctx_cg.output_buf, "} ")
                strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
                if len(var_def.names) > 0 {
                    strings.write_string(&ctx_cg.output_buf, var_def.names[0])
                }
                strings.write_string(&ctx_cg.output_buf, ";\n\n")
            }
        } else {
            // Handle destructuring assignment: value, ok := func()
            if len(var_def.names) > 1 {
                // Generate: type0 name0; type1 name1; ... name0 = call_result.field_0; name1 = call_result.field_1; ...
                call_result_type := node.inferred_type.(Type_Info)
                
                // Resolve if Named_Type
                resolved_call_type := call_result_type
                if named, is_named := call_result_type.(Named_Type); is_named {
                    if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                        resolved_call_type = resolved
                    }
                }
                
                tuple_ret, is_tuple := resolved_call_type.(Tuple_Type)
                if is_tuple {
                    // Generate temporary variable for the call result
                    temp_name := fmt.tprintf("_tuple_%d", ctx_cg.temp_counter)
                    ctx_cg.temp_counter += 1
                    
                    codegen_type(ctx_cg, call_result_type)
                    strings.write_string(&ctx_cg.output_buf, " ")
                    strings.write_string(&ctx_cg.output_buf, temp_name)
                    strings.write_string(&ctx_cg.output_buf, " = ")
                    codegen_node(ctx_cg, var_def.content)
                    strings.write_string(&ctx_cg.output_buf, ";\n")
                    
                    // Extract each field into its variable
                    for name, i in var_def.names {
                        if i < len(tuple_ret.types) {
                            element_type := tuple_ret.types[i]
                            codegen_type(ctx_cg, element_type)
                            strings.write_string(&ctx_cg.output_buf, " ")
                            strings.write_string(&ctx_cg.output_buf, name)
                            fmt.sbprintf(&ctx_cg.output_buf, " = %s.field_%d;\n", temp_name, i)
                            
                            // Track local variable
                            if ctx_cg.func_nesting_depth > 0 {
                                ctx_cg.current_function_locals[name] = true
                            }
                        }
                    }
                } else {
                    // Error case - should be caught in semantic
                    codegen_type(ctx_cg, call_result_type)
                    strings.write_string(&ctx_cg.output_buf, " ")
                    if len(var_def.names) > 0 {
                        strings.write_string(&ctx_cg.output_buf, var_def.names[0])
                    }
                    strings.write_string(&ctx_cg.output_buf, " = ")
                    codegen_node(ctx_cg, var_def.content)
                    if !ctx_cg.in_for_header {
                        strings.write_string(&ctx_cg.output_buf, ";\n")
                    }
                }
            } else {
                //| Regular case - single variable
                //+-------------
                var_type := node.inferred_type.(Type_Info)

                // Track local variable in current function
                if ctx_cg.func_nesting_depth > 0 {
                    if len(var_def.names) > 0 {
                        ctx_cg.current_function_locals[var_def.names[0]] = true
                    }
                }

                // Regular variable assignment
                codegen_type(ctx_cg, node.inferred_type.(Type_Info))
                strings.write_string(&ctx_cg.output_buf, " ")
                
                // Add package prefix for top-level variables
                if ctx_cg.func_nesting_depth == 0 {
                    strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
                    strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                    strings.write_string(&ctx_cg.output_buf, "__")
                }
                if len(var_def.names) > 0 {
                    strings.write_string(&ctx_cg.output_buf, var_def.names[0])
                }

                strings.write_string(&ctx_cg.output_buf, " = ")
                codegen_node(ctx_cg, var_def.content)
                if !ctx_cg.in_for_header {
                    strings.write_string(&ctx_cg.output_buf, ";\n")
                }
            }
        }
    
    case .Del:
        del_node := node.payload.(Node_Del)
        
        // Check what we're freeing
        ptr_type := del_node.pointer.inferred_type.? or_else Primitive_Type.Void
        
        // Special case: Vec types don't need allocator parameter
        is_vec := false
        if ptr_type_val, is_ptr := ptr_type.(Pointer_Type); is_ptr {
            if _, is_vec_type := ptr_type_val.pointee^.(Vec_Type); is_vec_type {
                is_vec = true
            }
        }
        
        if is_vec {
            // Vec deletion: free data, then free vec itself using embedded allocator
            // (*v).allocator.free((*v).allocator.data, (*v).data);
            // (*v).allocator.free((*v).allocator.data, v);
            strings.write_string(&ctx_cg.output_buf, "(*")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ").allocator.free((*")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ").allocator.data, (*")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ").data);\n")
            
            codegen_indent(ctx_cg, ctx_cg.indent_level)
            strings.write_string(&ctx_cg.output_buf, "(*")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ").allocator.free((*")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ").allocator.data, ")
            codegen_node(ctx_cg, del_node.pointer)
            strings.write_string(&ctx_cg.output_buf, ");\n")
        } else {
            // Regular deletion with allocator parameter
            #partial switch t in ptr_type {
            case Named_Type:
                if t.name == "string" {
                    // Free string's internal data
                    codegen_node(ctx_cg, del_node.allocator)
                    strings.write_string(&ctx_cg.output_buf, ".free(")
                    codegen_node(ctx_cg, del_node.allocator)
                    strings.write_string(&ctx_cg.output_buf, ".data, (void*)")
                    codegen_node(ctx_cg, del_node.pointer)
                    strings.write_string(&ctx_cg.output_buf, ".data);\n")
                }
            case Pointer_Type:
                // Direct pointer free
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".free(")
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".data, ")
                codegen_node(ctx_cg, del_node.pointer)
                strings.write_string(&ctx_cg.output_buf, ");\n")
            case Array_Type:
                // Free array pointer
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".free(")
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".data, ")
                codegen_node(ctx_cg, del_node.pointer)
                strings.write_string(&ctx_cg.output_buf, ");\n")
            case:
                // Fallback - direct pointer free
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".free(")
                codegen_node(ctx_cg, del_node.allocator)
                strings.write_string(&ctx_cg.output_buf, ".data, ")
                codegen_node(ctx_cg, del_node.pointer)
                strings.write_string(&ctx_cg.output_buf, ");\n")
            }
        }

    case .New:
        new_node := node.payload.(Node_New)
        result_type := node.inferred_type.? or_else Primitive_Type.Void
        
        // Check if this is a vec type
        is_vec := false
        vec_type: Vec_Type
        if ptr_type, is_ptr := result_type.(Pointer_Type); is_ptr {
            if vec_t, is_vec_type := ptr_type.pointee^.(Vec_Type); is_vec_type {
                is_vec = true
                vec_type = vec_t
            }
        }
        
        if is_vec {
            // Vec allocation: allocate + initialize fields individually
            strings.write_string(&ctx_cg.output_buf, "({")
            codegen_type(ctx_cg, result_type)
            strings.write_string(&ctx_cg.output_buf, " _v = (")
            codegen_type(ctx_cg, result_type)
            strings.write_string(&ctx_cg.output_buf, ")")
            codegen_node(ctx_cg, new_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".alloc(")
            codegen_node(ctx_cg, new_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".data, sizeof(")
            codegen_type(ctx_cg, new_node.type)
            strings.write_string(&ctx_cg.output_buf, ")); _v->data = NULL; _v->len = 0; _v->cap = 0; _v->allocator = ")
            codegen_node(ctx_cg, new_node.allocator)
            strings.write_string(&ctx_cg.output_buf, "; _v; })")
        } else {
            // Regular allocation: just call allocator.alloc and cast
            strings.write_string(&ctx_cg.output_buf, "((")
            codegen_type(ctx_cg, result_type)
            strings.write_string(&ctx_cg.output_buf, ")")
            codegen_node(ctx_cg, new_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".alloc(")
            codegen_node(ctx_cg, new_node.allocator)
            strings.write_string(&ctx_cg.output_buf, ".data, sizeof(")
            codegen_type(ctx_cg, new_node.type)
            strings.write_string(&ctx_cg.output_buf, ")))")
        }
        
    case: fmt.panicf("Internal error: cannot generate code for node %v", node.node_kind)
    }
}

// Generate zero value for a type (for error propagation when function returns tuple)
codegen_zero_value :: proc(ctx_cg: ^Codegen_Context, type: Type_Info) {
    #partial switch t in type {
    case Primitive_Type:
        #partial switch t {
        case .I8, .U8, .I32, .I64: strings.write_string(&ctx_cg.output_buf, "0")
        case .F32, .F64: strings.write_string(&ctx_cg.output_buf, "0.0")
        case .Bool: strings.write_string(&ctx_cg.output_buf, "0")
        case .Cstring: strings.write_string(&ctx_cg.output_buf, "NULL")
        case .Void: strings.write_string(&ctx_cg.output_buf, "/* void */")
        case: strings.write_string(&ctx_cg.output_buf, "0")
        }
    case Pointer_Type:
        strings.write_string(&ctx_cg.output_buf, "NULL")
    case Array_Type:
        // Zero-initialize array: {0} (C zero-initializes all elements)
        strings.write_string(&ctx_cg.output_buf, "{0}")
    case Vec_Type:
        // Vec zero value: {.data = NULL, .len = 0, .cap = 0, .allocator = {0}}
        strings.write_string(&ctx_cg.output_buf, "{.data = NULL, .len = 0, .cap = 0, .allocator = {0}}")
    case Struct_Type:
        // Struct zero value: {0} (C zero-initializes all fields)
        strings.write_string(&ctx_cg.output_buf, "{0}")
    case Union_Type:
        // Union zero value: {0} (C zero-initializes)
        strings.write_string(&ctx_cg.output_buf, "{0}")
    case Tuple_Type:
        // Tuple zero value: {0} (C zero-initializes all fields)
        strings.write_string(&ctx_cg.output_buf, "{0}")
    case Named_Type:
        // Check for string type
        if t.name == "string" || t.name == "strings.string" {
            // String zero value: {.data = NULL, .len = 0}
            strings.write_string(&ctx_cg.output_buf, "{.data = NULL, .len = 0}")
        } else {
            // Resolve named type and generate zero value
            if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, t, ctx_cg.current_pkg_name); ok {
                codegen_zero_value(ctx_cg, resolved)
            } else {
                // Unknown type - use {0} as fallback
                strings.write_string(&ctx_cg.output_buf, "{0}")
            }
        }
    case:
        // Fallback
        strings.write_string(&ctx_cg.output_buf, "0")
    }
}

codegen_type :: proc(ctx_cg: ^Codegen_Context, type: Type_Info, name: string = "") {
    #partial switch t in type {
    case Function_Type:
        // C function pointer format: return_type (*)(params)
        codegen_type(ctx_cg, t.return_type^)
        strings.write_string(&ctx_cg.output_buf, " (*")
        strings.write_string(&ctx_cg.output_buf, name)
        strings.write_string(&ctx_cg.output_buf, ")(")

        if len(t.params) == 0 {
            strings.write_string(&ctx_cg.output_buf, "void")
        } else {
            for param, i in t.params {
                codegen_type(ctx_cg, param)
                if i < len(t.params) - 1 {
                    strings.write_string(&ctx_cg.output_buf, ", ")
                }
            }
        }
        strings.write_string(&ctx_cg.output_buf, ")")

    case Primitive_Type:
        #partial switch t {
        case .I8:  strings.write_string(&ctx_cg.output_buf, "int8_t")
        case .U8:  strings.write_string(&ctx_cg.output_buf, "uint8_t")
        case .I32: strings.write_string(&ctx_cg.output_buf, "int32_t")
        case .I64: strings.write_string(&ctx_cg.output_buf, "int64_t")
        case .F32: strings.write_string(&ctx_cg.output_buf, "float")
        case .F64: strings.write_string(&ctx_cg.output_buf, "double")
        case .Bool: strings.write_string(&ctx_cg.output_buf, "_Bool")
        case .Cstring: strings.write_string(&ctx_cg.output_buf, "const char*")
        case .Void: strings.write_string(&ctx_cg.output_buf, "void")
        case: fmt.panicf("Internal error: cannot generate code for type %v", t)
        }
    case Array_Type:
        // Arrays are wrapped in structs for return values
        wrapper_name := get_array_wrapper_name(t.element_type, t.size)
        strings.write_string(&ctx_cg.output_buf, wrapper_name)
    case Vec_Type:
        // Vecs are structs with data, len, cap, allocator
        wrapper_name := get_vec_wrapper_name(t.element_type)
        strings.write_string(&ctx_cg.output_buf, wrapper_name)
    case Pointer_Type:
        codegen_type(ctx_cg, t.pointee^)
        strings.write_string(&ctx_cg.output_buf, "*")
    case Struct_Type:
        panic("Internal error: FIXME(Aria): Cannot inline anonymous struct type")
    case Union_Type:
        // Generate the struct name - this happens when resolving named union types
        // We can't know the actual name here, so this is a fallback
        panic("Internal error: Union_Type should be handled as Named_Type, not inline")
    case Tuple_Type:
        // Generate typedef name for tuple type
        typedef_name := codegen_tuple_typedef(ctx_cg, t)
        strings.write_string(&ctx_cg.output_buf, typedef_name)
    case Named_Type:
        // Check for builtin string type
        if t.name == "string" || t.name == "strings.string" {
            strings.write_string(&ctx_cg.output_buf, "Qoz_String")
            return
        }
        
        // Check if this is a type alias - if so, resolve and generate the underlying type
        // But only for non-struct types (primitives, function types, etc.)
        if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, t, ctx_cg.current_pkg_name); ok {
            // If resolved to a non-struct, non-named type, it's a pure alias
            #partial switch r in resolved {
            case Function_Type, Primitive_Type, Pointer_Type, Array_Type, Vec_Type:
                // This is a type alias to a concrete type, generate the underlying type
                codegen_type(ctx_cg, resolved, name)
                return
            case Struct_Type, Union_Type:
                // This is a typedef for a struct/union, keep using the name (don't inline)
                // Fall through to normal Named_Type handling
            case Named_Type:
                // Still named after resolution, fall through
            }
        }
        
        // Check if type name contains a dot (qualified)
        if strings.contains(t.name, ".") {
            // Qualified name like "mem.Allocator" -> "qoz__mem__Allocator"
            mangled_name, _ := strings.replace_all(t.name, ".", "__", context.temp_allocator)
            strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
            strings.write_string(&ctx_cg.output_buf, mangled_name)
        } else {
            // Unqualified name like "Arena" -> qualify with current package
            strings.write_string(&ctx_cg.output_buf, MANGLE_PREFIX)
            if ctx_cg.current_pkg_name != "" {
                strings.write_string(&ctx_cg.output_buf, ctx_cg.current_pkg_name)
                strings.write_string(&ctx_cg.output_buf, "__")
            }
            strings.write_string(&ctx_cg.output_buf, t.name)
        }
    case Untyped_Int:
        strings.write_string(&ctx_cg.output_buf, "int64_t")
    case Untyped_Float:
        strings.write_string(&ctx_cg.output_buf, "double")
    case Untyped_String:
        // Default to Qoz_String for untyped strings
        strings.write_string(&ctx_cg.output_buf, "Qoz_String")
    case: fmt.panicf("Internal error: cannot generate code for type %v", t)
    }
    
}

codegen_forward_decl :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    if node.node_kind == .Program {
        for stmt in node.payload.(Node_Statement_List).nodes {
            if stmt.node_kind == .Var_Def {
                var_def := stmt.payload.(Node_Var_Def)
                if var_def.content.node_kind == .Fn_Def {
                    // function signature generation
                    // type func_name(..params)
                    // Look up qualified type from current package's symbol table
                    fn_def := var_def.content.payload.(Node_Fn_Def)
                    qualified_fn_type: Maybe(Function_Type)
                    // Find the package dir that matches current package name
                    for pkg_dir, pkg_info in ctx_cg.ctx_sem.packages {
                        if filepath.base(pkg_dir) == ctx_cg.current_pkg_name {
                            if len(var_def.names) > 0 {
                                if sym, found := pkg_info.symbols[var_def.names[0]]; found {
                                    if fn_type, is_fn := sym.type.(Function_Type); is_fn {
                                        qualified_fn_type = fn_type
                                        break
                                    }
                                }
                            }
                        }
                    }
                    
                    // Skip declarations for standard library functions (already in system headers)
                    is_standard_lib_func := false
                    if fn_def.is_external && len(var_def.names) > 0 {
                        func_name := var_def.names[0]
                        standard_funcs := []string{"malloc", "free", "memcpy", "memmove", "memset", "exit", "printf", "sprintf", "fprintf", "strlen", "strcmp", "strcpy", "strcat"}
                        for std_func in standard_funcs {
                            if func_name == std_func {
                                is_standard_lib_func = true
                                break
                            }
                        }
                    }
                    
                    if !is_standard_lib_func {
                        if len(var_def.names) > 0 {
                            codegen_func_signature(ctx_cg, var_def.names[0], fn_def, qualified_fn_type)
                        }
                        strings.write_string(&ctx_cg.output_buf, ";\n")
                    }
                    
                    // Generate #define alias for external functions
                    if fn_def.is_external {
                        if ext_name, has_ext := fn_def.external_name.?; has_ext {
                            mangled_name: string
                            if ctx_cg.current_pkg_name != "" {
                                if len(var_def.names) > 0 {
                                    mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, var_def.names[0])
                                }
                            } else {
                                if len(var_def.names) > 0 {
                                    mangled_name = fmt.tprintf("%s%s", MANGLE_PREFIX, var_def.names[0])
                                }
                            }
                            fmt.sbprintf(&ctx_cg.output_buf, "#define %s %s\n", mangled_name, ext_name)
                        } else if !is_standard_lib_func {
                            mangled_name: string
                            if ctx_cg.current_pkg_name != "" {
                                if len(var_def.names) > 0 {
                                    mangled_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, var_def.names[0])
                                    fmt.sbprintf(&ctx_cg.output_buf, "#define %s %s\n", mangled_name, var_def.names[0])
                                }
                            } else {
                                if len(var_def.names) > 0 {
                                    mangled_name = fmt.tprintf("%s%s", MANGLE_PREFIX, var_def.names[0])
                                    fmt.sbprintf(&ctx_cg.output_buf, "#define %s %s\n", mangled_name, var_def.names[0])
                                }
                            }
                        }
                    }
                }

                // Forward declare structs and emit typedefs for aliases
                if var_def.content.node_kind == .Type_Expr {
                    type_expr := var_def.content.payload.(Node_Type_Expr)

                    if is_composite_type(type_expr.type_info) {
                        if len(var_def.names) > 0 {
                            codegen_composite_alias(ctx_cg, type_expr.type_info, var_def.names[0])
                        }
                    } else {
                        alias_c_name := ""
                        if ctx_cg.current_pkg_name != "" {
                            if len(var_def.names) > 0 {
                                alias_c_name = fmt.tprintf("%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, var_def.names[0])
                            }
                        } else {
                            if len(var_def.names) > 0 {
                                alias_c_name = fmt.tprintf("%s%s", MANGLE_PREFIX, var_def.names[0])
                            }
                        }

                        strings.write_string(&ctx_cg.output_buf, "typedef ")
                        if _, is_fn := type_expr.type_info.(Function_Type); is_fn {
                            codegen_type(ctx_cg, type_expr.type_info, alias_c_name)
                            strings.write_string(&ctx_cg.output_buf, ";\n")
                        } else {
                            codegen_type(ctx_cg, type_expr.type_info)
                            strings.write_string(&ctx_cg.output_buf, " ")
                            strings.write_string(&ctx_cg.output_buf, alias_c_name)
                            strings.write_string(&ctx_cg.output_buf, ";\n")
                        }
                    }
                }
            }
        }
    }
}

codegen_func_signature :: proc(ctx_cg: ^Codegen_Context, fn_name: string, node: Node_Fn_Def, qualified_type: Maybe(Function_Type) = nil) {
    // Special case for main, we alter the signature to match strict C main requirement
    if fn_name == "main" {
        strings.write_string(&ctx_cg.output_buf, "int32_t main(void)")
        return
    }

    // Use qualified type if available, otherwise fall back to node's type
    return_type := node.return_type
    param_types := node.params
    if fn_type, ok := qualified_type.?; ok {
        return_type = fn_type.return_type^
        // Note: params still come from node for names, but types from qualified_type
    }

    codegen_type(ctx_cg, return_type)
    strings.write_string(&ctx_cg.output_buf, " ")

    // Mangle unless external
    if node.is_external {
        // Declare with the C symbol name (external_name if provided)
        c_symbol_name := fn_name
        if ext_name, has_ext := node.external_name.?; has_ext {
            c_symbol_name = ext_name
        }
        strings.write_string(&ctx_cg.output_buf, c_symbol_name)
    } else {
        // Use current package name from context
        if ctx_cg.current_pkg_name != "" {
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s__%s", MANGLE_PREFIX, ctx_cg.current_pkg_name, fn_name)
        } else {
            fmt.sbprintf(&ctx_cg.output_buf, "%s%s", MANGLE_PREFIX, fn_name)
        }
    }

    strings.write_string(&ctx_cg.output_buf, "(")
    if len(node.params) == 0 {
        strings.write_string(&ctx_cg.output_buf, "void")
    } else {
        for param, i in node.params {
            // Use qualified param type if available
            param_type := param.type
            if fn_type, ok := qualified_type.?; ok && i < len(fn_type.params) {
                param_type = fn_type.params[i]
            }
            
            // Resolve named types to check if it's actually a function pointer
            resolved_param_type := param_type
            if named, is_named := param_type.(Named_Type); is_named {
                if resolved, ok := resolve_named_type(ctx_cg.ctx_sem, named, ctx_cg.current_pkg_name); ok {
                    resolved_param_type = resolved
                }
            }
            
            // For function pointer types, the name must be inside the type declaration
            if _, is_fn_ptr := resolved_param_type.(Function_Type); is_fn_ptr {
                codegen_type(ctx_cg, param_type, param.name)
            } else {
                codegen_type(ctx_cg, param_type)
                strings.write_string(&ctx_cg.output_buf, " ")
                strings.write_string(&ctx_cg.output_buf, param.name)
            }
            if i < len(node.params)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
    }
    strings.write_string(&ctx_cg.output_buf, ")")
}

emit_c_string_literal :: proc(ctx_cg: ^Codegen_Context, data: []i8) {
    strings.write_string(&ctx_cg.output_buf, "\"")
    for ch in data {
        ch_u8 := cast(u8)ch
        switch ch_u8 {
        case 10:
            strings.write_string(&ctx_cg.output_buf, "\\n")
        case 13:
            strings.write_string(&ctx_cg.output_buf, "\\r")
        case 9:
            strings.write_string(&ctx_cg.output_buf, "\\t")
        case 0:
            strings.write_string(&ctx_cg.output_buf, "\\0")
        case '"':
            strings.write_string(&ctx_cg.output_buf, "\\\"")
        case '\\':
            strings.write_string(&ctx_cg.output_buf, "\\\\")
        case:
            if ch_u8 < 32 || ch_u8 >= 127 {
                fmt.sbprintf(&ctx_cg.output_buf, "\\x%02X", ch_u8)
            } else {
                fmt.sbprintf(&ctx_cg.output_buf, "%c", ch_u8)
            }
        }
    }
    strings.write_string(&ctx_cg.output_buf, "\"")
}