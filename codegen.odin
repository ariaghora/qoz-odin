package main

import "core:fmt"
import "core:mem"
import "core:strings"

Codegen_Context :: struct {
    output_buf: strings.Builder,
    indent_level: int,
    ctx_sem: ^Semantic_Context,
    func_nesting_depth: int, // TODO(Aria): track depth for lambda lifting
}

codegen :: proc(root: ^Node, ctx_sem: ^Semantic_Context, allocator := context.allocator) -> (res: string, err: mem.Allocator_Error) {
    sb := strings.builder_make(allocator) or_return
    ctx_cg := Codegen_Context { output_buf=sb, indent_level=0, ctx_sem=ctx_sem }

    // Source header
    strings.write_string(&ctx_cg.output_buf, "#include <stdint.h>\n#include <stdio.h>\n\n")
    // Pass 1: forward declaration
    codegen_forward_decl(&ctx_cg, root)
    strings.write_string(&ctx_cg.output_buf, "\n")
    // Pass 2: implementation
    codegen_node(&ctx_cg, root)

    return strings.to_string(ctx_cg.output_buf), nil
}

indented :: proc(content: string, n_space: int) -> string {
    return fmt.tprintf("%s%s", strings.repeat(" ", n_space), content)
}

codegen_indent :: proc(ctx_cg: ^Codegen_Context, level: int) {
    strings.write_string(&ctx_cg.output_buf, strings.repeat(" ", ctx_cg.indent_level * 4, context.temp_allocator))
}

codegen_node :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
    #partial switch node.node_kind {
    case .Program:
        for stmt in node.payload.(Node_Statement_List).nodes {
            codegen_node(ctx_cg, stmt)
        }

    case .Bin_Op:
        binop := node.payload.(Node_Bin_Op)
        strings.write_string(&ctx_cg.output_buf, "(")
        codegen_node(ctx_cg, binop.left)
        strings.write_string(&ctx_cg.output_buf, binop.op.source)
        codegen_node(ctx_cg, binop.right)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Fn_Def:
        fn_def := node.payload.(Node_Fn_Def)

    case .Fn_Call:
        fn_call := node.payload.(Node_Call)
        codegen_node(ctx_cg, fn_call.callee)
        strings.write_string(&ctx_cg.output_buf, "(")
        for arg, i in fn_call.args {
            codegen_node(ctx_cg, arg)
            if i < len(fn_call.args)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
        
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Identifier:
        iden := node.payload.(Node_Identifier)
        strings.write_string(&ctx_cg.output_buf, iden.name)
    
    case .Literal:
        lit := node.payload.(Node_Literal)
        src := lit.content.source
        strings.write_string(&ctx_cg.output_buf, src)

    case .Return:
        ret := node.payload.(Node_Return)
        strings.write_string(&ctx_cg.output_buf, "return ")
        codegen_node(ctx_cg, ret.value)

    case .Print:
        print := node.payload.(Node_Print)
        strings.write_string(&ctx_cg.output_buf, "printf(") 
        codegen_node(ctx_cg, print.content)
        strings.write_string(&ctx_cg.output_buf, ")")

    case .Var_Def:
        var_def := node.payload.(Node_Var_Def)

        //| Special case, where variable is function definition
        //+----------------------------------------------------
        if var_def.content.node_kind == .Fn_Def {
            if ctx_cg.func_nesting_depth > 0 {
                panic("TODO(Aria): implement lambda lifting for nested function")
            }

            fn_def := var_def.content.payload.(Node_Fn_Def)

            ctx_cg.func_nesting_depth += 1
            ctx_cg.indent_level += 1
            codegen_func_signature(ctx_cg, var_def.name, fn_def)
            strings.write_string(&ctx_cg.output_buf, " {\n")
            for stmt in fn_def.body {
                codegen_indent(ctx_cg, ctx_cg.indent_level)
                codegen_node(ctx_cg, stmt)
                strings.write_string(&ctx_cg.output_buf, ";\n")
            }
            strings.write_string(&ctx_cg.output_buf, "}\n\n")
            ctx_cg.func_nesting_depth -= 1
            ctx_cg.indent_level -= 1
        } else {
            //| Regular case
            //+-------------
            codegen_type(ctx_cg, node.inferred_type.(Type_Info))
            strings.write_string(&ctx_cg.output_buf, " ")
            strings.write_string(&ctx_cg.output_buf, var_def.name)
            strings.write_string(&ctx_cg.output_buf, " = ")
            codegen_node(ctx_cg, var_def.content)
        }
    case: fmt.panicf("Internal error: cannot generate code for node %v", node.node_kind)
    }
}

codegen_type :: proc(ctx_cg: ^Codegen_Context, type: Type_Info) {
    switch t in type {
    case Function_Type:
    case Primitive_Type:
        #partial switch t {
        case .I32: strings.write_string(&ctx_cg.output_buf, "int32_t")
        case .F32: strings.write_string(&ctx_cg.output_buf, "float")
        case .Void: strings.write_string(&ctx_cg.output_buf, "void")
        case: fmt.panicf("Internal error: cannot generate code for type %v", t)
        }
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
                    codegen_func_signature(ctx_cg, var_def.name, var_def.content.payload.(Node_Fn_Def))
                    strings.write_string(&ctx_cg.output_buf, ";\n")
                }
            }
        }
    }
}

codegen_fn_def :: proc(ctx_cg: ^Codegen_Context, node: ^Node) {
}

codegen_func_signature :: proc(ctx_cg: ^Codegen_Context, fn_name: string, node: Node_Fn_Def) {
    codegen_type(ctx_cg, node.return_type)
    strings.write_string(&ctx_cg.output_buf, " ")
    strings.write_string(&ctx_cg.output_buf, fn_name)
    strings.write_string(&ctx_cg.output_buf, "(")
    if len(node.params) == 0 {
        strings.write_string(&ctx_cg.output_buf, "void")
    } else {
        for param, i in node.params {
            codegen_type(ctx_cg, node.return_type)
            strings.write_string(&ctx_cg.output_buf, " ")
            strings.write_string(&ctx_cg.output_buf, param.name)
            if i < len(node.params)-1 do strings.write_string(&ctx_cg.output_buf, ", ")
        }
    }
    strings.write_string(&ctx_cg.output_buf, ")")
}