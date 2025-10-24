package main 

import "core:mem"

Symbol :: struct {
    name: string,
    type: Type,
}

Scope :: struct {
    symbols: map[string]Symbol,
    parent: ^Scope
}

Semantic_Context :: struct {
    current_scope: ^Scope,
    errors: [dynamic]string,
    allocator: mem.Allocator,
}

push_scope :: proc(ctx: ^Semantic_Context) {
    scope := new(Scope, ctx.allocator)
    scope.parent = ctx.current_scope
    scope.symbols = make(map[string]Symbol, ctx.allocator)
    ctx.current_scope = scope
}

pop_scope :: proc(ctx: ^Semantic_Context) {
    old := ctx.current_scope
    ctx.current_scope = old.parent
    delete(old.symbols)
    free(old, ctx.allocator)
}

// Semantic analysis entry point. Should be called in, e.g., main.
semantic_analyze :: proc(root: ^Node, allocator := context.allocator) -> Semantic_Context {
    ctx := Semantic_Context {
        allocator=allocator,
        errors=make([dynamic]string, allocator)
    }

    push_scope(&ctx) // global
    semantic_analyze_node(&ctx, root)

    return ctx
}

semantic_free :: proc(ctx: ^Semantic_Context) {
    for ctx.current_scope != nil {
        pop_scope(ctx)
    }
    delete(ctx.errors)
}

semantic_analyze_node :: proc(ctx: ^Semantic_Context, node: ^Node) {
    if node == nil do return 

    // TODO
}