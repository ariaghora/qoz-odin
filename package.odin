package main

import "core:path/filepath"

Package_Info :: struct {
    dir: string,
    // List of files in this package (i.e., this dir)
    files: [dynamic]string,
    // Package directories this depends on
    imports: [dynamic]string,  
}

build_dependency_graph :: proc(asts: map[string]^Node, allocator := context.allocator) -> (packages: map[string]Package_Info, err: Parse_Error) {
    packages = make(map[string]Package_Info, allocator)
    
    // Group files by package (directory)
    for file_path, ast in asts {
        pkg_dir := filepath.dir(file_path, context.temp_allocator)
        
        if pkg_dir not_in packages {
            packages[pkg_dir] = Package_Info{
                dir = pkg_dir,
                files = make([dynamic]string, allocator),
                imports = make([dynamic]string, allocator),
            }
        }

        
        files := packages[pkg_dir].files
        append(&files, file_path)
        
        // Extract imports
        if ast.node_kind == .Program {
            for stmt in ast.payload.(Node_Statement_List).nodes {
                if stmt.node_kind == .Import {
                    import_node := stmt.payload.(Node_Import)
                    imported_pkg := resolve_package_path(pkg_dir, import_node.path)
                    
                    // Add to imports if not already there
                    found := false
                    for imp in packages[pkg_dir].imports {
                        if imp == imported_pkg {
                            found = true
                            break
                        }
                    }
                    if !found {
                        imports := packages[pkg_dir].imports
                        append(&imports, imported_pkg)
                    }
                }
            }
        }
    }
    
    return packages, nil
}

topological_sort_packages :: proc(packages: map[string]Package_Info, allocator := context.allocator) -> (sorted: []string, err: Parse_Error) {
    // Standard topological sort with cycle detection
    // Returns packages in dependency order (dependencies first)
    visited := make(map[string]bool, allocator)
    temp_mark := make(map[string]bool, allocator)
    result := make([dynamic]string, allocator)
    
    visit :: proc(pkg_dir: string, packages: map[string]Package_Info, visited, temp_mark: ^map[string]bool, result: ^[dynamic]string) -> Parse_Error {
        if temp_mark[pkg_dir] {
            return "Circular dependency detected"
        }
        if visited[pkg_dir] {
            return nil
        }
        
        temp_mark[pkg_dir] = true
        
        pkg_info := packages[pkg_dir]
        for dep in pkg_info.imports {
            visit(dep, packages, visited, temp_mark, result) or_return
        }
        
        temp_mark[pkg_dir] = false
        visited[pkg_dir] = true
        append(result, pkg_dir)
        
        return nil
    }
    
    for pkg_dir, _ in packages {
        visit(pkg_dir, packages, &visited, &temp_mark, &result) or_return
    }
    
    return result[:], nil
}