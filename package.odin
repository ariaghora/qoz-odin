package main

import "core:path/filepath"
import "core:slice"

Package_Info :: struct {
    dir: string,
    // List of files in this package (i.e., this dir)
    files: [dynamic]string,
    // Package directories this depends on
    imports: [dynamic]string,  
}

build_dependency_graph :: proc(asts: map[string]^Node, entry_package_dir: string, allocator := context.allocator) -> (packages: map[string]Package_Info, err: Maybe(Parse_Error)) {
    packages = make(map[string]Package_Info, allocator)
    
    // Group files by package (directory)
    for file_path, ast in asts {
        pkg_dir := filepath.dir(file_path, context.temp_allocator)
        // Normalize the path to ensure consistent matching
        pkg_dir = filepath.clean(pkg_dir, context.temp_allocator)
        
        if pkg_dir not_in packages {
            packages[pkg_dir] = Package_Info{
                dir = pkg_dir,
                files = make([dynamic]string, allocator),
                imports = make([dynamic]string, allocator),
            }
        }

        // Get pointer to modify in place
        pkg_info := &packages[pkg_dir]
        append(&pkg_info.files, file_path)
        
        // Extract imports
        if ast.node_kind == .Program {
            for stmt in ast.payload.(Node_Statement_List).nodes {
                if stmt.node_kind == .Import {
                    import_node := stmt.payload.(Node_Import)
                    // Use resolve_import_path to properly handle vendor: and std: imports
                    current_file_dir := filepath.dir(file_path, context.temp_allocator)
                    imported_pkg := resolve_import_path(current_file_dir, entry_package_dir, import_node.path)
                    // Normalize the path to ensure consistent matching
                    imported_pkg = filepath.clean(imported_pkg, context.temp_allocator)
                    
                    // Ensure imported package is in packages map (even if it has no files yet)
                    if imported_pkg not_in packages {
                        packages[imported_pkg] = Package_Info{
                            dir = imported_pkg,
                            files = make([dynamic]string, allocator),
                            imports = make([dynamic]string, allocator),
                        }
                    }
                    
                    // Add to imports if not already there
                    found := false
                    for imp in pkg_info.imports {
                        if imp == imported_pkg {
                            found = true
                            break
                        }
                    }
                    if !found {
                        append(&pkg_info.imports, imported_pkg)
                    }
                }
            }
        }
    }
    
    return packages, nil
}

topological_sort_packages :: proc(packages: map[string]Package_Info, allocator := context.allocator) -> (sorted: []string, err: Maybe(Parse_Error)) {
    // Standard topological sort with cycle detection
    // Returns packages in dependency order (dependencies first)
    visited := make(map[string]bool, allocator)
    temp_mark := make(map[string]bool, allocator)
    result := make([dynamic]string, allocator)
    
    visit :: proc(pkg_dir: string, packages: map[string]Package_Info, visited, temp_mark: ^map[string]bool, result: ^[dynamic]string) -> Maybe(Parse_Error) {
        if temp_mark[pkg_dir] {
            return make_parse_error_simple("Circular dependency detected", pkg_dir, 0, 0)
        }
        if visited[pkg_dir] {
            return nil
        }
        
        temp_mark[pkg_dir] = true
        
        // Only process if package exists in map
        pkg_info, pkg_exists := packages[pkg_dir]
        if !pkg_exists {
            temp_mark[pkg_dir] = false
            return nil
        }
        
        for dep in pkg_info.imports {
            // Only visit dependencies that exist in packages map
            if dep in packages {
                visit(dep, packages, visited, temp_mark, result) or_return
            }
        }
        
        temp_mark[pkg_dir] = false
        visited[pkg_dir] = true
        append(result, pkg_dir)
        
        return nil
    }
    
    // Sort package dirs for deterministic iteration
    pkg_dirs := make([dynamic]string, allocator)
    for pkg_dir in packages {
        append(&pkg_dirs, pkg_dir)
    }
    slice.sort(pkg_dirs[:])
    
    for pkg_dir in pkg_dirs {
        visit(pkg_dir, packages, &visited, &temp_mark, &result) or_return
    }
    
    return result[:], nil
}