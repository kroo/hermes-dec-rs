# Building ASTs with Comments in Oxc: Comment Anchors Approach

## Overview

When manually building an AST with Oxc, attaching comments presents unique challenges because the comment system was designed for parsing existing source code, not programmatic AST construction. This guide presents the recommended solution using Comment Anchors.

## The Core Problem

### How Oxc's Comment System Works

1. **Comments are stored separately** from AST nodes in the `Program.comments` array
2. Each comment has an `attached_to` field that references a source position (`u32`)
3. During code generation, when visiting a node, codegen calls `print_comments_at(node.span.start)`
4. Comments with `attached_to == node.span.start` are printed at that location

### The Challenge with Manual AST Construction

When building ASTs programmatically:
- We typically use `SPAN` (Span::new(0, 0)) for all nodes
- We don't know the final source positions until after generation
- We may build nodes out of order compared to their final rendered position
- Nested structures make it hard to predict rendering order

### Example of the Problem

```rust
// Building an object literal with properties out of order
Object {  SPAN = 100, 200
  Key: "key1" SPAN = 400, 500, Value: "value1" SPAN = 500, 600,
  Key: "key2" SPAN = 200, 300, Value: "value2" SPAN = 300, 400
}
```

This causes issues because:
- Traversal order: Object → key1 → value1 → key2 → value2
- Span order: 100 → 400 → 500 → 200 → 300 (not monotonic!)
- Comments attached to position 200 might be skipped or misplaced

## Solution: Comment Anchors

The Comment Anchors approach separates comment management from position tracking entirely, providing a clean and intuitive API.

### Core Concept

Instead of trying to predict final source positions during AST construction, we:
1. Assign stable node IDs during building
2. Collect comments mapped to these IDs
3. Traverse the completed AST to determine final positions
4. Convert pending comments to real comments with proper positions

### Implementation

```rust
type NodeId = usize;

struct AstBuilderWithComments<'a> {
    builder: AstBuilder<'a>,
    next_node_id: NodeId,
    comments: HashMap<NodeId, Vec<PendingComment>>,
}

struct PendingComment {
    text: String,
    kind: CommentKind,
    position: CommentPosition,
}

impl<'a> AstBuilderWithComments<'a> {
    fn function_with_id(&mut self, name: &str) -> (Function<'a>, NodeId) {
        let id = self.next_node_id;
        self.next_node_id += 1;
        
        let func = self.builder.function(
            Span::new(id as u32, id as u32), // Temporary ID as span
            // ... other params
        );
        
        (func, id)
    }
    
    fn add_comment(&mut self, node_id: NodeId, text: &str, kind: CommentKind) {
        self.comments.entry(node_id).or_default().push(PendingComment {
            text: text.to_string(),
            kind,
            position: CommentPosition::Leading,
        });
    }
    
    fn finalize(self, mut program: Program<'a>) -> Program<'a> {
        // Traverse AST to map NodeId -> final position
        let mut visitor = PositionAssigner::new();
        visitor.visit_program(&mut program);
        
        // Convert pending comments to real comments
        let mut final_comments = Vec::new();
        for (node_id, pending) in self.comments {
            if let Some(&pos) = visitor.id_to_position.get(&node_id) {
                // Create comments with proper positions
                for (i, pc) in pending.iter().enumerate() {
                    final_comments.push(Comment {
                        span: calculate_comment_span(pos, i, pc.position),
                        attached_to: pos,
                        kind: pc.kind,
                        position: pc.position,
                        // ...
                    });
                }
            }
        }
        
        final_comments.sort_by_key(|c| c.span.start);
        program.comments = final_comments.into_boxed_slice();
        program
    }
}
```

### Position Assignment Visitor

```rust
use oxc_ast::visit::walk_mut::*;
use oxc_ast::visit::VisitMut;

struct PositionAssigner {
    current_pos: u32,
    id_to_position: HashMap<NodeId, u32>,
}

impl<'a> VisitMut<'a> for PositionAssigner {
    fn visit_function(&mut self, func: &mut Function<'a>) {
        // If span.start is used as temporary ID
        if func.span.start > 0 {
            let node_id = func.span.start as NodeId;
            self.id_to_position.insert(node_id, self.current_pos);
            func.span = Span::new(self.current_pos, self.current_pos + 1);
        }
        self.current_pos += 100; // Leave space for comments
        walk_function(self, func);
    }
    
    // Implement for other node types...
}
```

### Comment Span Calculation

```rust
fn calculate_comment_span(
    node_pos: u32, 
    comment_index: usize, 
    position: CommentPosition
) -> Span {
    match position {
        CommentPosition::Leading => {
            // Place comments before the node
            let offset = (comment_index as u32 + 1) * 10;
            let start = node_pos.saturating_sub(offset);
            Span::new(start, start + 5)
        }
        CommentPosition::Trailing => {
            // Place comments after the node
            let start = node_pos + 50 + (comment_index as u32 * 10);
            Span::new(start, start + 5)
        }
    }
}
```

## Complete Usage Example

```rust
// Create builder with comment support
let mut builder = AstBuilderWithComments::new(&allocator);

// Build function with comments
let (func, func_id) = builder.function_with_id("calculateSum");
builder.add_comment(func_id, "/**\n * Calculates sum of two numbers\n */", CommentKind::Block);

// Build return statement with comment
let (return_stmt, return_id) = builder.return_statement(expr);
builder.add_comment(return_id, "// Return the computed sum", CommentKind::Line);

// Add function body
builder.set_function_body(func_id, vec![return_stmt]);

// Build program
let program = builder.build_program(vec![func]);

// Finalize with proper comment positions
let program_with_comments = builder.finalize(program);

// Generate code - comments will appear in correct positions
let code = Codegen::new(&program_with_comments).build();
```

## Why This Approach Works Best

1. **Stable Node IDs**: Comments remain attached to the correct nodes regardless of construction order
2. **Clean API**: The wrapper provides intuitive methods for both AST building and comment attachment
3. **Single Pass**: Final position assignment happens in one traversal
4. **Extensible**: Easy to add more comment-related features
5. **Battle-tested pattern**: Similar approach used by Babel and other AST tools

## Key Implementation Details

### Node ID Strategy

- Use incremental IDs starting from a high number (e.g., 1_000_000) to avoid collision with real positions
- Store the ID temporarily in the node's span.start field
- Map IDs to nodes during building, positions during traversal

### Comment Types

Support different comment positions:
- **Leading**: Comments before the node
- **Trailing**: Comments after the node on the same line
- **Inner**: Comments inside block statements

### Memory Efficiency

- Use a sparse HashMap for comments (most nodes won't have comments)
- Consider using a Vec<(NodeId, PendingComment)> if memory is critical
- Clear temporary data structures after finalization

## Advanced Features

### JSDoc Support

```rust
impl<'a> AstBuilderWithComments<'a> {
    fn add_jsdoc(&mut self, node_id: NodeId, params: Vec<JsDocParam>, returns: &str) {
        let jsdoc = format!(
            "/**\n{}\n */",
            // Format JSDoc content
        );
        self.add_comment(node_id, &jsdoc, CommentKind::Block);
    }
}
```

### Comment Preservation

When transforming existing ASTs:

```rust
fn transform_with_comments(original: &Program) -> Program {
    let mut builder = AstBuilderWithComments::new(&allocator);
    
    // During transformation, preserve original comments
    for comment in &original.comments {
        // Map old positions to new node IDs
    }
    
    // Transform and build new AST...
}
```

## Debugging Tips

1. **Temporary Logging**: Add debug prints to track ID assignments
2. **Visualization**: Generate a map of NodeId → generated position → comment text
3. **Test Cases**: Create tests with complex nesting to verify ordering
4. **Assertions**: Add checks that final positions are monotonic

## Common Pitfalls to Avoid

1. **Forgetting to increment node IDs**: Will cause comment collisions
2. **Not leaving position gaps**: Comments may overlap with nodes
3. **Incorrect traversal implementation**: Must visit nodes in rendering order
4. **Missing node types**: Ensure all AST node types are handled in the visitor

## Conclusion

The Comment Anchors approach provides a robust solution for attaching comments to manually constructed ASTs in Oxc. By separating the concerns of AST building and position assignment, it enables clean, maintainable code that correctly handles comments regardless of construction order.