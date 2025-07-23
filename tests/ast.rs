use hermes_dec_rs::ast::AstBuilder;
use oxc_allocator::Allocator;

#[test]
fn test_ast_builder_creation() {
    let allocator = Allocator::default();
    let builder = AstBuilder::new(&allocator);
    assert_eq!(builder.counter, 0);
}

#[test]
fn test_ast_builder_generate_id() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let id1 = builder.generate_id();
    let id2 = builder.generate_id();
    assert_eq!(id1, "_var1");
    assert_eq!(id2, "_var2");
    assert_eq!(builder.counter, 2);
}

#[test]
fn test_ast_builder_create_numeric_literal() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let expr = builder.create_numeric_literal(42.0);
    assert_eq!(expr, "42");
}

#[test]
fn test_ast_builder_create_string_literal() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let expr = builder.create_string_literal("hello");
    assert_eq!(expr, "\"hello\"");
}

#[test]
fn test_ast_builder_create_binary_expression() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let left = builder.create_numeric_literal(1.0);
    let right = builder.create_numeric_literal(2.0);
    let expr = builder.create_binary_expression(&left, "+", &right);
    assert_eq!(expr, "(1 + 2)");
}

#[test]
fn test_ast_builder_create_variable_declaration() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let value = builder.create_numeric_literal(42.0);
    let decl = builder.create_variable_declaration("x", Some(&value));
    assert_eq!(decl, "var x = 42;");
}

#[test]
fn test_ast_builder_create_expression_statement() {
    let allocator = Allocator::default();
    let mut builder = AstBuilder::new(&allocator);
    let expr = builder.create_binary_expression("a", "+", "b");
    let stmt = builder.create_expression_statement(&expr);
    assert_eq!(stmt, "(a + b);");
}
