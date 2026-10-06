//! Bounded syntax only. No evaluation, scope resolution, or instruction/PC inference.
use std::collections::{BTreeSet, HashMap, HashSet};

use oxc_ast::{ast::*, AstKind};
use oxc_ast_visit::Visit;
use oxc_span::{GetSpan, Span};
use serde_json::{json, Value};

use crate::{DecompilerError, DecompilerResult};

const SOURCE_BYTES: usize = 64 * 1024 * 1024;
const NODES: usize = 128;
const DEPTH: usize = 16;
const ITEMS: usize = 32;
const GLOBAL_NODES: usize = 4096;
// Charges visitor frames, including enum/collection wrappers, not just AST nodes.
const WORK: usize = 32_000_000;
// The upstream visitor is recursive. Fail closed before exhausting its stack.
const VISITOR_DEPTH: usize = 256;

fn error(message: &str) -> DecompilerError {
    DecompilerError::Internal {
        message: format!("expression projection: {message}"),
    }
}

fn valid(source: &str, span: Span) -> bool {
    span.start <= span.end && source.get(span.start as usize..span.end as usize).is_some()
}

fn location(span: Span) -> Value {
    json!({"start": span.start, "end": span.end})
}

#[derive(Clone, Copy)]
enum Syntax<'a> {
    Expression(&'a Expression<'a>),
    Assignment(&'a AssignmentExpression<'a>),
    Update(&'a UpdateExpression<'a>),
    Target(&'a SimpleAssignmentTarget<'a>),
    Member(&'a MemberExpression<'a>, bool),
    Leaf(Span, &'static str),
    Spread(&'a SpreadElement<'a>),
}

impl Syntax<'_> {
    fn span(self) -> Span {
        match self {
            Self::Expression(e) => e.span(),
            Self::Assignment(e) => e.span,
            Self::Update(e) => e.span,
            Self::Target(e) => e.span(),
            Self::Member(e, _) => e.span(),
            Self::Leaf(span, _) => span,
            Self::Spread(e) => e.span,
        }
    }
}

struct Index<'a, 's> {
    source: &'s str,
    wanted: HashSet<(u32, u32)>,
    selected: HashMap<(u32, u32), Syntax<'a>>,
    work: usize,
    depth: usize,
    error: Option<&'static str>,
}

impl<'a> Index<'a, '_> {
    fn begin(&mut self) -> bool {
        if self.error.is_some() {
            return false;
        }
        if self.work == WORK {
            self.error = Some("complete AST visitor work cap");
            return false;
        }
        if self.depth == VISITOR_DEPTH {
            self.error = Some("complete AST visitor depth cap");
            return false;
        }
        self.work += 1;
        self.depth += 1;
        true
    }

    fn select(&mut self, node: Syntax<'a>) {
        let span = node.span();
        if self.wanted.contains(&(span.start, span.end)) {
            self.selected.entry((span.start, span.end)).or_insert(node);
        }
    }
}

// Gate every upstream visitor entry point, including inherited expression
// variants and collections. Collection loops stop immediately on stored error.
macro_rules! gated_walk {
    ($visit:ident, $ty:ty, $walk:ident) => {
        fn $visit(&mut self, it: &$ty) {
            if self.begin() {
                oxc_ast_visit::walk::$walk(self, it);
                self.depth -= 1;
            }
        }
    };
}

macro_rules! gated_expressions {
    ($visit:ident, $ty:ty, $walk:ident) => {
        fn $visit(&mut self, it: &$ty) {
            if self.begin() {
                if let Some(expression) = it.as_expression() {
                    self.select(Syntax::Expression(self.alloc(expression)));
                }
                oxc_ast_visit::walk::$walk(self, it);
                self.depth -= 1;
            }
        }
    };
}

macro_rules! gated_collection {
    ($visit:ident, $ty:ty, $child:ident) => {
        fn $visit(&mut self, it: &oxc_allocator::Vec<'a, $ty>) {
            if self.begin() {
                for item in it {
                    if self.error.is_some() {
                        break;
                    }
                    self.$child(item);
                }
                self.depth -= 1;
            }
        }
    };
}

macro_rules! gated_optional_elements {
    ($visit:ident, $ty:ty, $kind:ident, $child:ident, $rest:ident) => {
        fn $visit(&mut self, it: &$ty) {
            if self.begin() {
                self.enter_node(AstKind::$kind(self.alloc(it)));
                self.visit_span(&it.span);
                for element in &it.elements {
                    // Charge holes too; flatten() alone can skip unbounded work.
                    if !self.begin() {
                        break;
                    }
                    if let Some(element) = element {
                        self.$child(element);
                    }
                    self.depth -= 1;
                }
                if let Some(rest) = &it.rest {
                    self.$rest(rest);
                }
                self.depth -= 1;
            }
        }
    };
}

impl<'a> Visit<'a> for Index<'a, '_> {
    fn enter_node(&mut self, kind: AstKind<'a>) {
        if !valid(self.source, kind.span()) {
            self.error = Some("malformed AST source span");
            return;
        }
        let selected = match kind {
            AstKind::AssignmentExpression(e) => Some(Syntax::Assignment(e)),
            AstKind::UpdateExpression(e) => Some(Syntax::Update(e)),
            _ => None,
        };
        if let Some(node) = selected {
            self.select(node);
        }
    }

    fn visit_expression(&mut self, it: &Expression<'a>) {
        if self.begin() {
            self.select(Syntax::Expression(self.alloc(it)));
            oxc_ast_visit::walk::walk_expression(self, it);
            self.depth -= 1;
        }
    }

    fn visit_function(&mut self, it: &Function<'a>, flags: oxc_syntax::scope::ScopeFlags) {
        if self.begin() {
            oxc_ast_visit::walk::walk_function(self, it, flags);
            self.depth -= 1;
        }
    }

    fn visit_span(&mut self, it: &Span) {
        if self.begin() {
            if !valid(self.source, *it) {
                self.error = Some("malformed AST source span");
            }
            self.depth -= 1;
        }
    }

    gated_walk!(visit_program, Program<'a>, walk_program);
    gated_walk!(
        visit_identifier_name,
        IdentifierName<'a>,
        walk_identifier_name
    );
    gated_walk!(
        visit_identifier_reference,
        IdentifierReference<'a>,
        walk_identifier_reference
    );
    gated_walk!(
        visit_binding_identifier,
        BindingIdentifier<'a>,
        walk_binding_identifier
    );
    gated_walk!(
        visit_label_identifier,
        LabelIdentifier<'a>,
        walk_label_identifier
    );
    gated_walk!(visit_this_expression, ThisExpression, walk_this_expression);
    gated_walk!(
        visit_array_expression,
        ArrayExpression<'a>,
        walk_array_expression
    );
    gated_expressions!(
        visit_array_expression_element,
        ArrayExpressionElement<'a>,
        walk_array_expression_element
    );
    gated_walk!(visit_elision, Elision, walk_elision);
    gated_walk!(
        visit_object_expression,
        ObjectExpression<'a>,
        walk_object_expression
    );
    gated_walk!(
        visit_object_property_kind,
        ObjectPropertyKind<'a>,
        walk_object_property_kind
    );
    gated_walk!(
        visit_object_property,
        ObjectProperty<'a>,
        walk_object_property
    );
    gated_expressions!(visit_property_key, PropertyKey<'a>, walk_property_key);
    gated_walk!(
        visit_template_literal,
        TemplateLiteral<'a>,
        walk_template_literal
    );
    gated_walk!(
        visit_tagged_template_expression,
        TaggedTemplateExpression<'a>,
        walk_tagged_template_expression
    );
    gated_walk!(
        visit_template_element,
        TemplateElement<'a>,
        walk_template_element
    );
    gated_walk!(
        visit_member_expression,
        MemberExpression<'a>,
        walk_member_expression
    );
    gated_walk!(
        visit_computed_member_expression,
        ComputedMemberExpression<'a>,
        walk_computed_member_expression
    );
    gated_walk!(
        visit_static_member_expression,
        StaticMemberExpression<'a>,
        walk_static_member_expression
    );
    gated_walk!(
        visit_private_field_expression,
        PrivateFieldExpression<'a>,
        walk_private_field_expression
    );
    gated_walk!(
        visit_call_expression,
        CallExpression<'a>,
        walk_call_expression
    );
    gated_walk!(visit_new_expression, NewExpression<'a>, walk_new_expression);
    gated_walk!(visit_meta_property, MetaProperty<'a>, walk_meta_property);
    gated_walk!(visit_spread_element, SpreadElement<'a>, walk_spread_element);
    gated_expressions!(visit_argument, Argument<'a>, walk_argument);
    gated_walk!(
        visit_update_expression,
        UpdateExpression<'a>,
        walk_update_expression
    );
    gated_walk!(
        visit_unary_expression,
        UnaryExpression<'a>,
        walk_unary_expression
    );
    gated_walk!(
        visit_binary_expression,
        BinaryExpression<'a>,
        walk_binary_expression
    );
    gated_walk!(
        visit_private_in_expression,
        PrivateInExpression<'a>,
        walk_private_in_expression
    );
    gated_walk!(
        visit_logical_expression,
        LogicalExpression<'a>,
        walk_logical_expression
    );
    gated_walk!(
        visit_conditional_expression,
        ConditionalExpression<'a>,
        walk_conditional_expression
    );
    gated_walk!(
        visit_assignment_expression,
        AssignmentExpression<'a>,
        walk_assignment_expression
    );
    gated_walk!(
        visit_assignment_target,
        AssignmentTarget<'a>,
        walk_assignment_target
    );
    gated_walk!(
        visit_simple_assignment_target,
        SimpleAssignmentTarget<'a>,
        walk_simple_assignment_target
    );
    gated_walk!(
        visit_assignment_target_pattern,
        AssignmentTargetPattern<'a>,
        walk_assignment_target_pattern
    );
    gated_optional_elements!(
        visit_array_assignment_target,
        ArrayAssignmentTarget<'a>,
        ArrayAssignmentTarget,
        visit_assignment_target_maybe_default,
        visit_assignment_target_rest
    );
    gated_walk!(
        visit_object_assignment_target,
        ObjectAssignmentTarget<'a>,
        walk_object_assignment_target
    );
    gated_walk!(
        visit_assignment_target_rest,
        AssignmentTargetRest<'a>,
        walk_assignment_target_rest
    );
    gated_walk!(
        visit_assignment_target_maybe_default,
        AssignmentTargetMaybeDefault<'a>,
        walk_assignment_target_maybe_default
    );
    gated_walk!(
        visit_assignment_target_with_default,
        AssignmentTargetWithDefault<'a>,
        walk_assignment_target_with_default
    );
    gated_walk!(
        visit_assignment_target_property,
        AssignmentTargetProperty<'a>,
        walk_assignment_target_property
    );
    gated_walk!(
        visit_assignment_target_property_identifier,
        AssignmentTargetPropertyIdentifier<'a>,
        walk_assignment_target_property_identifier
    );
    gated_walk!(
        visit_assignment_target_property_property,
        AssignmentTargetPropertyProperty<'a>,
        walk_assignment_target_property_property
    );
    gated_walk!(
        visit_sequence_expression,
        SequenceExpression<'a>,
        walk_sequence_expression
    );
    gated_walk!(visit_super, Super, walk_super);
    gated_walk!(
        visit_await_expression,
        AwaitExpression<'a>,
        walk_await_expression
    );
    gated_walk!(
        visit_chain_expression,
        ChainExpression<'a>,
        walk_chain_expression
    );
    gated_walk!(visit_chain_element, ChainElement<'a>, walk_chain_element);
    gated_walk!(
        visit_parenthesized_expression,
        ParenthesizedExpression<'a>,
        walk_parenthesized_expression
    );
    gated_walk!(visit_statement, Statement<'a>, walk_statement);
    gated_walk!(visit_directive, Directive<'a>, walk_directive);
    gated_walk!(visit_hashbang, Hashbang<'a>, walk_hashbang);
    gated_walk!(
        visit_block_statement,
        BlockStatement<'a>,
        walk_block_statement
    );
    gated_walk!(visit_declaration, Declaration<'a>, walk_declaration);
    gated_walk!(
        visit_variable_declaration,
        VariableDeclaration<'a>,
        walk_variable_declaration
    );
    gated_walk!(
        visit_variable_declarator,
        VariableDeclarator<'a>,
        walk_variable_declarator
    );
    gated_walk!(visit_empty_statement, EmptyStatement, walk_empty_statement);
    gated_walk!(
        visit_expression_statement,
        ExpressionStatement<'a>,
        walk_expression_statement
    );
    gated_walk!(visit_if_statement, IfStatement<'a>, walk_if_statement);
    gated_walk!(
        visit_do_while_statement,
        DoWhileStatement<'a>,
        walk_do_while_statement
    );
    gated_walk!(
        visit_while_statement,
        WhileStatement<'a>,
        walk_while_statement
    );
    gated_walk!(visit_for_statement, ForStatement<'a>, walk_for_statement);
    gated_expressions!(
        visit_for_statement_init,
        ForStatementInit<'a>,
        walk_for_statement_init
    );
    gated_walk!(
        visit_for_in_statement,
        ForInStatement<'a>,
        walk_for_in_statement
    );
    gated_walk!(
        visit_for_statement_left,
        ForStatementLeft<'a>,
        walk_for_statement_left
    );
    gated_walk!(
        visit_for_of_statement,
        ForOfStatement<'a>,
        walk_for_of_statement
    );
    gated_walk!(
        visit_continue_statement,
        ContinueStatement<'a>,
        walk_continue_statement
    );
    gated_walk!(
        visit_break_statement,
        BreakStatement<'a>,
        walk_break_statement
    );
    gated_walk!(
        visit_return_statement,
        ReturnStatement<'a>,
        walk_return_statement
    );
    gated_walk!(visit_with_statement, WithStatement<'a>, walk_with_statement);
    gated_walk!(
        visit_switch_statement,
        SwitchStatement<'a>,
        walk_switch_statement
    );
    gated_walk!(visit_switch_case, SwitchCase<'a>, walk_switch_case);
    gated_walk!(
        visit_labeled_statement,
        LabeledStatement<'a>,
        walk_labeled_statement
    );
    gated_walk!(
        visit_throw_statement,
        ThrowStatement<'a>,
        walk_throw_statement
    );
    gated_walk!(visit_try_statement, TryStatement<'a>, walk_try_statement);
    gated_walk!(visit_catch_clause, CatchClause<'a>, walk_catch_clause);
    gated_walk!(
        visit_catch_parameter,
        CatchParameter<'a>,
        walk_catch_parameter
    );
    gated_walk!(
        visit_debugger_statement,
        DebuggerStatement,
        walk_debugger_statement
    );
    gated_walk!(
        visit_binding_pattern,
        BindingPattern<'a>,
        walk_binding_pattern
    );
    gated_walk!(
        visit_binding_pattern_kind,
        BindingPatternKind<'a>,
        walk_binding_pattern_kind
    );
    gated_walk!(
        visit_assignment_pattern,
        AssignmentPattern<'a>,
        walk_assignment_pattern
    );
    gated_walk!(visit_object_pattern, ObjectPattern<'a>, walk_object_pattern);
    gated_walk!(
        visit_binding_property,
        BindingProperty<'a>,
        walk_binding_property
    );
    gated_optional_elements!(
        visit_array_pattern,
        ArrayPattern<'a>,
        ArrayPattern,
        visit_binding_pattern,
        visit_binding_rest_element
    );
    gated_walk!(
        visit_binding_rest_element,
        BindingRestElement<'a>,
        walk_binding_rest_element
    );
    gated_walk!(
        visit_formal_parameters,
        FormalParameters<'a>,
        walk_formal_parameters
    );
    gated_walk!(
        visit_formal_parameter,
        FormalParameter<'a>,
        walk_formal_parameter
    );
    gated_walk!(visit_function_body, FunctionBody<'a>, walk_function_body);
    gated_walk!(
        visit_arrow_function_expression,
        ArrowFunctionExpression<'a>,
        walk_arrow_function_expression
    );
    gated_walk!(
        visit_yield_expression,
        YieldExpression<'a>,
        walk_yield_expression
    );
    gated_walk!(visit_class, Class<'a>, walk_class);
    gated_walk!(visit_class_body, ClassBody<'a>, walk_class_body);
    gated_walk!(visit_class_element, ClassElement<'a>, walk_class_element);
    gated_walk!(
        visit_method_definition,
        MethodDefinition<'a>,
        walk_method_definition
    );
    gated_walk!(
        visit_property_definition,
        PropertyDefinition<'a>,
        walk_property_definition
    );
    gated_walk!(
        visit_private_identifier,
        PrivateIdentifier<'a>,
        walk_private_identifier
    );
    gated_walk!(visit_static_block, StaticBlock<'a>, walk_static_block);
    gated_walk!(
        visit_module_declaration,
        ModuleDeclaration<'a>,
        walk_module_declaration
    );
    gated_walk!(
        visit_accessor_property,
        AccessorProperty<'a>,
        walk_accessor_property
    );
    gated_walk!(
        visit_import_expression,
        ImportExpression<'a>,
        walk_import_expression
    );
    gated_walk!(
        visit_import_declaration,
        ImportDeclaration<'a>,
        walk_import_declaration
    );
    gated_walk!(
        visit_import_declaration_specifier,
        ImportDeclarationSpecifier<'a>,
        walk_import_declaration_specifier
    );
    gated_walk!(
        visit_import_specifier,
        ImportSpecifier<'a>,
        walk_import_specifier
    );
    gated_walk!(
        visit_import_default_specifier,
        ImportDefaultSpecifier<'a>,
        walk_import_default_specifier
    );
    gated_walk!(
        visit_import_namespace_specifier,
        ImportNamespaceSpecifier<'a>,
        walk_import_namespace_specifier
    );
    gated_walk!(visit_with_clause, WithClause<'a>, walk_with_clause);
    gated_walk!(
        visit_import_attribute,
        ImportAttribute<'a>,
        walk_import_attribute
    );
    gated_walk!(
        visit_import_attribute_key,
        ImportAttributeKey<'a>,
        walk_import_attribute_key
    );
    gated_walk!(
        visit_export_named_declaration,
        ExportNamedDeclaration<'a>,
        walk_export_named_declaration
    );
    gated_walk!(
        visit_export_default_declaration,
        ExportDefaultDeclaration<'a>,
        walk_export_default_declaration
    );
    gated_walk!(
        visit_export_all_declaration,
        ExportAllDeclaration<'a>,
        walk_export_all_declaration
    );
    gated_walk!(
        visit_export_specifier,
        ExportSpecifier<'a>,
        walk_export_specifier
    );
    gated_expressions!(
        visit_export_default_declaration_kind,
        ExportDefaultDeclarationKind<'a>,
        walk_export_default_declaration_kind
    );
    gated_walk!(
        visit_module_export_name,
        ModuleExportName<'a>,
        walk_module_export_name
    );
    gated_walk!(
        visit_v_8_intrinsic_expression,
        V8IntrinsicExpression<'a>,
        walk_v_8_intrinsic_expression
    );
    gated_walk!(visit_boolean_literal, BooleanLiteral, walk_boolean_literal);
    gated_walk!(visit_null_literal, NullLiteral, walk_null_literal);
    gated_walk!(
        visit_numeric_literal,
        NumericLiteral<'a>,
        walk_numeric_literal
    );
    gated_walk!(visit_string_literal, StringLiteral<'a>, walk_string_literal);
    gated_walk!(
        visit_big_int_literal,
        BigIntLiteral<'a>,
        walk_big_int_literal
    );
    gated_walk!(
        visit_reg_exp_literal,
        RegExpLiteral<'a>,
        walk_reg_exp_literal
    );
    gated_walk!(visit_jsx_element, JSXElement<'a>, walk_jsx_element);
    gated_walk!(
        visit_jsx_opening_element,
        JSXOpeningElement<'a>,
        walk_jsx_opening_element
    );
    gated_walk!(
        visit_jsx_closing_element,
        JSXClosingElement<'a>,
        walk_jsx_closing_element
    );
    gated_walk!(visit_jsx_fragment, JSXFragment<'a>, walk_jsx_fragment);
    gated_walk!(
        visit_jsx_opening_fragment,
        JSXOpeningFragment,
        walk_jsx_opening_fragment
    );
    gated_walk!(
        visit_jsx_closing_fragment,
        JSXClosingFragment,
        walk_jsx_closing_fragment
    );
    gated_walk!(
        visit_jsx_element_name,
        JSXElementName<'a>,
        walk_jsx_element_name
    );
    gated_walk!(
        visit_jsx_namespaced_name,
        JSXNamespacedName<'a>,
        walk_jsx_namespaced_name
    );
    gated_walk!(
        visit_jsx_member_expression,
        JSXMemberExpression<'a>,
        walk_jsx_member_expression
    );
    gated_walk!(
        visit_jsx_member_expression_object,
        JSXMemberExpressionObject<'a>,
        walk_jsx_member_expression_object
    );
    gated_walk!(
        visit_jsx_expression_container,
        JSXExpressionContainer<'a>,
        walk_jsx_expression_container
    );
    gated_expressions!(visit_jsx_expression, JSXExpression<'a>, walk_jsx_expression);
    gated_walk!(
        visit_jsx_empty_expression,
        JSXEmptyExpression,
        walk_jsx_empty_expression
    );
    gated_walk!(
        visit_jsx_attribute_item,
        JSXAttributeItem<'a>,
        walk_jsx_attribute_item
    );
    gated_walk!(visit_jsx_attribute, JSXAttribute<'a>, walk_jsx_attribute);
    gated_walk!(
        visit_jsx_spread_attribute,
        JSXSpreadAttribute<'a>,
        walk_jsx_spread_attribute
    );
    gated_walk!(
        visit_jsx_attribute_name,
        JSXAttributeName<'a>,
        walk_jsx_attribute_name
    );
    gated_walk!(
        visit_jsx_attribute_value,
        JSXAttributeValue<'a>,
        walk_jsx_attribute_value
    );
    gated_walk!(visit_jsx_identifier, JSXIdentifier<'a>, walk_jsx_identifier);
    gated_walk!(visit_jsx_child, JSXChild<'a>, walk_jsx_child);
    gated_walk!(
        visit_jsx_spread_child,
        JSXSpreadChild<'a>,
        walk_jsx_spread_child
    );
    gated_walk!(visit_jsx_text, JSXText<'a>, walk_jsx_text);
    gated_walk!(
        visit_ts_this_parameter,
        TSThisParameter<'a>,
        walk_ts_this_parameter
    );
    gated_walk!(
        visit_ts_enum_declaration,
        TSEnumDeclaration<'a>,
        walk_ts_enum_declaration
    );
    gated_walk!(visit_ts_enum_body, TSEnumBody<'a>, walk_ts_enum_body);
    gated_walk!(visit_ts_enum_member, TSEnumMember<'a>, walk_ts_enum_member);
    gated_walk!(
        visit_ts_enum_member_name,
        TSEnumMemberName<'a>,
        walk_ts_enum_member_name
    );
    gated_walk!(
        visit_ts_type_annotation,
        TSTypeAnnotation<'a>,
        walk_ts_type_annotation
    );
    gated_walk!(
        visit_ts_literal_type,
        TSLiteralType<'a>,
        walk_ts_literal_type
    );
    gated_walk!(visit_ts_literal, TSLiteral<'a>, walk_ts_literal);
    gated_walk!(visit_ts_type, TSType<'a>, walk_ts_type);
    gated_walk!(
        visit_ts_conditional_type,
        TSConditionalType<'a>,
        walk_ts_conditional_type
    );
    gated_walk!(visit_ts_union_type, TSUnionType<'a>, walk_ts_union_type);
    gated_walk!(
        visit_ts_intersection_type,
        TSIntersectionType<'a>,
        walk_ts_intersection_type
    );
    gated_walk!(
        visit_ts_parenthesized_type,
        TSParenthesizedType<'a>,
        walk_ts_parenthesized_type
    );
    gated_walk!(
        visit_ts_type_operator,
        TSTypeOperator<'a>,
        walk_ts_type_operator
    );
    gated_walk!(visit_ts_array_type, TSArrayType<'a>, walk_ts_array_type);
    gated_walk!(
        visit_ts_indexed_access_type,
        TSIndexedAccessType<'a>,
        walk_ts_indexed_access_type
    );
    gated_walk!(visit_ts_tuple_type, TSTupleType<'a>, walk_ts_tuple_type);
    gated_walk!(
        visit_ts_named_tuple_member,
        TSNamedTupleMember<'a>,
        walk_ts_named_tuple_member
    );
    gated_walk!(
        visit_ts_optional_type,
        TSOptionalType<'a>,
        walk_ts_optional_type
    );
    gated_walk!(visit_ts_rest_type, TSRestType<'a>, walk_ts_rest_type);
    gated_walk!(
        visit_ts_tuple_element,
        TSTupleElement<'a>,
        walk_ts_tuple_element
    );
    gated_walk!(visit_ts_any_keyword, TSAnyKeyword, walk_ts_any_keyword);
    gated_walk!(
        visit_ts_string_keyword,
        TSStringKeyword,
        walk_ts_string_keyword
    );
    gated_walk!(
        visit_ts_boolean_keyword,
        TSBooleanKeyword,
        walk_ts_boolean_keyword
    );
    gated_walk!(
        visit_ts_number_keyword,
        TSNumberKeyword,
        walk_ts_number_keyword
    );
    gated_walk!(
        visit_ts_never_keyword,
        TSNeverKeyword,
        walk_ts_never_keyword
    );
    gated_walk!(
        visit_ts_intrinsic_keyword,
        TSIntrinsicKeyword,
        walk_ts_intrinsic_keyword
    );
    gated_walk!(
        visit_ts_unknown_keyword,
        TSUnknownKeyword,
        walk_ts_unknown_keyword
    );
    gated_walk!(visit_ts_null_keyword, TSNullKeyword, walk_ts_null_keyword);
    gated_walk!(
        visit_ts_undefined_keyword,
        TSUndefinedKeyword,
        walk_ts_undefined_keyword
    );
    gated_walk!(visit_ts_void_keyword, TSVoidKeyword, walk_ts_void_keyword);
    gated_walk!(
        visit_ts_symbol_keyword,
        TSSymbolKeyword,
        walk_ts_symbol_keyword
    );
    gated_walk!(visit_ts_this_type, TSThisType, walk_ts_this_type);
    gated_walk!(
        visit_ts_object_keyword,
        TSObjectKeyword,
        walk_ts_object_keyword
    );
    gated_walk!(
        visit_ts_big_int_keyword,
        TSBigIntKeyword,
        walk_ts_big_int_keyword
    );
    gated_walk!(
        visit_ts_type_reference,
        TSTypeReference<'a>,
        walk_ts_type_reference
    );
    gated_walk!(visit_ts_type_name, TSTypeName<'a>, walk_ts_type_name);
    gated_walk!(
        visit_ts_qualified_name,
        TSQualifiedName<'a>,
        walk_ts_qualified_name
    );
    gated_walk!(
        visit_ts_type_parameter_instantiation,
        TSTypeParameterInstantiation<'a>,
        walk_ts_type_parameter_instantiation
    );
    gated_walk!(
        visit_ts_type_parameter,
        TSTypeParameter<'a>,
        walk_ts_type_parameter
    );
    gated_walk!(
        visit_ts_type_parameter_declaration,
        TSTypeParameterDeclaration<'a>,
        walk_ts_type_parameter_declaration
    );
    gated_walk!(
        visit_ts_type_alias_declaration,
        TSTypeAliasDeclaration<'a>,
        walk_ts_type_alias_declaration
    );
    gated_walk!(
        visit_ts_class_implements,
        TSClassImplements<'a>,
        walk_ts_class_implements
    );
    gated_walk!(
        visit_ts_interface_declaration,
        TSInterfaceDeclaration<'a>,
        walk_ts_interface_declaration
    );
    gated_walk!(
        visit_ts_interface_body,
        TSInterfaceBody<'a>,
        walk_ts_interface_body
    );
    gated_walk!(
        visit_ts_property_signature,
        TSPropertySignature<'a>,
        walk_ts_property_signature
    );
    gated_walk!(visit_ts_signature, TSSignature<'a>, walk_ts_signature);
    gated_walk!(
        visit_ts_index_signature,
        TSIndexSignature<'a>,
        walk_ts_index_signature
    );
    gated_walk!(
        visit_ts_call_signature_declaration,
        TSCallSignatureDeclaration<'a>,
        walk_ts_call_signature_declaration
    );
    gated_walk!(
        visit_ts_method_signature,
        TSMethodSignature<'a>,
        walk_ts_method_signature
    );
    gated_walk!(
        visit_ts_construct_signature_declaration,
        TSConstructSignatureDeclaration<'a>,
        walk_ts_construct_signature_declaration
    );
    gated_walk!(
        visit_ts_index_signature_name,
        TSIndexSignatureName<'a>,
        walk_ts_index_signature_name
    );
    gated_walk!(
        visit_ts_interface_heritage,
        TSInterfaceHeritage<'a>,
        walk_ts_interface_heritage
    );
    gated_walk!(
        visit_ts_type_predicate,
        TSTypePredicate<'a>,
        walk_ts_type_predicate
    );
    gated_walk!(
        visit_ts_type_predicate_name,
        TSTypePredicateName<'a>,
        walk_ts_type_predicate_name
    );
    gated_walk!(
        visit_ts_module_declaration,
        TSModuleDeclaration<'a>,
        walk_ts_module_declaration
    );
    gated_walk!(
        visit_ts_module_declaration_name,
        TSModuleDeclarationName<'a>,
        walk_ts_module_declaration_name
    );
    gated_walk!(
        visit_ts_module_declaration_body,
        TSModuleDeclarationBody<'a>,
        walk_ts_module_declaration_body
    );
    gated_walk!(
        visit_ts_module_block,
        TSModuleBlock<'a>,
        walk_ts_module_block
    );
    gated_walk!(
        visit_ts_type_literal,
        TSTypeLiteral<'a>,
        walk_ts_type_literal
    );
    gated_walk!(visit_ts_infer_type, TSInferType<'a>, walk_ts_infer_type);
    gated_walk!(visit_ts_type_query, TSTypeQuery<'a>, walk_ts_type_query);
    gated_walk!(
        visit_ts_type_query_expr_name,
        TSTypeQueryExprName<'a>,
        walk_ts_type_query_expr_name
    );
    gated_walk!(visit_ts_import_type, TSImportType<'a>, walk_ts_import_type);
    gated_walk!(
        visit_ts_function_type,
        TSFunctionType<'a>,
        walk_ts_function_type
    );
    gated_walk!(
        visit_ts_constructor_type,
        TSConstructorType<'a>,
        walk_ts_constructor_type
    );
    gated_walk!(visit_ts_mapped_type, TSMappedType<'a>, walk_ts_mapped_type);
    gated_walk!(
        visit_ts_template_literal_type,
        TSTemplateLiteralType<'a>,
        walk_ts_template_literal_type
    );
    gated_walk!(
        visit_ts_as_expression,
        TSAsExpression<'a>,
        walk_ts_as_expression
    );
    gated_walk!(
        visit_ts_satisfies_expression,
        TSSatisfiesExpression<'a>,
        walk_ts_satisfies_expression
    );
    gated_walk!(
        visit_ts_type_assertion,
        TSTypeAssertion<'a>,
        walk_ts_type_assertion
    );
    gated_walk!(
        visit_ts_import_equals_declaration,
        TSImportEqualsDeclaration<'a>,
        walk_ts_import_equals_declaration
    );
    gated_walk!(
        visit_ts_module_reference,
        TSModuleReference<'a>,
        walk_ts_module_reference
    );
    gated_walk!(
        visit_ts_external_module_reference,
        TSExternalModuleReference<'a>,
        walk_ts_external_module_reference
    );
    gated_walk!(
        visit_ts_non_null_expression,
        TSNonNullExpression<'a>,
        walk_ts_non_null_expression
    );
    gated_walk!(visit_decorator, Decorator<'a>, walk_decorator);
    gated_walk!(
        visit_ts_export_assignment,
        TSExportAssignment<'a>,
        walk_ts_export_assignment
    );
    gated_walk!(
        visit_ts_namespace_export_declaration,
        TSNamespaceExportDeclaration<'a>,
        walk_ts_namespace_export_declaration
    );
    gated_walk!(
        visit_ts_instantiation_expression,
        TSInstantiationExpression<'a>,
        walk_ts_instantiation_expression
    );
    gated_walk!(
        visit_js_doc_nullable_type,
        JSDocNullableType<'a>,
        walk_js_doc_nullable_type
    );
    gated_walk!(
        visit_js_doc_non_nullable_type,
        JSDocNonNullableType<'a>,
        walk_js_doc_non_nullable_type
    );
    gated_walk!(
        visit_js_doc_unknown_type,
        JSDocUnknownType,
        walk_js_doc_unknown_type
    );
    gated_collection!(visit_directives, Directive<'a>, visit_directive);
    gated_collection!(visit_statements, Statement<'a>, visit_statement);
    gated_collection!(
        visit_array_expression_elements,
        ArrayExpressionElement<'a>,
        visit_array_expression_element
    );
    gated_collection!(
        visit_object_property_kinds,
        ObjectPropertyKind<'a>,
        visit_object_property_kind
    );
    gated_collection!(
        visit_template_elements,
        TemplateElement<'a>,
        visit_template_element
    );
    gated_collection!(visit_expressions, Expression<'a>, visit_expression);
    gated_collection!(visit_arguments, Argument<'a>, visit_argument);
    gated_collection!(
        visit_assignment_target_properties,
        AssignmentTargetProperty<'a>,
        visit_assignment_target_property
    );
    gated_collection!(
        visit_variable_declarators,
        VariableDeclarator<'a>,
        visit_variable_declarator
    );
    gated_collection!(visit_switch_cases, SwitchCase<'a>, visit_switch_case);
    gated_collection!(
        visit_binding_properties,
        BindingProperty<'a>,
        visit_binding_property
    );
    gated_collection!(
        visit_formal_parameter_list,
        FormalParameter<'a>,
        visit_formal_parameter
    );
    gated_collection!(visit_decorators, Decorator<'a>, visit_decorator);
    gated_collection!(
        visit_ts_class_implements_list,
        TSClassImplements<'a>,
        visit_ts_class_implements
    );
    gated_collection!(visit_class_elements, ClassElement<'a>, visit_class_element);
    gated_collection!(
        visit_import_declaration_specifiers,
        ImportDeclarationSpecifier<'a>,
        visit_import_declaration_specifier
    );
    gated_collection!(
        visit_import_attributes,
        ImportAttribute<'a>,
        visit_import_attribute
    );
    gated_collection!(
        visit_export_specifiers,
        ExportSpecifier<'a>,
        visit_export_specifier
    );
    gated_collection!(visit_jsx_children, JSXChild<'a>, visit_jsx_child);
    gated_collection!(
        visit_jsx_attribute_items,
        JSXAttributeItem<'a>,
        visit_jsx_attribute_item
    );
    gated_collection!(
        visit_ts_enum_members,
        TSEnumMember<'a>,
        visit_ts_enum_member
    );
    gated_collection!(visit_ts_types, TSType<'a>, visit_ts_type);
    gated_collection!(
        visit_ts_tuple_elements,
        TSTupleElement<'a>,
        visit_ts_tuple_element
    );
    gated_collection!(
        visit_ts_type_parameters,
        TSTypeParameter<'a>,
        visit_ts_type_parameter
    );
    gated_collection!(
        visit_ts_interface_heritages,
        TSInterfaceHeritage<'a>,
        visit_ts_interface_heritage
    );
    gated_collection!(visit_ts_signatures, TSSignature<'a>, visit_ts_signature);
    gated_collection!(
        visit_ts_index_signature_names,
        TSIndexSignatureName<'a>,
        visit_ts_index_signature_name
    );
    gated_collection!(visit_spans, Span, visit_span);
}

/// Exact expression (including assignment/update) spans, in request order.
/// All returned JSON is owned; statement spans including semicolons do not match.
/// Node IDs are view-local; source spans are the cross-view join keys.
pub(crate) fn views(
    source: &str,
    program: &Program<'_>,
    wanted: &[Span],
) -> DecompilerResult<Vec<Value>> {
    if source.len() > SOURCE_BYTES {
        return Err(error("source byte cap"));
    }
    if source != program.source_text
        || program.span != Span::new(0, source.len() as u32)
        || wanted.iter().any(|&s| !valid(source, s))
    {
        return Err(error("source/program mismatch or malformed requested span"));
    }
    let mut index = Index {
        source,
        wanted: wanted.iter().map(|s| (s.start, s.end)).collect(),
        selected: HashMap::new(),
        work: 0,
        depth: 0,
        error: None,
    };
    index.visit_program(program);
    if let Some(reason) = index.error {
        return Err(error(reason));
    }
    let mut global = 0;
    wanted
        .iter()
        .map(|&span| {
            let mut graph = Graph {
                source,
                nodes: Vec::new(),
                global: &mut global,
                omissions: BTreeSet::new(),
            };
            let root = match index.selected.get(&(span.start, span.end)) {
                Some(&node) => graph.project(node, 1)?,
                None => json!({"unresolved": "missing_exact_expression_span", "source": location(span)}),
            };
            Ok(json!({
                "schema_version": 1, "semantics": "syntax_only", "source": location(span),
                "root": root, "projected_nodes": graph.nodes.len(), "nodes": graph.nodes,
                "omissions": graph.omissions, "truncated": !graph.omissions.is_empty(),
                "limits": {"nodes_per_view": NODES, "depth": DEPTH, "collection_items": ITEMS,
                    "global_nodes": GLOBAL_NODES, "source_preview_bytes": 128, "literal_preview_bytes": 256,
                    "source_bytes": SOURCE_BYTES, "visitor_work": WORK, "visitor_depth": VISITOR_DEPTH},
                "visitor_work": index.work
            }))
        })
        .collect()
}

struct Graph<'s, 'g> {
    source: &'s str,
    nodes: Vec<Value>,
    global: &'g mut usize,
    omissions: BTreeSet<&'static str>,
}

impl Graph<'_, '_> {
    fn preview(&mut self, span: Span, cap: usize) -> DecompilerResult<Value> {
        let raw = self
            .source
            .get(span.start as usize..span.end as usize)
            .ok_or_else(|| error("malformed projected source span"))?;
        let mut end = raw.len().min(cap);
        while !raw.is_char_boundary(end) {
            end -= 1;
        }
        if end < raw.len() {
            self.omissions.insert("source");
        }
        Ok(
            json!({"start": span.start, "end": span.end, "preview": &raw[..end],
            "original_bytes": raw.len(), "preview_bytes": end, "truncated": end < raw.len()}),
        )
    }

    fn project(&mut self, syntax: Syntax<'_>, depth: usize) -> DecompilerResult<Value> {
        // Normalize without adding synthetic layers to the projection depth.
        let syntax = match syntax {
            Syntax::Expression(Expression::AssignmentExpression(e)) => Syntax::Assignment(e),
            Syntax::Expression(Expression::UpdateExpression(e)) => Syntax::Update(e),
            Syntax::Expression(e) if e.as_member_expression().is_some() => {
                Syntax::Member(e.as_member_expression().unwrap(), false)
            }
            Syntax::Target(e) if e.as_member_expression().is_some() => {
                Syntax::Member(e.as_member_expression().unwrap(), true)
            }
            other => other,
        };
        let span = syntax.span();
        if !valid(self.source, span) {
            return Err(error("malformed projected source span"));
        }
        let reason = if *self.global >= GLOBAL_NODES {
            Some("global_nodes")
        } else if self.nodes.len() >= NODES {
            Some("nodes")
        } else if depth > DEPTH {
            Some("depth")
        } else {
            None
        };
        if let Some(reason) = reason {
            self.omissions.insert(reason);
            return Ok(json!({"omitted": reason, "source": location(span)}));
        }
        let id = self.nodes.len();
        *self.global += 1;
        self.nodes.push(Value::Null);
        let mut node = json!({"id": id, "depth": depth, "kind": "opaque", "unresolved": true});
        let mut literal = None;
        match syntax {
            Syntax::Assignment(e) => {
                node = json!({"kind": "assignment", "operator": e.operator.as_str()});
                node["destination"] = match e.left.as_simple_assignment_target() {
                    Some(t) => self.project(Syntax::Target(t), depth + 1)?,
                    None => self.project(
                        Syntax::Leaf(e.left.span(), "opaque_assignment_pattern"),
                        depth + 1,
                    )?,
                };
                node["rhs"] = self.project(Syntax::Expression(&e.right), depth + 1)?;
            }
            Syntax::Update(e) => {
                node = json!({"kind": "update", "operator": e.operator.as_str(), "prefix": e.prefix,
                    "destination_is_also_read": true});
                node["destination"] = self.project(Syntax::Target(&e.argument), depth + 1)?;
            }
            Syntax::Target(SimpleAssignmentTarget::AssignmentTargetIdentifier(_)) => {
                node = json!({"kind": "identifier", "access": "destination", "symbolic": true});
            }
            Syntax::Target(_) => {}
            Syntax::Leaf(_, kind) => {
                node["kind"] = json!(kind);
                node["unresolved"] = json!(kind.starts_with("opaque"));
            }
            Syntax::Spread(e) => {
                node = json!({"kind": "spread"});
                node["argument"] = self.project(Syntax::Expression(&e.argument), depth + 1)?;
            }
            Syntax::Member(e, destination) => {
                node = json!({"kind": "member", "access": if destination {"destination"} else {"read"}});
                let (receiver, key, optional, computed) = match e {
                    MemberExpression::ComputedMemberExpression(e) => {
                        if let (Expression::Identifier(r), Expression::NumericLiteral(n)) =
                            (&e.object, &e.expression)
                        {
                            if r.name == "r"
                                && n.value >= 0.0
                                && n.value <= u32::MAX as f64
                                && n.value.fract() == 0.0
                            {
                                node["register"] = json!(n.value as u32);
                                node["register_syntax_only"] = json!(true);
                            }
                        }
                        (
                            &e.object,
                            Syntax::Expression(&e.expression),
                            e.optional,
                            true,
                        )
                    }
                    MemberExpression::StaticMemberExpression(e) => (
                        &e.object,
                        Syntax::Leaf(e.property.span, "static_key"),
                        e.optional,
                        false,
                    ),
                    MemberExpression::PrivateFieldExpression(e) => (
                        &e.object,
                        Syntax::Leaf(e.field.span, "private_key"),
                        e.optional,
                        false,
                    ),
                };
                node["optional"] = json!(optional);
                node["computed"] = json!(computed);
                node["receiver"] = self.project(Syntax::Expression(receiver), depth + 1)?;
                node["key"] = self.project(key, depth + 1)?;
            }
            Syntax::Expression(e) => match e {
                Expression::BooleanLiteral(_) => literal = Some("boolean"),
                Expression::NullLiteral(_) => literal = Some("null"),
                Expression::NumericLiteral(_) => literal = Some("number"),
                Expression::BigIntLiteral(_) => literal = Some("bigint"),
                Expression::RegExpLiteral(_) => literal = Some("regexp"),
                Expression::StringLiteral(_) => literal = Some("string"),
                Expression::Identifier(_) => node = json!({"kind": "identifier", "symbolic": true}),
                Expression::ThisExpression(_) => node = json!({"kind": "this", "symbolic": true}),
                Expression::ArrayExpression(e) => {
                    node = json!({"kind": "array"});
                    let mut items = Vec::new();
                    for (position, item) in e.elements.iter().take(ITEMS).enumerate() {
                        let syntax = match item {
                            ArrayExpressionElement::SpreadElement(s) => Syntax::Spread(s),
                            ArrayExpressionElement::Elision(h) => Syntax::Leaf(h.span, "hole"),
                            other => Syntax::Expression(other.as_expression().unwrap()),
                        };
                        items.push(json!({"position": position, "role": "element", "node": self.project(syntax, depth + 1)?}));
                    }
                    node["items"] = json!(items);
                    self.collection(&mut node, e.elements.len());
                }
                Expression::CallExpression(e) => {
                    node = json!({"kind": "call", "optional": e.optional});
                    self.call(&mut node, &e.callee, &e.arguments, depth, true)?;
                }
                Expression::NewExpression(e) => {
                    node = json!({"kind": "new"});
                    self.call(&mut node, &e.callee, &e.arguments, depth, false)?;
                }
                Expression::UnaryExpression(e) => {
                    node = json!({"kind": "unary", "operator": e.operator.as_str()});
                    node["argument"] = self.project(Syntax::Expression(&e.argument), depth + 1)?;
                }
                Expression::BinaryExpression(e) => {
                    node = json!({"kind": "binary", "operator": e.operator.as_str()});
                    node["left"] = self.project(Syntax::Expression(&e.left), depth + 1)?;
                    node["right"] = self.project(Syntax::Expression(&e.right), depth + 1)?;
                }
                Expression::LogicalExpression(e) => {
                    node = json!({"kind": "logical", "operator": e.operator.as_str()});
                    node["left"] = self.project(Syntax::Expression(&e.left), depth + 1)?;
                    node["right"] = self.project(Syntax::Expression(&e.right), depth + 1)?;
                }
                Expression::ConditionalExpression(e) => {
                    node = json!({"kind": "conditional"});
                    node["test"] = self.project(Syntax::Expression(&e.test), depth + 1)?;
                    node["consequent"] =
                        self.project(Syntax::Expression(&e.consequent), depth + 1)?;
                    node["alternate"] =
                        self.project(Syntax::Expression(&e.alternate), depth + 1)?;
                }
                Expression::ParenthesizedExpression(e) => {
                    node = json!({"kind": "parenthesized"});
                    node["expression"] =
                        self.project(Syntax::Expression(&e.expression), depth + 1)?;
                }
                // Unsupported variants retain their AST type and source, never a value.
                other => node["ast_variant"] = json!(opaque_variant(other)),
            },
        }
        if let Some(kind) = literal {
            node = json!({"kind": "literal", "literal_kind": kind, "raw_js_in": "source.preview"});
        }
        node["id"] = json!(id);
        node["depth"] = json!(depth);
        node["source"] = self.preview(span, if literal.is_some() { 256 } else { 128 })?;
        self.nodes[id] = node;
        Ok(json!({"node": id}))
    }

    fn collection(&mut self, node: &mut Value, total: usize) {
        node["items_total"] = json!(total);
        node["items_omitted"] = json!(total.saturating_sub(ITEMS));
        if total > ITEMS {
            self.omissions.insert("items");
        }
    }

    fn call(
        &mut self,
        node: &mut Value,
        callee: &Expression<'_>,
        args: &[Argument<'_>],
        depth: usize,
        helper: bool,
    ) -> DecompilerResult<()> {
        let shape = if helper
            && args.len() == 3
            && args[0].as_expression().is_some()
            && args[1].as_expression().is_some()
            && matches!(
                args[2].as_expression(),
                Some(Expression::ArrayExpression(_))
            ) {
            match callee {
                Expression::Identifier(i) if i.name == "apply" => Some("apply"),
                Expression::Identifier(i) if i.name == "construct" => Some("construct"),
                _ => None,
            }
        } else {
            None
        };
        if let Some(shape) = shape {
            node["syntactic_helper_shape"] = json!(shape);
            node["helper_identity_verified"] = json!(false);
        }
        node["callee"] = self.project(Syntax::Expression(callee), depth + 1)?;
        let mut arguments = Vec::new();
        for (position, argument) in args.iter().take(ITEMS).enumerate() {
            let syntax = match argument {
                Argument::SpreadElement(s) => Syntax::Spread(s),
                other => Syntax::Expression(other.as_expression().unwrap()),
            };
            let role = match (shape, position) {
                (Some(_), 0) => "syntactic_callee",
                (Some("construct"), 1) => "preallocated_receiver",
                (Some(_), 1) => "receiver",
                (Some(_), 2) => "user_arguments_array",
                _ => "argument",
            };
            let projected = self.project(syntax, depth + 1)?;
            if role == "user_arguments_array" {
                if let Some(id) = projected["node"].as_u64() {
                    if let Some(items) = self.nodes[id as usize]["items"].as_array_mut() {
                        for item in items {
                            item["role"] = json!("user_argument");
                        }
                    }
                }
            }
            arguments.push(json!({"position": position, "role": role, "node": projected}));
        }
        node["arguments"] = json!(arguments);
        self.collection(node, args.len());
        Ok(())
    }
}

fn opaque_variant(expression: &Expression<'_>) -> &'static str {
    match expression {
        Expression::TemplateLiteral(_) => "TemplateLiteral",
        Expression::MetaProperty(_) => "MetaProperty",
        Expression::Super(_) => "Super",
        Expression::ArrowFunctionExpression(_) => "ArrowFunctionExpression",
        Expression::AwaitExpression(_) => "AwaitExpression",
        Expression::ChainExpression(_) => "ChainExpression",
        Expression::ClassExpression(_) => "ClassExpression",
        Expression::FunctionExpression(_) => "FunctionExpression",
        Expression::ImportExpression(_) => "ImportExpression",
        Expression::ObjectExpression(_) => "ObjectExpression",
        Expression::SequenceExpression(_) => "SequenceExpression",
        Expression::TaggedTemplateExpression(_) => "TaggedTemplateExpression",
        Expression::YieldExpression(_) => "YieldExpression",
        Expression::PrivateInExpression(_) => "PrivateInExpression",
        Expression::JSXElement(_) => "JSXElement",
        Expression::JSXFragment(_) => "JSXFragment",
        Expression::TSAsExpression(_) => "TSAsExpression",
        Expression::TSSatisfiesExpression(_) => "TSSatisfiesExpression",
        Expression::TSTypeAssertion(_) => "TSTypeAssertion",
        Expression::TSNonNullExpression(_) => "TSNonNullExpression",
        Expression::TSInstantiationExpression(_) => "TSInstantiationExpression",
        Expression::V8IntrinsicExpression(_) => "V8IntrinsicExpression",
        _ => "UnsupportedExpression",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indexing_work_cap_stops_without_unwinding() {
        let allocator = oxc_allocator::Allocator::default();
        let source = "r[1] = 1; r[2] = 2;";
        let parsed =
            oxc_parser::Parser::new(&allocator, source, oxc_span::SourceType::default()).parse();
        let mut index = Index {
            source,
            wanted: HashSet::new(),
            selected: HashMap::new(),
            work: WORK - 2,
            depth: 0,
            error: None,
        };
        index.visit_program(&parsed.program);
        assert_eq!(index.error, Some("complete AST visitor work cap"));
        assert_eq!(index.work, WORK);
        assert_eq!(index.depth, 0);
    }
}
