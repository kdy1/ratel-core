use ratel::ast::{
    expression::{
        BinaryExpression, CallExpression, ClassExpression, FunctionExpression, MemberExpression,
        ThisExpression,
    },
    statement::{
        BlockStatement, ClassStatement, DeclarationStatement, Declarator, ReturnStatement,
    },
    Block, Class, ClassMember, DeclarationKind, Expression, ExpressionNode, Function, Identifier,
    IdentifierNode, MandatoryName, MethodKind, Name, NodeList, OperatorKind, OptionalName, Pattern,
    Statement, StatementNode,
};
use ratel_visitor::Visitor;
use std::iter;
use toolshed::list::{List, ListBuilder};
use TransformerCtxt;

#[cfg(test)]
mod tests;

pub struct TransformClass<'ast> {
    ctx: TransformerCtxt<'ast>,
}

impl<'ast> TransformClass<'ast> {
    pub fn new(ctx: TransformerCtxt<'ast>) -> TransformClass<'ast> {
        TransformClass { ctx }
    }

    /// Returns function call expression.
    fn fold_class<N>(
        &mut self,
        class_name: Option<IdentifierNode<'ast>>,
        class: &Class<'ast, N>,
    ) -> Expression<'ast>
    where
        N: Name<'ast>,
    {
        let super_ident: Option<Identifier<'ast>> = match class.extends {
            Some(expr) => Some(match **expr {
                Expression::Identifier(i) => i,
                _ => "_Super",
            }),
            None => None,
        };

        let (params, arguments) = if let Some(super_ident) = super_ident {
            let param: Pattern = Pattern::Identifier(super_ident);
            let arg = class.extends.unwrap();
            (
                NodeList::from(self.ctx.arena, self.ctx.alloc(param)),
                NodeList::from(self.ctx.arena, arg),
            )
        } else {
            (NodeList::empty(), NodeList::empty())
        };

        // Ident of the super class **in** iife
        let super_ident = super_ident.map(|i| self.ctx.alloc(i));
        // Body of the iife
        let body = BlockStatement {
            body: self.class_to_stmts(class_name, super_ident, class),
        };

        let function = self.ctx.alloc(FunctionExpression {
            body: self.ctx.alloc(body),
            params,
            name: OptionalName::empty(),
            generator: false,
        });

        Expression::Call(CallExpression {
            callee: function,
            arguments,
        })
    }

    fn class_to_stmts<N>(
        &mut self,
        class_name: Option<IdentifierNode<'ast>>,
        super_class_ident: Option<IdentifierNode<'ast>>,
        class: &Class<'ast, N>,
    ) -> List<'ast, StatementNode<'ast>>
    where
        N: Name<'ast>,
    {
        // Class name inside iife
        let class_name = class_name.unwrap_or_else(|| self.ctx.alloc("_Class"));
        let class_name_expr = self.ctx.alloc(Expression::Identifier(**class_name));
        let stmts = ListBuilder::new(self.ctx.arena, Statement::Empty);

        if let Some(super_class_ident) = super_class_ident {
            // TODO: inject helper methods
            // self.helpers.inherits.store(true, Ordering::Relaxed);
            // self.helpers
            //     .possible_constructor_return
            //     .store(true, Ordering::Relaxed);

            let child = self.ctx.alloc(Expression::Identifier(**class_name));
            let parent = self.ctx.alloc(Expression::Identifier(**super_class_ident));
            let arguments = self.ctx.list([child, parent]);
            stmts.push(
                self.ctx.arena,
                Statement::Expression(self.ctx.alloc(Expression::Call(CallExpression {
                    callee: self.ctx.alloc(Expression::Identifier("_inherits")),
                    arguments,
                }))),
            );
        }

        {
            // Find constuctor
            let constructor = class
                .body
                .body
                .iter()
                .filter_map(|m| match ***m {
                    ClassMember::Method {
                        kind: MethodKind::Constructor,
                        value,
                        ..
                    } => Some(Function {
                        name: MandatoryName(class_name),
                        params: value.params,
                        generator: value.generator,
                        body: value.body,
                    }),
                    _ => None,
                })
                .next();

            let function = constructor.unwrap_or_else(|| Function {
                name: MandatoryName(class_name),
                generator: false,
                params: List::empty(),
                body: self.ctx.alloc(BlockStatement {
                    body: List::empty(),
                }),
            });

            // inject _classCallCheck(this, Bar);
            let body_builder = ListBuilder::new(
                self.ctx.arena,
                self.ctx
                    .alloc(Statement::Expression(self.ctx.alloc(Expression::Call(
                        CallExpression {
                            callee: self.ctx.alloc(Expression::Identifier("_classCallCheck")),
                            arguments: {
                                let this = self.ctx.alloc(Expression::This(ThisExpression));
                                self.ctx.list(&[this, class_name_expr])
                            },
                        },
                    )))),
            );

            let mut orig_stmts = function.body.body.iter();
            let mut has_super_call = false;
            let mut orig_left_count = function.body.body.iter().count();
            for stmt in &mut orig_stmts {
                // Find super() call
                match ***stmt {
                    Statement::Expression(expr) => match **expr {
                        Expression::Call(CallExpression { callee, .. }) => match **callee {
                            Expression::Identifier("super") => {
                                has_super_call = true;
                                break;
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                }
                orig_left_count -= 1;
                body_builder.push(self.ctx.arena, *stmt);
            }

            if super_class_ident.is_some() {
                // inject possibleReturnCheck

                // is super() call last?
                let is_last = orig_left_count == 0;

                // possible return value from super() call
                let possible_return_value = self.ctx.alloc(Expression::Call(CallExpression {
                    callee: self.ctx.alloc("_possibleContstructorReturn"),
                    arguments: self.ctx.list(&[self.ctx.alloc(ThisExpression), {
                        let apply = Expression::Call(CallExpression {
                            callee: self.ctx.alloc(MemberExpression {
                                object: get_prototype_of(
                                    self.ctx,
                                    self.ctx.alloc(Expression::Identifier(**class_name)),
                                ),
                                property: self.ctx.alloc(if super_class_ident.is_some() {
                                    "call"
                                } else {
                                    "apply"
                                }),
                            }),

                            arguments: if has_super_call {
                                // Code like `super(foo, bar)` should be result in
                                // `.call(this, foo, bar)`
                                match ***function.body.body.iter().next().unwrap() {
                                    Statement::Expression(expr) => match **expr {
                                        Expression::Call(CallExpression {
                                            callee,
                                            arguments,
                                            ..
                                        }) => match **callee {
                                            Expression::Identifier("super") => List::from_iter(
                                                self.ctx.arena,
                                                iter::once(
                                                    self.ctx
                                                        .alloc(Expression::This(ThisExpression)),
                                                )
                                                .chain(arguments.into_iter().cloned()),
                                            ),
                                            _ => unreachable!(),
                                        },
                                        _ => unreachable!(),
                                    },
                                    _ => unreachable!(),
                                }
                            } else {
                                self.ctx.list(&[
                                    self.ctx.alloc(ThisExpression),
                                    self.ctx.alloc("arguments"),
                                ])
                            },
                        });

                        self.ctx.alloc(apply)
                    }]),
                }));

                match has_super_call {
                    true => {
                        if !is_last {
                            body_builder.push(
                                self.ctx.arena,
                                self.ctx.alloc(Statement::Declaration(DeclarationStatement {
                                    kind: DeclarationKind::Var,
                                    declarators: self.ctx.list(&[self.ctx.alloc(Declarator {
                                        id: self.ctx.alloc(Pattern::Identifier("_this")),
                                        init: Some(possible_return_value),
                                    })]),
                                })),
                            );

                            for stmt in orig_stmts {
                                orig_left_count -= 1;
                                body_builder.push(self.ctx.arena, *stmt);
                            }

                            body_builder.push(
                                self.ctx.arena,
                                self.ctx.alloc(Statement::Return(ReturnStatement {
                                    value: Some(self.ctx.alloc(Expression::Identifier("_this"))),
                                })),
                            );
                        } else {
                            for stmt in orig_stmts {
                                orig_left_count -= 1;
                                body_builder.push(self.ctx.arena, *stmt);
                            }

                            body_builder.push(
                                self.ctx.arena,
                                self.ctx.alloc(Statement::Return(ReturnStatement {
                                    value: Some(possible_return_value),
                                })),
                            );
                        }
                    }

                    _ => {
                        for stmt in orig_stmts {
                            orig_left_count -= 1;
                            body_builder.push(self.ctx.arena, *stmt);
                        }

                        body_builder.push(
                            self.ctx.arena,
                            self.ctx.alloc(Statement::Return(ReturnStatement {
                                value: Some(possible_return_value),
                            })),
                        )
                    }
                }
            }
            assert_eq!(orig_left_count, 0);

            // TODO: Handle
            //
            //     console.log('foo');
            //     super();
            //     console.log('bar');
            //
            //

            stmts.push(
                self.ctx.arena,
                Statement::Function(Function {
                    name: MandatoryName(class_name),
                    body: self.ctx.alloc(Block {
                        body: body_builder.as_list(),
                    }),
                    ..function
                }),
            );
        }

        unimplemented!()
    }
}

impl<'ast> Visitor<'ast> for TransformClass<'ast> {
    fn on_class_expression(
        &mut self,
        node: &ClassExpression<'ast>,
        ptr: &'ast ExpressionNode<'ast>,
    ) {
        let expr = self.fold_class(node.name.0, node);

        self.ctx.swap(*ptr, expr)
    }

    fn on_class_statement(&mut self, node: &ClassStatement<'ast>, ptr: &'ast StatementNode<'ast>) {
        let id = node.name.0;
        let expr = self.fold_class(Some(node.name.0), node);
        let expr = self.ctx.alloc(expr);

        let id = self.ctx.alloc(Pattern::Identifier(&*id));
        let decl = self.ctx.alloc_as_loc(
            &expr,
            Declarator {
                id,
                init: Some(expr),
            },
        );

        self.ctx.swap(
            *ptr,
            DeclarationStatement {
                kind: DeclarationKind::Var,
                declarators: NodeList::from(self.ctx.arena, decl),
            },
        )
    }
}

/// Creates
///
/// ```js
/// Child.__proto__ || Object.getPrototypeOf(Child)
/// ```
fn get_prototype_of<'ast>(
    ctx: TransformerCtxt<'ast>,
    object: ExpressionNode<'ast>,
) -> ExpressionNode<'ast> {
    // `Child.__proto__`
    let proto = ctx.alloc(Expression::Member(MemberExpression {
        object,
        property: ctx.alloc("__proto__"),
    }));

    // `Object.getPrototypeOf(Child)`
    let get_proto_of = ctx.alloc(Expression::Call(CallExpression {
        callee: ctx.alloc(MemberExpression {
            object: ctx.alloc("Object"),
            property: ctx.alloc("getPrototypeOf"),
        }),
        arguments: List::from(ctx.arena, object),
    }));

    // `Child.__proto__ || Object.getPrototypeOf(Child)`
    ctx.alloc(Expression::Binary(BinaryExpression {
        left: proto,
        operator: OperatorKind::LogicalAnd,
        // Object.getPrototypeOf(Child)
        right: get_proto_of,
    }))
}
