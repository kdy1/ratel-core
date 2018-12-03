use ratel::ast::{NodeList, ExpressionNode, Function, Name, OptionalName, Block, Loc, Node};
use ratel::ast::expression::{ArrowExpression, ArrowBody, ThisExpression, CallExpression, MemberExpression};
use ratel::ast::statement::ReturnStatement;
use ratel_visitor::{Visitor, Visitable};

use TransformerCtxt;

pub struct TransformArrow<'ast> {
    ctx: TransformerCtxt<'ast>
}

impl<'ast> TransformArrow<'ast> {
    pub fn new(ctx: TransformerCtxt<'ast>) -> TransformArrow<'ast> {
        TransformArrow {
            ctx
        }
    }
}

static BIND: &Loc<&str> = &Loc {
    start: 0,
    end: 0,
    item: "bind"
};

fn uses_this(body: &ArrowBody) -> bool {
    struct ThisChecker {
        found: bool,
    }
    impl<'ast> Visitor<'ast> for ThisChecker {
        fn on_this_expression(&mut self, _: &'ast ExpressionNode<'ast>) {
            self.found = true;
        }
    }
    let mut checker = ThisChecker { found: false };
    body.visit_with(&mut checker);
    checker.found
}

impl<'ast> Visitor<'ast> for TransformArrow<'ast> {
    fn on_arrow_expression(&mut self, node: &ArrowExpression<'ast>, ptr: &'ast ExpressionNode<'ast>) {
        let used_this = uses_this(&node.body);

        let body = match node.body {
            ArrowBody::Block(block)     => block,
            ArrowBody::Expression(expr) => {
                let ret = self.ctx.alloc_as_loc(&expr, ReturnStatement {
                    value: Some(expr)
                });

                self.ctx.alloc_as_loc(&ret, Block {
                    body: NodeList::from(self.ctx.arena, ret)
                })
            }
        };

        let f = Function {
            name: OptionalName::empty(),
            generator: false,
            params: node.params,
            body,
        };

        if !used_this {
            self.ctx.swap(*ptr, f);
        } else {
            let f = self.ctx.alloc(f);
            // f.bind(this)
            let callee = self.ctx.alloc(MemberExpression {
                object: f,
                property: Node::new(BIND),
            });
            let this = self.ctx.alloc(ThisExpression {

            });
            let arguments = self.ctx.list([this]);

            self.ctx.swap(*ptr, CallExpression {
                callee,
                arguments
            });
        }
    }
}
