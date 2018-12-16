use ratel::ast::{
    expression::{CallExpression, ClassExpression, FunctionExpression},
    statement::{BlockStatement, ClassStatement, DeclarationStatement, Declarator},
    Class, DeclarationKind, Expression, ExpressionNode, Identifier, IdentifierNode, Name, NodeList,
    OptionalName, Pattern, StatementNode,
};
use ratel_visitor::Visitor;
use toolshed::list::List;

use TransformerCtxt;

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
        super_ident: Option<Identifier<'ast>>,
        class: &Class<'ast, N>,
    ) -> List<'ast, StatementNode<'ast>>
    where
        N: Name<'ast>,
    {
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