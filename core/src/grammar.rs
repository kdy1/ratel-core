use owned_slice::OwnedSlice;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Undefined,
    Null,
    True,
    False,
    Number(OwnedSlice),
    Binary(u64),
    String(OwnedSlice),
    RawQuasi(OwnedSlice),
}


#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub name: OwnedSlice,
    pub default: Option<Box<Expression>>
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorType {
    FatArrow,         //   …  => …
    Accessor,         //   …  .  …
    New,              //     new …
    Increment,        //      ++ … | … ++
    Decrement,        //      -- … | … --
    LogicalNot,       //       ! …
    BitwiseNot,       //       ~ …
    Typeof,           //  typeof …
    Void,             //    void …
    Delete,           //  delete …
    Multiplication,   //   …  *  …
    Division,         //   …  /  …
    Remainder,        //   …  %  …
    Exponent,         //   …  ** …
    Addition,         //   …  +  … | + …
    Substraction,     //   …  -  … | - …
    BitShiftLeft,     //   …  << …
    BitShiftRight,    //   …  >> …
    UBitShiftRight,   //   … >>> …
    Lesser,           //   …  <  …
    LesserEquals,     //   …  <= …
    Greater,          //   …  >  …
    GreaterEquals,    //   …  >= …
    Instanceof,       //   … instanceof …
    In,               //   …  in …
    StrictEquality,   //   … === …
    StrictInequality, //   … !== …
    Equality,         //   …  == …
    Inequality,       //   …  != …
    BitwiseAnd,       //   …  &  …
    BitwiseXor,       //   …  ^  …
    BitwiseOr,        //   …  |  …
    LogicalAnd,       //   …  && …
    LogicalOr,        //   …  || …
    Conditional,      //   …  ?  …  :  …
    Assign,           //   …  =  …
    AddAssign,        //   …  += …
    SubstractAssign,  //   …  -= …
    ExponentAssign,   //   … **= …
    MultiplyAssign,   //   …  *= …
    DivideAssign,     //   …  /= …
    RemainderAssign,  //   …  %= …
    BSLAssign,        //   … <<= …
    BSRAssign,        //   … >>= …
    UBSRAssign,       //   … >>>= …
    BitAndAssign,     //   …  &= …
    BitXorAssign,     //   …  ^= …
    BitOrAssign,      //   …  |= …
    Spread,           //     ... …
}
use self::OperatorType::*;

impl OperatorType {
    /// According to the Operator Precedence Table
    /// Note: Unary opearotrs default to 15!
    pub fn binding_power(&self) -> u8 {
        match *self {
            FatArrow         |
            Accessor         => 18,

            New              => 17,

            Increment        |
            Decrement        => 16,

            LogicalNot       |
            BitwiseNot       |
            Typeof           |
            Void             |
            Delete           => 15,

            Multiplication   |
            Division         |
            Remainder        |
            Exponent         => 14,

            Addition         |
            Substraction     => 13,

            BitShiftLeft     |
            BitShiftRight    |
            UBitShiftRight   => 12,

            Lesser           |
            LesserEquals     |
            Greater          |
            GreaterEquals    |
            Instanceof       |
            In               => 11,

            StrictEquality   |
            StrictInequality |
            Equality         |
            Inequality       => 10,

            BitwiseAnd       => 9,
            BitwiseXor       => 8,
            BitwiseOr        => 7,
            LogicalAnd       => 6,
            LogicalOr        => 5,
            Conditional      => 4,

            Assign           |
            AddAssign        |
            SubstractAssign  |
            ExponentAssign   |
            MultiplyAssign   |
            DivideAssign     |
            RemainderAssign  |
            BSLAssign        |
            BSRAssign        |
            UBSRAssign       |
            BitAndAssign     |
            BitXorAssign     |
            BitOrAssign      => 3,

            Spread           => 1,
        }
    }

    pub fn prefix(&self) -> bool {
        match *self {
            LogicalNot       |
            BitwiseNot       |
            Typeof           |
            Void             |
            Delete           |
            New              |
            Spread           |
            Increment        |
            Decrement        |
            Addition         |
            Substraction     => true,

            _                => false
        }
    }

    pub fn infix(&self) -> bool {
        match *self {
            FatArrow         |
            Accessor         |
            Multiplication   |
            Division         |
            Remainder        |
            Exponent         |
            StrictEquality   |
            StrictInequality |
            Equality         |
            Inequality       |
            Lesser           |
            LesserEquals     |
            Greater          |
            GreaterEquals    |
            Instanceof       |
            In               |
            BitShiftLeft     |
            BitShiftRight    |
            UBitShiftRight   |
            BitwiseAnd       |
            BitwiseXor       |
            BitwiseOr        |
            LogicalAnd       |
            LogicalOr        |
            Conditional      |
            Addition         |
            Substraction     |
            Assign           |
            AddAssign        |
            SubstractAssign  |
            ExponentAssign   |
            MultiplyAssign   |
            DivideAssign     |
            RemainderAssign  |
            BSLAssign        |
            BSRAssign        |
            UBSRAssign       |
            BitAndAssign     |
            BitXorAssign     |
            BitOrAssign      => true,

            _                => false
        }
    }

    pub fn assignment(&self) -> bool {
        match *self {
            Assign           |
            AddAssign        |
            SubstractAssign  |
            ExponentAssign   |
            MultiplyAssign   |
            DivideAssign     |
            RemainderAssign  |
            BSLAssign        |
            BSRAssign        |
            UBSRAssign       |
            BitAndAssign     |
            BitXorAssign     |
            BitOrAssign      => true,

            _                => false
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    This,
    Identifier(OwnedSlice),
    Literal(Value),
    Template {
        tag: Option<Box<Expression>>,
        expressions: Vec<Expression>,
        quasis: Vec<OwnedSlice>,
    },
    RegEx {
        pattern: OwnedSlice,
        flags: OwnedSlice
    },
    Array(Vec<Expression>),
    Sequence(Vec<Expression>),
    Object(Vec<ObjectMember>),
    Member {
        object: Box<Expression>,
        property: OwnedSlice,
    },
    ComputedMember {
        object: Box<Expression>,
        property: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Binary {
        parenthesized: bool,
        operator: OperatorType,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Prefix {
        operator: OperatorType,
        operand: Box<Expression>,
    },
    Postfix {
        operator: OperatorType,
        operand: Box<Expression>,
    },
    Conditional {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Expression>,
    },
    ArrowFunction {
        params: Vec<Parameter>,
        body: Box<Statement>,
    },
    Function {
        name: Option<OwnedSlice>,
        params: Vec<Parameter>,
        body: Vec<Statement>,
    }
}

impl Expression {
    pub fn binding_power(&self) -> u8 {
        match *self {
            Expression::Member {
                ..
            }
            |
            Expression::ArrowFunction {
                ..
            } => 18,

            Expression::Call {
                ..
            } => 17,

            Expression::Prefix {
                ..
            } => 15,

            Expression::Binary {
                ref operator,
                ..
            }
            |
            Expression::Postfix {
                ref operator,
                ..
            } => operator.binding_power(),

            Expression::Conditional {
                ..
            } => 4,

            _  => 100,
        }
    }

    #[inline]
    pub fn binary<E: Into<Expression>>(left: E, operator: OperatorType, right: E) -> Self {
        Expression::Binary {
            parenthesized: false,
            operator: operator,
            left: Box::new(left.into()),
            right: Box::new(right.into()),
        }
    }

    #[inline]
    pub fn member<E: Into<Expression>, S: Into<OwnedSlice>>(object: E, property: S) -> Self {
        Expression::Member {
            object: Box::new(object.into()),
            property: property.into(),
        }
    }

    #[inline]
    pub fn call<E: Into<Expression>>(callee: E, arguments: Vec<Expression>) -> Self {
        Expression::Call {
            callee: Box::new(callee.into()),
            arguments: arguments,
        }
    }

    #[inline]
    pub fn parenthesize(mut self) -> Expression {
        if let Expression::Binary {
            ref mut parenthesized,
            ..
        } = self {
            *parenthesized = true;
        }

        self
    }

    #[inline]
    pub fn needs_parens(&self, bp: u8) -> bool {
        match *self {
            Expression::Binary {
                ref parenthesized,
                ref operator,
                ..
            } => *parenthesized && bp >= operator.binding_power(),
            _ => false
        }
    }

    #[inline]
    pub fn is_allowed_as_bare_statement(&self) -> bool {
        match *self {
            Expression::Object(_)       => false,
            Expression::Function { .. } => false,

            _                           => true,
        }
    }
}

impl From<&'static str> for Expression {
    #[inline]
    fn from(ident: &'static str) -> Self {
        Expression::Identifier(OwnedSlice::from_static(ident))
    }
}

impl From<OwnedSlice> for Expression {
    #[inline]
    fn from(ident: OwnedSlice) -> Self {
        Expression::Identifier(ident)
    }
}

impl<'a> From<&'a OwnedSlice> for Expression {
    #[inline]
    fn from(ident: &'a OwnedSlice) -> Self {
        Expression::Identifier(*ident)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectMember {
    Shorthand {
        key: OwnedSlice,
    },
    Value {
        key: ObjectKey,
        value: Expression,
    },
    Method {
        key: ObjectKey,
        params: Vec<Parameter>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum ObjectKey {
    Computed(Expression),
    Literal(OwnedSlice),
    Binary(u64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassMember {
    Constructor {
        params: Vec<Parameter>,
        body: Vec<Statement>,
    },
    Method {
        is_static: bool,
        key: ClassKey,
        params: Vec<Parameter>,
        body: Vec<Statement>,
    },
    Property {
        is_static: bool,
        key: ClassKey,
        value: Expression,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ClassKey {
    Computed(Expression),
    Literal(OwnedSlice),
    Number(OwnedSlice),
    Binary(u64),
}

impl ClassKey {
    #[inline]
    pub fn is_constructor(&self) -> bool {
        match *self {
            ClassKey::Literal(ref name) => name.as_str() == "constructor",

            _ => false
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VariableDeclarationKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclarator {
    pub name: OwnedSlice,
    pub value: Option<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Block {
        body: Vec<Statement>,
    },
    // `Transparent` is not part of the language grammar, just a helper that
    // allows the transformer to replace a single statement with mutliple
    // statements without messing with parent array.
    Transparent {
        body: Vec<Statement>,
    },
    Labeled {
        label: OwnedSlice,
        body: Box<Statement>,
    },
    VariableDeclaration {
        kind: VariableDeclarationKind,
        declarators: Vec<VariableDeclarator>,
    },
    Expression {
        value: Expression
    },
    Return {
        value: Option<Expression>,
    },
    Break {
        label: Option<OwnedSlice>,
    },
    Function {
        name: OwnedSlice,
        params: Vec<Parameter>,
        body: Vec<Statement>,
    },
    If {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>,
    },
    While {
        test: Expression,
        body: Box<Statement>,
    },
    For {
        init: Option<Box<Statement>>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Box<Statement>,
    },
    ForIn {
        left: Box<Statement>,
        right: Expression,
        body: Box<Statement>,
    },
    ForOf {
        left: Box<Statement>,
        right: Expression,
        body: Box<Statement>,
    },
    Class {
        name: OwnedSlice,
        extends: Option<OwnedSlice>,
        body: Vec<ClassMember>,
    },
    Throw {
        value: Expression
    },
}

impl From<Expression> for Statement {
    #[inline]
    fn from(expression: Expression) -> Self {
        Statement::Expression {
            value: expression
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub source: String,
    pub body: Vec<Statement>,
}
