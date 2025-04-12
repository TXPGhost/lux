use super::*;

impl DesugarArena {
    /// Constructs a new [DesugarArena] with prelude symbols defined
    pub fn new_prelude() -> (Self, Parent) {
        let mut member_lists = Arena::default();
        let mut members = Arena::default();
        let mut exprs = Arena::default();

        let u64ty = members.add(
            Member {
                field: Field::Ident(Ident::TIdent("U64".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::U64Ty).unloc()),
            }
            .unloc(),
        );
        let boolty = members.add(
            Member {
                field: Field::Ident(Ident::TIdent("Bool".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::Bool).unloc()),
            }
            .unloc(),
        );

        let trueval = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("true".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::True).unloc()),
            }
            .unloc(),
        );
        let falseval = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("false".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::False).unloc()),
            }
            .unloc(),
        );

        let assert_eq = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("assert_eq".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::Assert(Assertion::Eq)).unloc()),
            }
            .unloc(),
        );
        let assert_ne = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("assert_ne".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::Assert(Assertion::Ne)).unloc()),
            }
            .unloc(),
        );
        let assert_sub = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("assert_sub".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::Assert(Assertion::Subtype)).unloc()),
            }
            .unloc(),
        );
        let assert_super = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("assert_super".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::Assert(Assertion::Supertype)).unloc()),
            }
            .unloc(),
        );
        let debug_print = members.add(
            Member {
                field: Field::Ident(Ident::VIdent("debug_print".into()).unloc()).unloc(),
                expr: exprs.add(Expr::Primitive(Primitive::DebugPrint).unloc()),
            }
            .unloc(),
        );

        let member_list = member_lists.add(
            MemberList {
                members: vec![
                    u64ty,
                    boolty,
                    trueval,
                    falseval,
                    assert_eq,
                    assert_ne,
                    assert_sub,
                    assert_super,
                    debug_print,
                ],
                parent: None,
            }
            .unloc(),
        );

        let arena = Self {
            member_lists,
            members,
            blocks: Arena::default(),
            exprs,
            stmts: Arena::default(),
        };

        (arena, Parent::MemberList(member_list))
    }
}
