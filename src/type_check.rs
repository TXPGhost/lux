use std::collections::HashSet;

use type_compare::TypeCompare;

use crate::{
    arena::{ArenaMap, Handle},
    ast::{desugar::*, Node, NodeExt, Primitive},
    lexer::Operator,
    pretty_print::PrettyPrint,
};

/// Compares two types
pub mod type_compare;

/// The result of type assignments, a mapping from desugared expression handles to types
pub struct TypeArena {
    map: ArenaMap<Node<Expr>, Handle<Node<Expr>>>,
    arena: DesugarArena,
    unit: Handle<Node<Expr>>,
    empty: Handle<Node<Expr>>,
    u64ty: Handle<Node<Expr>>,
    discovered_exprs: HashSet<Handle<Node<Expr>>>,
    recursive_exprs: HashSet<Handle<Node<Expr>>>,
}

impl TypeArena {
    /// Constructs a new [TypeArena]
    pub fn new() -> Self {
        let mut types = Self {
            map: ArenaMap::default(),
            arena: DesugarArena::default(),
            unit: Handle::from_idx(0),
            empty: Handle::from_idx(1),
            u64ty: Handle::from_idx(2),
            discovered_exprs: HashSet::new(),
            recursive_exprs: HashSet::new(),
        };

        // add the unit and empty types
        let unit_members = types.arena.member_lists.add(
            MemberList {
                members: Vec::new(),
                parent: None,
            }
            .unloc(),
        );
        types.arena.exprs.add(Expr::Struct(unit_members).unloc());
        types
            .arena
            .exprs
            .add(Expr::Primitive(Primitive::Empty).unloc());
        types
            .arena
            .exprs
            .add(Expr::Primitive(Primitive::U64Ty).unloc());

        types
    }
}

impl Default for TypeArena {
    fn default() -> Self {
        Self::new()
    }
}

/// An error that can occur during type checking
#[derive(Clone, Debug)]
pub enum TypeError {
    /// Expected a function, but found something else
    ExpectedFunction(Node<Expr>),

    /// Expected a struct, but found something else
    ExpectedStruct(Node<Expr>),

    /// The given struct field does not exist
    UnrecognizedStructField(Node<Field>),

    /// A function argument type did not match
    FunctionArgumentMismatch,

    /// An identifier was not resolved
    UnresolvedIdentifier(Node<Ident>),

    /// An identifier was defined recursively
    RecursiveIdent(Node<Ident>),

    /// An illegal type was used as part of a binary operation
    IllegalBinopType(Handle<Node<Expr>>, Operator),

    /// A part of the typechecker is unimplemented
    Unimplemented(String),
}

/// Indicates that types can be assigned to an expression
pub trait AssignTypes: Sized {
    /// Assigns types to the given type
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError>;
}

impl AssignTypes for Handle<Node<Expr>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        if types.map.get(self).is_some() {
            return Ok(());
        }
        if types.discovered_exprs.contains(&self) {
            types.recursive_exprs.insert(self);
            types.map.insert(self, types.empty);
            return Ok(());
        }
        types.discovered_exprs.insert(self);
        let expr = &arena.exprs.get(self).val;
        let loc = &arena.exprs.get(self).loc;
        match expr {
            Expr::Ident(ident) => match ident.val {
                Ident::Resolved(expr) => {
                    expr.assign_types(arena, types)?;
                    let ty = types.map.get(expr).unwrap();
                    types.map.insert(self, *ty);
                    Ok(())
                }
                _ => Err(TypeError::UnresolvedIdentifier(ident.clone())),
            },
            Expr::Index(handle, handle1) => todo!("typecheck index"),
            Expr::Field(expr, field) => {
                // assign types to the struct
                expr.assign_types(arena, types)?;

                // make sure we actually have a struct
                let expr_ty = types.map.get(*expr).unwrap();
                let Expr::Struct(member_list) = types.arena.exprs.get(*expr_ty).val else {
                    return Err(TypeError::ExpectedStruct(arena.exprs.get(*expr).clone()));
                };

                let member_list = &types.arena.member_lists.get(member_list).val;
                for member in &member_list.members {
                    let member = &types.arena.members.get(*member).val;
                    if member.field.val == field.val {
                        types.map.insert(self, member.expr);
                        return Ok(());
                    }
                }

                Err(TypeError::UnrecognizedStructField(field.clone()))
            }
            Expr::Struct(member_list) | Expr::Enum(member_list) => {
                // assign types to each member
                member_list.assign_types(arena, types)?;

                // get te member types
                let mut member_types = Vec::new();
                for member in &arena.member_lists.get(*member_list).val.members {
                    let member = &arena.members.get(*member).val;
                    let member_ty = types.map.get(member.expr).unwrap();
                    member_types.push(
                        types.arena.members.add(
                            Member {
                                field: member.field.clone(),
                                expr: *member_ty,
                            }
                            .unloc(),
                        ),
                    );
                }
                let member_list = types.arena.member_lists.add(
                    MemberList {
                        members: member_types,
                        parent: None,
                    }
                    .unloc(),
                );
                match expr {
                    Expr::Struct(_) => {
                        let expr = types.arena.exprs.add(Expr::Struct(member_list).unloc());
                        types.map.insert(self, expr);
                    }
                    Expr::Enum(_) => {
                        let expr = types.arena.exprs.add(Expr::Enum(member_list).unloc());
                        types.map.insert(self, expr);
                    }
                    _ => unreachable!(),
                };

                Ok(())
            }
            Expr::Call(func, args) => {
                // assign types to the function call and the arguments
                func.assign_types(arena, types)?;
                args.assign_types(arena, types)?;

                // if we have a resolved identifier, get it
                let mut func = *func;
                if let Expr::Ident(ident) = &arena.exprs.get(func).val {
                    if let Ident::Resolved(expr) = ident.val {
                        func = expr;
                    }
                }

                let func_loc = arena.exprs.get(func).loc;
                let args_loc = arena.member_lists.get(*args).loc;

                // make sure we actually have a function type (TODO: constructors)
                match arena.exprs.get(func).val {
                    Expr::Func(fargs, fbody) => {
                        // assign types to the function's arguments and body
                        fargs.assign_types(arena, types)?;
                        fbody.assign_types(arena, types)?;

                        // make sure the argument types match (TODO: implement)
                        // let comparison = args.type_compare(fargs, arena, types);
                        // if !comparison.is_subtype() {
                        //     return Err(TypeError::FunctionArgumentMismatch);
                        // }

                        // the type of the call expression equals the (return) type of the function body
                        types.map.insert(self, *types.map.get(fbody).unwrap());

                        Ok(())
                    }
                    Expr::Primitive(Primitive::Binop(op)) => {
                        // there should be two arguments
                        let args = &arena.member_lists.get(*args).val;
                        if args.members.len() != 2 {
                            unreachable!();
                        }

                        // make sure we in fact have two arguments (this should always be the case)
                        assert!(args.members.len() == 2);

                        // check the type of the arguments
                        let lhs = &arena.members.get(args.members[0]).val;
                        let rhs = &arena.members.get(args.members[1]).val;
                        assert!(matches!((lhs.field.val), Field::Number(_)));
                        assert!(matches!((rhs.field.val), Field::Number(_)));

                        // for now we only support math operations
                        if !matches!(
                            op,
                            Operator::Plus | Operator::Minus | Operator::Times | Operator::Divide
                        ) {
                            println!("WARNING: UNSUPPORTED OPERATOR {:?}", op);
                        }

                        // match on the argument types
                        let lhs_ty = types.arena.exprs.get(*types.map.get(lhs.expr).unwrap());
                        let rhs_ty = types.arena.exprs.get(*types.map.get(rhs.expr).unwrap());
                        let ret_ty = match (&lhs_ty.val, &rhs_ty.val) {
                            (
                                Expr::Primitive(Primitive::U64Val(x)),
                                Expr::Primitive(Primitive::U64Val(y)),
                            ) => {
                                // perform constant folding on two known values
                                let result = Expr::Primitive(Primitive::U64Val(x + y));
                                types.arena.exprs.add(result.node(*loc))
                            }
                            (
                                Expr::Primitive(Primitive::U64Ty | Primitive::U64Val(_)),
                                Expr::Primitive(Primitive::U64Ty | Primitive::U64Val(_)),
                            ) => types.u64ty,
                            (Expr::Primitive(Primitive::Empty), _) => types.empty,
                            (_, Expr::Primitive(Primitive::Empty)) => types.empty,
                            _ => {
                                return Err(TypeError::IllegalBinopType(self, op));
                            }
                        };

                        // return a u64 type
                        types.map.insert(self, ret_ty);
                        Ok(())
                    }
                    Expr::Primitive(Primitive::DebugPrint | Primitive::Assert(_)) => {
                        // these functions return unit type
                        types.map.insert(self, types.unit);
                        Ok(())
                    }
                    Expr::Struct(member_list) => {
                        // assign types to the struct members
                        member_list.assign_types(arena, types)?;

                        let members = &arena.member_lists.get(member_list).val.members;
                        let mut member_types = Vec::new();
                        for member in members {
                            let member = &arena.members.get(*member);
                            let member_ty = types.map.get(member.val.expr).unwrap();

                            // TODO: check subtyping

                            member_types.push(
                                types.arena.members.add(
                                    Member {
                                        field: member.val.field.clone(),
                                        expr: *member_ty,
                                    }
                                    .node(member.loc),
                                ),
                            );
                        }
                        let struct_ty = Expr::Struct(
                            types.arena.member_lists.add(
                                MemberList {
                                    parent: None,
                                    members: member_types,
                                }
                                .node(args_loc),
                            ),
                        );
                        types
                            .map
                            .insert(self, types.arena.exprs.add(struct_ty.node(*loc)));

                        Ok(())
                    }
                    _ => Err(TypeError::ExpectedFunction(arena.exprs.get(func).clone())),
                }
            }
            Expr::Func(args, body) => {
                // assign types to the arguments and the body
                args.assign_types(arena, types)?;
                body.assign_types(arena, types)?;

                // build up a function type
                let args = &arena.member_lists.get(*args);
                let args_loc = args.loc;
                let args = &args.val;
                let mut arg_types = Vec::new();
                for arg in &args.members {
                    let member = &arena.members.get(*arg);
                    let member_loc = member.loc;
                    let member = &member.val;
                    arg_types.push(
                        types.arena.members.add(
                            Member {
                                field: member.field.clone(),
                                expr: *types.map.get(member.expr).unwrap(),
                            }
                            .node(member_loc),
                        ),
                    );
                }
                let body_type = types.map.get(*body).unwrap();
                let args_type = types.arena.member_lists.add(
                    MemberList {
                        members: arg_types,
                        parent: None,
                    }
                    .node(args_loc),
                );
                let func_type = types
                    .arena
                    .exprs
                    .add(Expr::Func(args_type, *body_type).node(*loc));

                // add the function type to the map
                types.map.insert(self, func_type);

                Ok(())
            }
            Expr::Block(block) => {
                block.assign_types(arena, types)?;
                let last_stmt = arena.blocks.get(*block).val.stmts.last();
                let last_stmt = last_stmt.map(|stmt| &arena.stmts.get(*stmt).val);
                match last_stmt {
                    Some(Stmt {
                        ident: None, ty, ..
                    }) => {
                        types.map.insert(self, *ty);
                    }
                    _ => {
                        types.map.insert(self, types.unit);
                    }
                }
                Ok(())
            }
            Expr::Array(elements) => {
                elements.assign_types(arena, types);
                for elem in &elements.val.elements {}
                Ok(())
            }
            Expr::Vector(handle, handle1) => {
                // TODO
                Ok(())
            }
            Expr::Primitive(primitive) => {
                let primitive = types
                    .arena
                    .exprs
                    .add(Expr::Primitive(primitive.clone()).node(*loc));
                types.map.insert(self, primitive);
                Ok(())
            }
        }
    }
}

impl AssignTypes for Handle<Node<MemberList>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let member_list = &arena.member_lists.get(self).val;
        for member in &member_list.members {
            member.assign_types(arena, types)?;
        }
        Ok(())
    }
}

impl AssignTypes for Handle<Node<Block>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let block = &arena.blocks.get(self).val;
        for stmt in &block.stmts {
            stmt.assign_types(arena, types)?;
        }
        Ok(())
    }
}

impl AssignTypes for Handle<Node<Member>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let member = &arena.members.get(self).val;
        member.expr.assign_types(arena, types)?;
        Ok(())
    }
}

impl AssignTypes for Handle<Node<Stmt>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let stmt = &arena.stmts.get(self).val;
        stmt.ty.assign_types(arena, types)?;
        stmt.value.assign_types(arena, types)?;
        Ok(())
    }
}

impl AssignTypes for &Node<Array> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        for elem in &self.val.elements {
            elem.assign_types(arena, types)?;
        }
        Ok(())
    }
}
