use type_compare::TypeCompare;

use crate::{
    arena::{ArenaMap, Handle},
    ast::{desugar::*, Node},
};

/// Compares two types
pub mod type_compare;

/// The result of type assignments, a mapping from desugared expression handles to types
pub struct TypeArena {
    types: ArenaMap<Node<Expr>, Expr>,
    recursive_idents: Vec<Ident>,
}

impl TypeArena {
    /// Constructs a new, empty [TypeArena]
    pub fn new_empty() -> Self {
        Self {
            types: ArenaMap::default(),
            recursive_idents: Vec::new(),
        }
    }
}

/// An error that can occur during type checking
#[derive(Clone, Debug)]
pub enum TypeError {
    /// Expected a function, but found something else
    ExpectedFunction,

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
}

/// Indicates that types can be assigned to an expression
pub trait AssignTypes: Sized {
    /// Assigns types to the given type
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError>;
}

impl AssignTypes for Handle<Node<Expr>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let expr = &arena.exprs.get(self).val;
        match expr {
            Expr::Ident(ident) => match ident.val {
                Ident::Resolved(expr) => {
                    if types.recursive_idents.contains(&ident.val) {
                        return Err(TypeError::RecursiveIdent(ident.clone()));
                    } else if types.types.get(self).is_none() {
                        expr.assign_types(arena, types)?;
                    }
                    Ok(())
                }
                _ => Err(TypeError::UnresolvedIdentifier(ident.clone())),
            },
            Expr::Index(handle, handle1) => todo!("typecheck index"),
            Expr::Field(expr, field) => {
                // assign types to the struct
                expr.assign_types(arena, types)?;

                // make sure we actually have a struct
                let Expr::Struct(member_list) = arena.exprs.get(*expr).val else {
                    return Err(TypeError::ExpectedStruct(arena.exprs.get(*expr).clone()));
                };

                let member_list = &arena.member_lists.get(member_list).val;
                for member in &member_list.members {
                    let member = &arena.members.get(*member).val;
                    if member.field.val == field.val {
                        return Ok(());
                    }
                }

                Err(TypeError::UnrecognizedStructField(field.clone()))
            }
            Expr::Struct(member_list) | Expr::Enum(member_list) => {
                // assign types to each member
                member_list.assign_types(arena, types)?;
                Ok(())
            }
            Expr::Call(func, args) => {
                // assign types to the function call and the arguments
                func.assign_types(arena, types)?;
                args.assign_types(arena, types)?;

                // make sure we actually have a function type (TODO: constructors)
                let Expr::Func(fargs, fbody) = arena.exprs.get(*func).val else {
                    return Err(TypeError::ExpectedFunction);
                };

                // assign types to the function's arguments and body
                fargs.assign_types(arena, types)?;
                fbody.assign_types(arena, types)?;

                // make sure the argument types match
                let comparison = args.type_compare(fargs, arena, types);
                if !comparison.is_subtype() {
                    return Err(TypeError::FunctionArgumentMismatch);
                }

                // the type of the call expression equals the (return) type of the function body
                types
                    .types
                    .insert(self, types.types.get(fbody).unwrap().clone());

                Ok(())
            }
            Expr::Func(args, body) => {
                // assign types to the arguments and the body
                args.assign_types(arena, types)?;
                body.assign_types(arena, types)?;

                Ok(())
            }
            Expr::Block(block) => {
                block.assign_types(arena, types)?;
                Ok(())
            }
            Expr::Array(elements) => todo!("typecheck array"),
            Expr::Vector(handle, handle1) => todo!("typecheck array type"),
            Expr::Primitive(primitive) => {
                types.types.insert(self, Expr::Primitive(primitive.clone()));
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
        if let Field::Ident(ident) = &member.field.val {
            println!("recursive ident {:?}", ident.val);
            types.recursive_idents.push(ident.val.clone());
        }
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
