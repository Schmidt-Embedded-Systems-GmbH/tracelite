// Copyright 2020 TiKV Project Authors. Licensed under Apache-2.0.

//! An attribute macro designed to eliminate boilerplate code for [`minitrace`](https://crates.io/crates/minitrace).

#![recursion_limit = "256"]
// Instrumenting the async fn is not as straight forward as expected because `async_trait` rewrites
// `async fn` into a normal fn which returns `Box<impl Future>`, and this stops the macro from
// distinguishing `async fn` from `fn`. The following code reused the `async_trait` probes from [tokio-tracing](https://github.com/tokio-rs/tracing/blob/6a61897a5e834988ad9ac709e28c93c4dbf29116/tracing-attributes/src/expand.rs).

extern crate proc_macro;
// #[macro_use]
// extern crate proc_macro_error;

use proc_macro::{Ident, TokenStream, TokenTree};
use proc_macro2::Span;
use quote::quote_spanned;
use syn::spanned::Spanned;
use syn::*;

#[proc_macro_attribute]
pub fn trace_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_trace_span", args, input)
}

#[proc_macro_attribute]
pub fn trace2_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_trace2_span", args, input)
}

#[proc_macro_attribute]
pub fn debug_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_debug_span", args, input)
}

#[proc_macro_attribute]
pub fn debug2_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_debug2_span", args, input)
}

#[proc_macro_attribute]
pub fn info_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_info_span", args, input)
}

#[proc_macro_attribute]
pub fn info2_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_info2_span", args, input)
}

#[proc_macro_attribute]
pub fn warn_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_warn_span", args, input)
}

#[proc_macro_attribute]
pub fn warn2_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_warn2_span", args, input)
}

#[proc_macro_attribute]
pub fn error_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_error_span", args, input)
}

#[proc_macro_attribute]
pub fn error2_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_error2_span", args, input)
}

#[proc_macro_attribute]
pub fn fatal_span(args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as proc_macro2::TokenStream);
    let input = parse_macro_input!(item as ItemFn);
    span_generate("new_fatal_span", args, input)
}

fn span_generate(
    span_macro: &str,
    args: proc_macro2::TokenStream,
    input: syn::ItemFn,
) -> proc_macro::TokenStream {
    let func_name = input.sig.ident.to_string();
    // check for async_trait-like patterns in the block, and instrument
    // the future instead of the wrapper
    let func_body = if let Some(internal_fun) = get_async_trait_info(&input.block, input.sig.asyncness.is_some()) {
        // let's rewrite some statements!
        match internal_fun.kind {
            // async-trait <= 0.1.43
            AsyncTraitKind::Function => {
                unimplemented!(
                    "Please upgrade the crate `async-trait` to a version higher than 0.1.44"
                )
            }
            // async-trait >= 0.1.44
            AsyncTraitKind::Async(async_expr) => {
                // fallback if we couldn't find the '__async_trait' binding, might be
                // useful for crates exhibiting the same behaviors as async-trait
                let instrumented_block = gen_block(span_macro, &func_name, &async_expr.block, true, false, &args);
                let async_attrs = &async_expr.attrs;
                quote::quote! {
                    Box::pin(#(#async_attrs) * #instrumented_block)
                }
            }
        }
    } else {
        gen_block(
            span_macro,
            &func_name,
            &input.block,
            input.sig.asyncness.is_some(),
            input.sig.asyncness.is_some(),
            &args,
        )
    };

    let ItemFn {
        attrs, vis, sig, ..
    } = input;

    let Signature {
        output: return_type,
        inputs: params,
        unsafety,
        constness,
        abi,
        ident,
        asyncness,
        generics:
            Generics {
                params: gen_params,
                where_clause,
                ..
            },
        ..
    } = sig;

    quote::quote!(
        #(#attrs) *
        #vis #constness #unsafety #asyncness #abi fn #ident<#gen_params>(#params) #return_type
        #where_clause
        {
            #func_body
        }
    )
    .into()
}

/// Instrument a block
fn gen_block(
    span_macro: &str,
    func_name: &str,
    block: &Block,
    async_context: bool,
    async_keyword: bool,
    args: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let span_macro = syn::Ident::new(span_macro, proc_macro2::Span::call_site());
    // Generate the instrumented function body.
    // If the function is an `async fn`, this will wrap it in an async block.
    // Otherwise, this will enter the span and then perform the rest of the body.
    if async_context {
        let block = quote_spanned!(block.span() => {
            tracelite::InSpan::in_span(
                async move { #block },
                tracelite::#span_macro!(#func_name, #args),
            )
        });

        if async_keyword {
            quote_spanned!(block.span() =>
                #block.await
            )
        } else {
            panic!("please find me");
            // block // TODO what happened here?
        }
    } else {
        quote_spanned!(block.span() =>
            tracelite::in_span(
                tracelite::#span_macro!(#func_name, #args),
                || #block
            )
        )
    }
}

enum AsyncTraitKind<'a> {
    // old construction. Contains the function
    Function,
    // new construction. Contains a reference to the async block
    Async(&'a ExprAsync),
}

struct AsyncTraitInfo<'a> {
    // statement that must be patched
    _source_stmt: &'a Stmt,
    kind: AsyncTraitKind<'a>,
}

// Get the AST of the inner function we need to hook, if it was generated
// by async-trait.
// When we are given a function annotated by async-trait, that function
// is only a placeholder that returns a pinned future containing the
// user logic, and it is that pinned future that needs to be instrumented.
// Were we to instrument its parent, we would only collect information
// regarding the allocation of that future, and not its own span of execution.
// Depending on the version of async-trait, we inspect the block of the function
// to find if it matches the pattern
// `async fn foo<...>(...) {...}; Box::pin(foo<...>(...))` (<=0.1.43), or if
// it matches `Box::pin(async move { ... }) (>=0.1.44). We the return the
// statement that must be instrumented, along with some other information.
// 'gen_body' will then be able to use that information to instrument the
// proper function/future.
// (this follows the approach suggested in
// https://github.com/dtolnay/async-trait/issues/45#issuecomment-571245673)
fn get_async_trait_info(block: &Block, block_is_async: bool) -> Option<AsyncTraitInfo<'_>> {
    // are we in an async context? If yes, this isn't a async_trait-like pattern
    if block_is_async {
        return None;
    }

    // list of async functions declared inside the block
    let inside_funs = block.stmts.iter().filter_map(|stmt| {
        if let Stmt::Item(Item::Fn(fun)) = &stmt {
            // If the function is async, this is a candidate
            if fun.sig.asyncness.is_some() {
                return Some((stmt, fun));
            }
        }
        None
    });

    // last expression of the block (it determines the return value
    // of the block, so that if we are working on a function whose
    // `trait` or `impl` declaration is annotated by async_trait,
    // this is quite likely the point where the future is pinned)
    let (last_expr_stmt, last_expr) = block.stmts.iter().rev().find_map(|stmt| {
        if let Stmt::Expr(expr) = stmt {
            Some((stmt, expr))
        } else {
            None
        }
    })?;

    // is the last expression a function call?
    let (outside_func, outside_args) = match last_expr {
        Expr::Call(ExprCall { func, args, .. }) => (func, args),
        _ => return None,
    };

    // is it a call to `Box::pin()`?
    let path = match outside_func.as_ref() {
        Expr::Path(path) => &path.path,
        _ => return None,
    };
    if !path_to_string(path).ends_with("Box::pin") {
        return None;
    }

    // Does the call take an argument? If it doesn't,
    // it's not gonna compile anyway, but that's no reason
    // to (try to) perform an out of bounds access
    if outside_args.is_empty() {
        return None;
    }

    // Is the argument to Box::pin an async block that
    // captures its arguments?
    if let Expr::Async(async_expr) = &outside_args[0] {
        // check that the move 'keyword' is present
        async_expr.capture?;

        return Some(AsyncTraitInfo {
            _source_stmt: last_expr_stmt,
            kind: AsyncTraitKind::Async(async_expr),
        });
    }

    // Is the argument to Box::pin a function call itself?
    let func = match &outside_args[0] {
        Expr::Call(ExprCall { func, .. }) => func,
        _ => return None,
    };

    // "stringify" the path of the function called
    let func_name = match **func {
        Expr::Path(ref func_path) => path_to_string(&func_path.path),
        _ => return None,
    };

    // Was that function defined inside of the current block?
    // If so, retrieve the statement where it was declared and the function itself
    let (stmt_func_declaration, _) = inside_funs
        .into_iter()
        .find(|(_, fun)| fun.sig.ident == func_name)?;

    Some(AsyncTraitInfo {
        _source_stmt: stmt_func_declaration,
        kind: AsyncTraitKind::Function,
    })
}

// Return a path as a String
fn path_to_string(path: &Path) -> String {
    use std::fmt::Write;
    // some heuristic to prevent too many allocations
    let mut res = String::with_capacity(path.segments.len() * 5);
    for i in 0..path.segments.len() {
        write!(res, "{}", path.segments[i].ident).expect("writing to a String should never fail");
        if i < path.segments.len() - 1 {
            res.push_str("::");
        }
    }
    res
}
