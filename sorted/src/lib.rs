extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::visit_mut::VisitMut;
use syn::*;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as AttributeArgs);
    let input = parse_macro_input!(input as Item);
    let mut output = quote! {#input};
    if let Err(err) = sorted_output(&input) {
        output.extend(err.to_compile_error());
    }
    TokenStream::from(output)
}

fn sorted_output(item: &Item) -> Result<()> {
    match item {
        Item::Enum(ref e) => enum_sorted(e),
        _ => Err(Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn enum_sorted(e: &ItemEnum) -> Result<()> {
    let mut names = Vec::new();
    for v in &e.variants {
        let current = &v.ident;
        for name in &names {
            if &current < name {
                return Err(Error::new(
                    current.span(),
                    format!("{} should sort before {}", current, name),
                ));
            }
        }
        names.push(current);
    }
    Ok(())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as AttributeArgs);
    let mut input = parse_macro_input!(input as ItemFn);
    let mut fn_with_match = FnWithMatch { result: Ok(()) };
    fn_with_match.visit_item_fn_mut(&mut input);
    let mut output = quote! {#input};
    if let Err(err) = fn_with_match.result {
        output.extend(err.to_compile_error());
    }
    TokenStream::from(output)
}

struct FnWithMatch {
    result: Result<()>,
}

impl Default for FnWithMatch {
    fn default() -> Self {
        FnWithMatch { result: Ok(()) }
    }
}

impl VisitMut for FnWithMatch {
    fn visit_expr_match_mut(&mut self, node: &mut ExprMatch) {
        if node.attrs.iter().any(|a| a.path.is_ident("sorted")) {
            node.attrs.retain(|a| !a.path.is_ident("sorted"));
            self.result = arms_sorted(&node.arms);
        }

        visit_mut::visit_expr_match_mut(self, node);
    }
}

fn arms_sorted(arms: &[Arm]) -> Result<()> {
    let mut paths: Vec<Path> = Vec::new();
    let mut iter = arms.iter().peekable();
    while let Some(arm) = iter.next() {
        if let Pat::Wild(wild) = &arm.pat {
            if iter.peek().is_some() {
                return Err(Error::new_spanned(wild, "'_' should be last"));
            }
        } else {
            let current = arm_pat_name(&arm.pat)?;
            let current_name = path_to_string(&current);
            for path in &paths {
                let path_name = path_to_string(path);
                if current_name < path_name {
                    return Err(Error::new_spanned(
                        current,
                        format!("{} should sort before {}", current_name, path_name),
                    ));
                }
            }
            paths.push(current);
        }
    }
    Ok(())
}

fn path_to_string(p: &Path) -> String {
    p.segments
        .iter()
        .enumerate()
        .map(|(i, s)| {
            if i == 0 && p.leading_colon.is_none() {
                format!("{}", s.ident)
            } else {
                format!("::{}", s.ident)
            }
        })
        .collect()
}

fn arm_pat_name(pat: &Pat) -> Result<Path> {
    match pat {
        Pat::Ident(pi) => Ok(pi.ident.clone().into()),
        Pat::Path(pp) => Ok(pp.path.clone()),
        Pat::TupleStruct(pts) => Ok(pts.path.clone()),
        Pat::Struct(ps) => Ok(ps.path.clone()),
        p => Err(Error::new_spanned(p, "unsupported by #[sorted]")),
    }
}
