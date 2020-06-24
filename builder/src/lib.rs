extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::punctuated::Punctuated;
use syn::*;

struct AttrInfo {
    arg: LitStr,
    inner_ty: Type,
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let output = builder_output(&input).unwrap_or_else(|err| err.to_compile_error());
    proc_macro::TokenStream::from(output)
}

fn builder_output(input: &DeriveInput) -> Result<TokenStream> {
    let name = &input.ident;
    let data = &input.data;
    let bname = format_ident!("{}Builder", name);
    let named: Punctuated<Field, Token![,]> = Punctuated::new();
    let named = match named_fields(data) {
        Some(f) => f,
        None => &named,
    };

    let mut fields: Vec<TokenStream> = Vec::new();
    let mut args: Vec<TokenStream> = Vec::new();
    let mut setters: Vec<TokenStream> = Vec::new();
    let mut fn_args: Vec<TokenStream> = Vec::new();

    for f in named {
        let name = &f.ident;
        let ty = &f.ty;

        if let Some(info) = match parse_attr(f) {
            Some(Ok(info)) => Some(info),
            Some(Err(err)) => return Err(err),
            _ => None,
        } {
            fields.push(quote! { #name: #ty, });
            args.push(quote! { #name: Vec::new(), });
            let attr_arg = format_ident!("{}", &info.arg.value());
            let inner_ty = &info.inner_ty;
            setters.push(quote! {
                pub fn #attr_arg(&mut self, #attr_arg: #inner_ty) -> &mut Self {
                    self.#name.push(#attr_arg);
                    self
                }
            });
            fn_args.push(quote! { #name: self.#name.clone(), });
        } else if let Some(ty) = inner_for(ty, "Option") {
            fields.push(quote! { #name: std::option::Option<#ty>, });
            args.push(quote! { #name: std::option::Option::None, });
            setters.push(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            });
            fn_args.push(quote! { #name: self.#name.clone(), });
        } else {
            fields.push(quote! { #name: std::option::Option<#ty>, });
            args.push(quote! { #name: std::option::Option::None, });
            setters.push(quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            });
            let err = format!("{} is not set", name.clone().unwrap());
            fn_args.push(quote! {
                #name: self.#name.clone().ok_or(#err)?,
            });
        }
    }

    Ok(quote! {
        pub struct #bname {
            #(#fields)*
        }
        impl #bname {
            #(#setters)*
            pub fn build(
                &mut self
            ) -> std::result::Result<Command, std::boxed::Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#fn_args)*
                })
            }
        }
        impl #name {
            pub fn builder() -> #bname {
                #bname {
                    #(#args)*
                }
            }
        }
    })
}

fn named_fields(data: &Data) -> Option<&Punctuated<Field, Token![,]>> {
    match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { ref named, .. }),
            ..
        }) => Some(named),
        _ => None,
    }
}

fn inner_for<'a>(ty: &'a Type, wrap: &str) -> Option<&'a Type> {
    let tp = match ty {
        Type::Path(ref tp) => tp,
        _ => return None,
    };

    let segment = &tp.path.segments.first()?;
    if segment.ident != wrap {
        return None;
    }

    let inner_ty = match segment.arguments {
        PathArguments::AngleBracketed(ref inner_ty) => inner_ty,
        _ => return None,
    };

    if inner_ty.args.len() != 1 {
        return None;
    }

    match inner_ty.args.first()? {
        GenericArgument::Type(ref t) => Some(t),
        _ => None,
    }
}

fn parse_attr(field: &Field) -> Option<Result<AttrInfo>> {
    let attr = field.attrs.first()?;
    let meta = attr.parse_meta().ok()?;

    let (path, nested) = match meta {
        Meta::List(MetaList {
            ref path,
            ref nested,
            ..
        }) => (path, nested),
        _ => return None,
    };

    let ident = path.get_ident()?;
    if ident != "builder" {
        return None;
    }

    let (path, lit) = match nested.first()? {
        NestedMeta::Meta(Meta::NameValue(MetaNameValue {
            ref path, ref lit, ..
        })) => (path, lit),
        _ => return Some(Err(Error::new_spanned(nested, "expected NameValue"))),
    };

    let ident = match path.get_ident() {
        Some(ident) => ident,
        None => return Some(Err(Error::new_spanned(path, "expected Ident"))),
    };
    if ident != "each" {
        return Some(Err(Error::new_spanned(
            meta,
            "expected `builder(each = \"...\")`",
        )));
    }

    let litstr = match lit {
        Lit::Str(ref litstr) => litstr,
        _ => return Some(Err(Error::new_spanned(lit, "expected LitStr"))),
    };

    match inner_for(&field.ty, "Vec") {
        Some(ty) => Some(Ok(AttrInfo {
            arg: litstr.clone(),
            inner_ty: ty.clone(),
        })),
        None => Some(Err(Error::new_spanned(&field.ty, "expected Vec"))),
    }
}
