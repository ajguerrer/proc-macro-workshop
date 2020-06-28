extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::quote;
use syn::*;

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let output = custom_debug_output(&input).unwrap_or_else(|err| err.to_compile_error());
    proc_macro::TokenStream::from(output)
}

fn custom_debug_output(input: &DeriveInput) -> Result<TokenStream> {
    let name = &input.ident;
    let name_string = format!("{}", name);
    let fields = named_fields(&input.data);

    let field_chain = fields.iter().filter_map(|f| {
        let name = f.ident.as_ref()?;
        let name_string = format!("{}", name);
        match debug_attr(&f.attrs) {
            Some(arg) => Some(quote! {.field(#name_string, &format_args!(#arg, &self.#name))}),
            None => Some(quote! {.field(#name_string, &self.#name)}),
        }
    });

    let predicates = if let Some(pred) = bound_predicate(&input.attrs) {
        vec![pred]
    } else {
        fields
            .iter()
            .filter_map(|f| {
                bound_predicate(&f.attrs)
                    .or_else(|| generic_predicate(&f, struct_generics(&input.generics)))
            })
            .collect()
    };

    let generics = add_debug_where_clause(&input.generics, predicates);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let phantom_data: bool = fields.iter().any(|f| phantom_data(f));
    let where_clause = if phantom_data {
        quote! {}
    } else {
        quote! {#where_clause}
    };

    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause  {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#name_string)
                #(#field_chain)*
                .finish()
            }
        }
    })
}

fn add_debug_where_clause(generics: &Generics, predicates: Vec<WherePredicate>) -> Generics {
    let mut new_generics = generics.clone();
    let where_clause = new_generics.make_where_clause();

    for predicate in predicates {
        where_clause.predicates.push(predicate);
    }

    new_generics
}

fn named_fields(data: &Data) -> Vec<&Field> {
    match data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { ref named, .. }),
            ..
        }) => named.iter().collect(),
        _ => Vec::new(),
    }
}

fn debug_attr(attrs: &[Attribute]) -> Option<LitStr> {
    for attr in attrs {
        if let Some(litstr) = parse_debug_attr(attr) {
            return Some(litstr);
        }
    }
    None
}

fn parse_debug_attr(attr: &Attribute) -> Option<LitStr> {
    let meta = attr.parse_meta().ok()?;
    match meta {
        Meta::NameValue(ref name_value) => litstr_from_name_value(name_value, "debug"),
        _ => None,
    }
}

fn bound_predicate(attrs: &[Attribute]) -> Option<WherePredicate> {
    for attr in attrs {
        if let Some(litstr) = parse_bound_attr(attr) {
            return Some(parse_str::<WherePredicate>(&litstr.value()).ok()?);
        }
    }
    None
}

fn parse_bound_attr(attr: &Attribute) -> Option<LitStr> {
    let meta = attr.parse_meta().ok()?;
    match meta {
        Meta::List(MetaList {
            ref path,
            ref nested,
            ..
        }) => {
            if path.segments.first()?.ident != "debug" {
                return None;
            }
            for meta in nested {
                if let Some(litstr) = match meta {
                    NestedMeta::Meta(Meta::NameValue(ref name_value)) => {
                        litstr_from_name_value(name_value, "bound")
                    }
                    _ => None,
                } {
                    return Some(litstr);
                }
            }
            None
        }
        _ => None,
    }
}

fn litstr_from_name_value(name_value: &MetaNameValue, name: &str) -> Option<LitStr> {
    if name_value.path.segments.first()?.ident != name {
        return None;
    }
    match &name_value.lit {
        Lit::Str(litstr) => Some(litstr.clone()),
        _ => None,
    }
}

fn generic_predicate(field: &Field, struct_generics: Vec<&Ident>) -> Option<WherePredicate> {
    for generic in struct_generics {
        if let Some(path) = find_generic_in_field(&field.ty, generic) {
            return Some(parse_quote! {#path: std::fmt::Debug});
        }
    }
    None
}

fn find_generic_in_field<'a>(ty: &'a Type, generic: &Ident) -> Option<&'a Path> {
    let path = match ty {
        Type::Path(TypePath { ref path, .. }) => Some(path),
        _ => None,
    }?;

    find_generic_in_path(path, generic)
}

fn find_generic_in_path<'a>(path: &'a Path, generic: &Ident) -> Option<&'a Path> {
    for segment in &path.segments {
        if &segment.ident == generic {
            return Some(path);
        }

        let args = match segment.arguments {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) => {
                Some(args)
            }
            _ => None,
        }?;
        for arg in args {
            let path = match arg {
                GenericArgument::Type(Type::Path(TypePath { ref path, .. })) => Some(path),
                _ => None,
            }?;
            if let Some(path) = find_generic_in_path(path, generic) {
                return Some(path);
            }
        }
    }
    None
}

fn struct_generics(generics: &Generics) -> Vec<&Ident> {
    generics.type_params().map(|param| &param.ident).collect()
}

fn phantom_data(field: &Field) -> bool {
    let segment = match &field.ty {
        Type::Path(TypePath { ref path, .. }) => path.segments.first(),
        _ => None,
    };

    match segment {
        Some(s) => s.ident == "PhantomData",
        None => false,
    }
}
