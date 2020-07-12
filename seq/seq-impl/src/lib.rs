extern crate proc_macro;

use parse::{Parse, ParseStream};
use proc_macro::TokenStream;
use proc_macro2::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, TokenStream as TokenStream2,
    TokenTree,
};
use proc_macro_hack::proc_macro_hack;
use syn::*;

struct SeqMacroInput {
    expansion_var: Ident,
    from: u16,
    to: u16,
    inclusive: bool,
    tokens: TokenStream2,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let expansion_var: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let from: LitInt = input.parse()?;
        let from = from.base10_parse::<u16>()?;
        let inclusive = input.peek(Token![..=]);
        if inclusive {
            <Token![..=]>::parse(input)?;
        } else {
            <Token![..]>::parse(input)?;
        }
        let to: LitInt = input.parse()?;
        let to = to.base10_parse::<u16>()?;
        let content;
        let _brace_token = braced!(content in input);
        let tokens = content.parse()?;

        Ok(SeqMacroInput {
            expansion_var,
            from,
            to,
            inclusive,
            tokens,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as SeqMacroInput);
    let output = seq.expand_seq();
    TokenStream::from(output)
}

#[proc_macro_hack]
pub fn eseq(input: TokenStream) -> TokenStream {
    seq(input)
}

impl SeqMacroInput {
    fn expand_seq(&self) -> TokenStream2 {
        match self.expand_repeat(self.tokens.clone()) {
            (true, out) => out,
            _ => self
                .range()
                .map(|i| self.expand_pass(self.tokens.clone(), i))
                .collect(),
        }
    }

    fn expand_pass(&self, stream: TokenStream2, i: u16) -> TokenStream2 {
        let mut ts = TokenStream2::new();
        let mut iter = stream.into_iter();
        while let Some(tt) = iter.next() {
            let tree = match tt {
                TokenTree::Ident(ref ident) if &self.expansion_var == ident => {
                    TokenTree::Literal(Literal::u16_unsuffixed(i))
                }
                TokenTree::Ident(ref ident) => self.cat_ident(ident, &mut iter, i),
                TokenTree::Group(group) => TokenTree::Group(Group::new(
                    group.delimiter(),
                    self.expand_pass(group.stream(), i),
                )),
                tt => tt,
            };
            ts.extend(std::iter::once(tree));
        }
        ts
    }

    fn expand_repeat(&self, stream: TokenStream2) -> (bool, TokenStream2) {
        let mut ts = TokenStream2::new();
        let mut iter = stream.into_iter();
        let mut found = false;
        while let Some(tt) = iter.next() {
            let mut peek = iter.clone();
            let s = match (tt, peek.next(), peek.next()) {
                (
                    TokenTree::Punct(ref p1),
                    Some(TokenTree::Group(ref group)),
                    Some(TokenTree::Punct(ref p2)),
                ) if p1.as_char() == '#'
                    && group.delimiter() == Delimiter::Parenthesis
                    && p2.as_char() == '*' =>
                {
                    iter.nth(1);
                    found = true;
                    self.range()
                        .map(|i| self.expand_pass(group.stream(), i))
                        .collect()
                }
                (TokenTree::Group(group), _, _) => {
                    let (f, out) = self.expand_repeat(group.stream());
                    found = f;
                    TokenStream2::from(TokenTree::Group(Group::new(group.delimiter(), out)))
                }
                (tt, _, _) => TokenStream2::from(tt),
            };
            ts.extend(s);
        }
        (found, ts)
    }

    fn cat_ident(&self, prefix: &Ident, iter: &mut IntoIter, i: u16) -> TokenTree {
        let mut peek = iter.clone();
        let tt = match (peek.next(), peek.next()) {
            (Some(TokenTree::Punct(ref punct)), Some(TokenTree::Ident(ref ident)))
                if punct.as_char() == '#' && &self.expansion_var == ident =>
            {
                iter.nth(1);
                TokenTree::Ident(Ident::new(&format!("{}{}", prefix, i), prefix.span()))
            }
            _ => return TokenTree::Ident(prefix.clone()),
        };
        match (peek.next(), peek.next()) {
            (Some(TokenTree::Punct(ref punct)), Some(TokenTree::Ident(ref suffix)))
                if punct.as_char() == '#' =>
            {
                iter.nth(1);
                TokenTree::Ident(Ident::new(&format!("{}{}", tt, suffix), tt.span()))
            }
            _ => tt,
        }
    }

    fn range(&self) -> std::ops::Range<u16> {
        if self.inclusive {
            self.from..self.to + 1
        } else {
            self.from..self.to
        }
    }
}
