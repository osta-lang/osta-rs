use osta_ast::{Data, NodeKind};
use osta_lexer::base::{token, TokenEmitter};
use crate::{Parser, ParserError, ParserInput};

fn data<'a, E>(emitter: E) -> impl Parser<'a>
where
    E: TokenEmitter<'a>
{
    move |input: ParserInput<'a>| {
        let (result, rest) = emitter.apply(input.input);
        match result {
            Ok(out) => {
                let mut builder = input.builder.borrow_mut();
                let data_ref = builder.push_data(Data::Token(out));
                let node_ref = builder.push_node(NodeKind::Data(data_ref), !0); // TODO: Node must have a daddy
                (Ok((node_ref, Some(data_ref))), ParserInput { input: rest, builder: input.builder.clone() })
            },
            Err(err) => (Err(ParserError::TokenizerError(err)), input)
        }
    }
}

pub fn term<'a>() -> impl Parser<'a> {
    move |_| todo!()
}