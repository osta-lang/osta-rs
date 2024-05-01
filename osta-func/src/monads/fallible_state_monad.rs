use osta_data::either::Either;

use crate::monads::state_monad::StateMonad;

pub trait FallibleStateMonad<'a, In: 'a + Clone, Out: 'a, Err: 'a>: StateMonad<'a, In, Result<Out, Err>> {
    fn map_out<NewOut: 'a, F>(
        self,
        map_fn: F
    ) -> impl FallibleStateMonad<'a, In, NewOut, Err>
    where
        F: FnOnce(Out) -> NewOut + Copy + 'a
{
    self.map(move |result| result.map(map_fn))
}

    fn map_err<NewErr: 'a, F>(
        self,
        map_fn: F
    ) -> impl FallibleStateMonad<'a, In, Out, NewErr>
    where
        F: FnOnce(Err) -> NewErr + Copy + 'a
{
    self.map(move |result| result.map_err(map_fn))
}

    fn and_then<P, F, NewOut: 'a, OtherErr: 'a>(
        self,
        and_then_fn: F
    ) -> impl FallibleStateMonad<'a, In, NewOut, Either<Err, OtherErr>>
    where
        P: FallibleStateMonad<'a, In, NewOut, OtherErr>,
        F: FnOnce(Out) -> P + Copy + 'a
    {
        move |input| {
            let (result, rest) = self.apply(input);
            match result {
                Ok(out) => and_then_fn(out).map_err(Either::Right).apply(rest),
                Err(err) => (Err(Either::Left(err)), rest),
            }
        }
    }

    fn or_else<P, F, OtherOut: 'a, NewErr: 'a>(
        self,
        or_else_fn: F
    ) -> impl FallibleStateMonad<'a, In, Either<Out, OtherOut>, NewErr>
    where
        P: FallibleStateMonad<'a, In, OtherOut, NewErr>,
        F: FnOnce(Err) -> P + Copy + 'a
    {
        move |input| {
            let (result, rest) = self.apply(input);
            match result {
                Ok(out) => (Ok(Either::Left(out)), rest),
                Err(err) => or_else_fn(err).map_out(Either::Right).apply(rest),
            }
        }
    }

    fn map_out_with_input<F, NewOut: 'a>(
        self,
        map_fn: F
    ) -> impl FallibleStateMonad<'a, In, NewOut, Err>
        where
            F: Fn(Out, In) -> NewOut + Copy + 'a
    {
        move |input: In| {
            let (result, rest) = self.apply(input);
            match result {
                Ok(out) => {
                    let out = map_fn(out, rest.clone());
                    (Ok(out), rest)
                },
                Err(err) => (Err(err), rest)
            }
        }
    }
}

impl<'a, In: 'a + Clone, Out: 'a, Err: 'a, M> FallibleStateMonad<'a, In, Out, Err> for M
where
    M: StateMonad<'a, In, Result<Out, Err>> + Sized,
{}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fallible_state_monad_map_out<'a>() {
        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (Ok::<&'a str, ()>(out), rest)
        };

        let monad = monad
            .map_out(|c: &str| c.to_uppercase());
        let (out, rest) = monad.apply("foo");
        assert_eq!(out, Ok("F".to_string()));
        assert_eq!(rest, "oo");
    }

    #[test]
    fn test_fallible_state_monad_map_err<'a>() {
        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (Ok(out), rest)
        };

        let monad = monad.map_err(|err: &str| err.len());
        let (out, rest) = monad.apply("foo");
        assert_eq!(out, Ok("f"));
        assert_eq!(rest, "oo");

        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (Err::<&'a str, usize>(out.len()), rest)
        };

        let monad = monad.map_err(|err| err * 2);
        let (out, rest) = monad.apply("foo");
        assert_eq!(out, Err(2));
        assert_eq!(rest, "oo");
    }
}