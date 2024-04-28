pub trait StateMonad<'a, In, Out>: Sized + Copy + 'a {
    fn apply(self, input: In) -> (Out, In);

    fn map<NewOut, F>(self, map_fn: F) -> impl StateMonad<'a, In, NewOut>
    where
        F: FnOnce(Out) -> NewOut + Copy + 'a
    {
        move |input| {
            let (out, rest) = self.apply(input);
            (map_fn(out), rest)
        }
    }

    fn and_then<P, F, NewOut>(self, and_then_fn: F) -> impl StateMonad<'a, In, NewOut>
    where
        P: StateMonad<'a, In, NewOut>,
        F: FnOnce(Out) -> P + Copy + 'a
    {
        move |input| {
            let (out, rest) = self.apply(input);
            and_then_fn(out).apply(rest)
        }
    }
}

impl<'a, In, Out, F> StateMonad<'a, In, Out> for F
    where
        F: FnOnce(In) -> (Out, In) + Sized + Copy + 'a,
{
    fn apply(self, input: In) -> (Out, In) {
        self(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_monad_map<'a>() {
        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (out, rest)
        };

        let monad = monad.map(|c: &str| c.to_uppercase());
        let (out, rest) = monad.apply("foo");
        assert_eq!(out, "F");
        assert_eq!(rest, "oo");
    }

    #[test]
    fn test_state_monad_and_then<'a>() {
        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (out, rest)
        };

        let monad = monad.and_then(|_| move |input: &'a str| {
                let (out, rest) = input.split_at(1);
                (out, rest)
        });
        let (out, rest) = monad.apply("bar");
        assert_eq!(out, "a");
        assert_eq!(rest, "r");
    }
}