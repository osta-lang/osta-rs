use osta_data::either::Either;
use crate::monads::fallible_state_monad::FallibleStateMonad;

pub fn pair<'a, M1, M2, In: 'a, Out1: Copy + 'a, Out2: 'a, Err1: 'a, Err2: 'a>(
    left: M1,
    right: M2
) -> impl FallibleStateMonad<'a, In, (Out1, Out2), Either<Err1, Err2>>
where
    M1: FallibleStateMonad<'a, In, Out1, Err1>,
    M2: FallibleStateMonad<'a, In, Out2, Err2>,
{
    FallibleStateMonad::and_then(left, move |out1| right.map_out(move |out2| (out1, out2)))
}

pub fn either<'a, M1, M2, In: 'a, Out1: 'a, Out2: 'a, Err1: Copy + 'a, Err2: 'a>(
    left: M1,
    right: M2
) -> impl FallibleStateMonad<'a, In, Either<Out1, Out2>, (Err1, Err2)>
where
    M1: FallibleStateMonad<'a, In, Out1, Err1>,
    M2: FallibleStateMonad<'a, In, Out2, Err2>,
{
    FallibleStateMonad::or_else(left, move |err1| right.map_err(move |err2| (err1, err2)))
}
