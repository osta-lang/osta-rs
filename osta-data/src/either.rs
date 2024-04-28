use osta_proc_macros::impl_either_unwrap;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl_either_unwrap!(10);
