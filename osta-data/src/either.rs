use osta_proc_macros::impl_either_unwrap;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl_either_unwrap!(20);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_either_unwrap_left() {
        let either: Either<Either<i32, i32>, i32> = Either::Left(Either::Left(42));
        assert_eq!(either.unwrap(), 42);
    }

    #[test]
    fn test_either_unwrap_right() {
        let either: Either<i32, Either<i32, i32>> = Either::Right(Either::Right(42));
        assert_eq!(either.unwrap(), 42);
    }

    #[test]
    fn test_either_unwrap_mixed() {
        let either: Either<Either<i32, i32>, i32> = Either::Left(Either::Right(42));
        assert_eq!(either.unwrap(), 42);
    }
}
