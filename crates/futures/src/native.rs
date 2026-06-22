
use futures_lite::future::block_on;

pub fn blockon<F: Future>(fut: F) -> F::Output {
    block_on(fut)
}
