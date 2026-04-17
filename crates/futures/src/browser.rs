
pub fn block<F: Future>(fut: F) -> F::Output {
    let pinned = Box::pin(fut);
}
