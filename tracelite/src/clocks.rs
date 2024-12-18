use std::sync::{atomic, Arc};
use std::time::UNIX_EPOCH;


pub trait Clock: Send + Sync + 'static {
    fn now_unix_nano(&self) -> u64;
}

pub struct StdClock;

impl Clock for StdClock {
    fn now_unix_nano(&self) -> u64 {
        std::time::SystemTime::now()
            .duration_since(UNIX_EPOCH).unwrap()
            .as_nanos() as u64
    }
}

#[derive(Default, Clone)]
pub struct TestClock(Arc<atomic::AtomicU64>);

impl TestClock {
    pub fn advance(&self, by: u64){
        self.0.fetch_add(by, atomic::Ordering::SeqCst);
    }
}

impl Clock for TestClock {
    fn now_unix_nano(&self) -> u64 {
        self.0.load(atomic::Ordering::SeqCst)
    }
}