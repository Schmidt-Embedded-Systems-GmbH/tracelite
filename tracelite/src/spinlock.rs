
//! This crate provides a thread-safe data container.
//!
//! # Description
//!
//! This structure behaves a lot like a normal Mutex. There are some differences:
//!
//! - It may be used outside the runtime.
//!   - A normal mutex will fail when used without the runtime, this will just lock
//!   - When the runtime is present, it will call the deschedule function when appropriate
//! - No lock poisoning. When a fail occurs when the lock is held, no guarantees are made
//!
//! When calling rust functions from bare threads, such as C `pthread`s, this lock will be very
//! helpfull. In other cases however, you are encouraged to use the locks from the standard
//! library.
//!
//! # Simple example
//!
//! ```
//! use spinlock::Spinlock;
//! let spinlock = Spinlock::new(0);
//!
//! // Modify the data
//! {
//!     let mut data = spinlock.lock();
//!     *data = 2;
//! }
//!
//! // Read the data
//! let answer =
//! {
//!     let data = spinlock.lock();
//!     *data
//! };
//!
//! assert_eq!(answer, 2);
//! ```
//!
//! # Thread-safety example
//!
//! ```
//! use spinlock::Spinlock;
//! use std::sync::{Arc, Barrier};
//!
//! let numthreads = 1000;
//! let spinlock = Arc::new(Spinlock::new(0));
//!
//! // We use a barrier to ensure the readout happens after all writing
//! let barrier = Arc::new(Barrier::new(numthreads + 1));
//!
//! for _ in (0..numthreads)
//! {
//!     let my_barrier = barrier.clone();
//!     let my_lock = spinlock.clone();
//!     std::thread::spawn(move||
//!     {
//!         let mut guard = my_lock.lock();
//!         *guard += 1;
//!
//!         // Release the lock to prevent a deadlock
//!         drop(guard);
//!         my_barrier.wait();
//!     });
//! }
//!
//! barrier.wait();
//!
//! let answer = { *spinlock.lock() };
//! assert_eq!(answer, numthreads);
//! ```

#[cfg(test)]
extern crate std;

use core::sync::atomic::{AtomicBool, Ordering};
use core::cell::UnsafeCell;
use core::marker::Sync;
use core::ops::{Drop, Deref, DerefMut};

/// A wrapper for the data giving access in a thread-safe manner
pub struct Spinlock<T>
{
    lock: AtomicBool,
    data: UnsafeCell<T>,
}

/// A guard to which the protected data can be accessed
///
/// When the guard falls out of scope it will release the lock.
pub struct SpinlockGuard<'a, T:'a>
{
    lock: &'a AtomicBool,
    data: &'a mut T,
}

unsafe impl<T> Sync for Spinlock<T> {}

/// A Spinlock which may be used statically.
///
/// ```
/// use spinlock::{StaticSpinlock, STATIC_SPINLOCK_INIT};
///
/// static SPLCK: StaticSpinlock = STATIC_SPINLOCK_INIT;
///
/// fn demo() {
///     let lock = SPLCK.lock();
///     // do something with lock
///     drop(lock);
/// }
/// ```
pub type StaticSpinlock = Spinlock<()>;

/// A initializer for StaticSpinlock, containing no data.
pub const STATIC_SPINLOCK_INIT: StaticSpinlock = Spinlock {
    lock: AtomicBool::new(false),
    data: UnsafeCell::new(()),
};

//#[deprecated = "renamed to STATIC_SPINLOCK_INIT"]
pub const INIT_STATIC_SPINLOCK: StaticSpinlock = STATIC_SPINLOCK_INIT;

impl<T> Spinlock<T>
{
    /// Creates a new spinlock wrapping the supplied data.
    pub fn new(user_data: T) -> Spinlock<T>
    {
        Spinlock
        {
            lock: AtomicBool::new(false),
            data: UnsafeCell::new(user_data),
        }
    }

    fn obtain_lock(&self)
    {
        while self.lock.compare_and_swap(false, true, Ordering::SeqCst) != false
        {
            std::hint::spin_loop();
        }
    }

    /// Locks the spinlock and returns a guard.
    ///
    /// The returned value may be dereferenced for data access
    /// and the lock will be dropped when the guard falls out of scope.
    ///
    /// ```
    /// let mylock = spinlock::Spinlock::new(0);
    /// {
    ///     let mut data = mylock.lock();
    ///     // The lock is now locked and the data can be accessed
    ///     *data += 1;
    /// }
    /// // The lock is dropped
    /// ```
    pub fn lock(&self) -> SpinlockGuard<T>
    {
        self.obtain_lock();
        SpinlockGuard
        {
            lock: &self.lock,
            data: unsafe { &mut *self.data.get() },
        }
    }
}

impl<'a, T> Deref for SpinlockGuard<'a, T>
{
    type Target = T;
    fn deref<'b>(&'b self) -> &'b T { &*self.data }
}

impl<'a, T> DerefMut for SpinlockGuard<'a, T>
{
    fn deref_mut<'b>(&'b mut self) -> &'b mut T { &mut *self.data }
}

impl<'a, T> Drop for SpinlockGuard<'a, T>
{
    /// The dropping of the SpinlockGuard will release the lock it was created from.
    fn drop(&mut self)
    {
        self.lock.store(false, Ordering::SeqCst);
    }
}