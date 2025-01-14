use crate::{tracer::*, Severity};


pub trait RecordException<E: std::error::Error>: Sized {
    #[doc(hidden)]
    fn get_exception(&self) -> Option<&E>;

    #[doc(hidden)]
    fn _record_error_ext<'a>(self,
        name: impl Into<Text<'a>>,
        severity: Option<Severity>,
    ) -> Self {
        let Some(tracer) = globals::tracer().ok() else { return self };
        if !tracer.is_enabled(None, severity) { return self; }

        if let Some(ex) = self.get_exception() {
            EventBuilder::new(name.into(), None, severity)
                .exception(ex)
                .add_to_current_span(tracer, AttributeList(&[]));
        }
        self
    }

    fn record_error_as_debug<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Debug))
    }
    fn record_error_as_info<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Info))
    }
    fn record_error_as_warn<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Warn))
    }
    fn record_error<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Error))
    }
    fn record_error_as_fatal<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Fatal))
    }

    fn record_exception(self) -> Self {
        self._record_error_ext("exception", None)
    }
    fn record_exception_as_trace(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Trace))
    }
    fn record_exception_as_debug(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Debug))
    }
    fn record_exception_as_info(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Info))
    }
    fn record_exception_as_warn(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Warn))
    }
}

impl<E: std::error::Error> RecordException<E> for Option<E> {
    fn get_exception(&self) -> Option<&E> { self.as_ref() }
}

impl<T, E: std::error::Error> RecordException<E> for Result<T, E> {
    fn get_exception(&self) -> Option<&E> { self.as_ref().err() }
}

pub trait RecordExceptionDebugFmt<E: std::fmt::Debug>: Sized {
    #[doc(hidden)]
    fn get_exception(&self) -> Option<&E>;

    #[doc(hidden)]
    fn _record_error_ext<'a>(self,
        name: impl Into<Text<'a>>,
        severity: Option<Severity>,
    ) -> Self {
        if let Some(ex) = self.get_exception() {
            let Some(tracer) = globals::tracer().ok() else { return self };
            if !tracer.is_enabled(None, severity) { return self; }

            EventBuilder::new(name.into(), None, severity)
                .exception_dbgfmt(ex)
                .add_to_current_span(tracer, AttributeList(&[]));
        }
        self
    }

    fn record_dbgfmt_error_as_debug<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Debug))
    }
    fn record_dbgfmt_error_as_info<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Info))
    }
    fn record_dbgfmt_error_as_warn<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Warn))
    }
    fn record_dbgfmt_error<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Error))
    }
    fn record_dbgfmt_error_as_fatal<'a>(self, name: impl Into<Text<'a>>) -> Self {
        self._record_error_ext(name, Some(Severity::Fatal))
    }

    fn record_dbgfmt_exception(self) -> Self {
        self._record_error_ext("exception", None)
    }
    fn record_dbgfmt_exception_as_trace(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Trace))
    }
    fn record_dbgfmt_exception_as_debug(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Debug))
    }
    fn record_dbgfmt_exception_as_info(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Info))
    }
    fn record_dbgfmt_exception_as_warn(self) -> Self {
        self._record_error_ext("exception", Some(Severity::Warn))
    }
}

impl<E: std::fmt::Debug> RecordExceptionDebugFmt<E> for Option<E> {
    fn get_exception(&self) -> Option<&E> { self.as_ref() }
}

impl<T, E: std::fmt::Debug> RecordExceptionDebugFmt<E> for Result<T, E> {
    fn get_exception(&self) -> Option<&E> { self.as_ref().err() }
}