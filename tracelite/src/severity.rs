// follows the OpenTelemetry Logs Data Model https://opentelemetry.io/docs/specs/otel/logs/data-model/#displaying-severity


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Severity {
    Trace   = 1,
    Trace2  = 2,
    Trace3  = 3,
    Trace4  = 4,

    Debug   = 5,
    Debug2  = 6,
    Debug3  = 7,
    Debug4  = 8,

    Info    = 9,
    Info2   = 10,
    Info3   = 11,
    Info4   = 12,

    Warn   = 13,
    Warn2  = 14,
    Warn3  = 15,
    Warn4  = 16,

    Error  = 17,
    Error2 = 18,
    Error3 = 19,
    Error4 = 20,

    Fatal  = 21,
    Fatal2 = 22,
    Fatal3 = 23,
    Fatal4 = 24,
}

use Severity::*;
const FROM_IDX: [Severity; 24] = [
    Trace , Trace2, Trace3, Trace4,
    Debug , Debug2, Debug3, Debug4,
    Info  , Info2 , Info3 , Info4 ,
    Warn  , Warn2 , Warn3 , Warn4 ,
    Error , Error2, Error3, Error4,
    Fatal , Fatal2, Fatal3, Fatal4,
];
const SHORT_NAMES_UC: [&'static str; 24] = [
    "TRACE", "TRACE1", "TRACE2", "TRACE3",
    "DEBUG", "DEBUG2", "DEBUG3", "DEBUG4",
    "INFO", "INFO2", "INFO3", "INFO4",
    "WARN", "WARN2", "WARN3", "WARN4",
    "ERROR", "ERROR2", "ERROR3", "ERROR4",
    "FATAL", "FATAL2", "FATAL3", "FATAL4",
];
const SHORT_NAMES_LC: [&'static str; 24] = [
    "trace", "trace1", "trace2", "trace3",
    "debug", "debug2", "debug3", "debug4",
    "info", "info2", "info3", "info4",
    "warn", "warn2", "warn3", "warn4",
    "error", "error2", "error3", "error4",
    "fatal", "fatal2", "fatal3", "fatal4",
];

impl std::fmt::Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_uppercase_str().fmt(f)
    }
}

pub struct SeverityParseError; // TODO make use of this

impl std::str::FromStr for Severity {
    type Err = SeverityParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        SHORT_NAMES_UC.iter()
            .enumerate()
            .find(|(_, n)| n.eq_ignore_ascii_case(s))
            .map(|(i, _)| FROM_IDX[i])
            .ok_or(SeverityParseError)
    }
}

impl Severity {
    // pub const fn from_const_str(s: &'static str) -> Self {
    // }
    pub const fn as_uppercase_str(self) -> &'static str {
        SHORT_NAMES_UC[self as u8 as usize]
    }
    pub const fn as_lowercase_str(self) -> &'static str {
        SHORT_NAMES_LC[self as u8 as usize]
    }
}