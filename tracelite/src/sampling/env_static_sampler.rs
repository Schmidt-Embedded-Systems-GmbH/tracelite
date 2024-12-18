use super::StaticSampler;
use crate::Severity;

pub struct EnvStaticSampler {
    default_min_severity: Severity,
    specific_min_severities: Vec<(String, Option<Severity>)>,
}

impl StaticSampler for EnvStaticSampler {
    fn is_enabled(&self, target: Option<&str>, severity: Option<Severity>) -> bool {
        // TODO properly support target=some and severity=none
        if let (Some(target), Some(severity)) = (target, severity) {
            for (path, min_severity) in &self.specific_min_severities {
                if target == path || (path.ends_with("::") && target.starts_with(path)) {
                    // if OFF or less-than min_severity for target
                    println!("is_enabled {}",*min_severity != None && *min_severity <= Some(severity));
                    return *min_severity != None && *min_severity <= Some(severity);
                }
            }
        }
                    println!("is_enabled {}",Some(self.default_min_severity) <= severity );
        Some(self.default_min_severity) <= severity
    }
}

impl EnvStaticSampler {
    pub fn from_env(env_var: &str) -> Self {
        Self::new(std::env::var(env_var).ok())
    }
    pub fn new(rust_trace_env: Option<String>) -> Self {
        let mut default_min_severity = Severity::Trace;
        let mut specific_min_severities = vec![];

        let rust_trace_env = rust_trace_env.unwrap_or(String::new());

        for entry in rust_trace_env.trim().split(",").map(|e| e.trim()).filter(|e| !e.is_empty()) {
            if let Some(sev) = entry.parse::<Severity>().ok() {
                default_min_severity = sev;
            }
            
            let Some((path, sev_text)) = entry.split_once("=") else {
                eprintln!("[ERROR] tracelite: malformed RUST_TRACE entry: field does not follow format: {entry}");
                continue
            };

            let sev = if sev_text.eq_ignore_ascii_case("off") {
                None
            } else {
                let Ok(sev) = sev_text.parse::<Severity>() else {
                    eprintln!("[ERROR] tracelite: malformed RUST_TRACE entry; invalid severity: {sev_text}");
                    continue
                };
                Some(sev)
            };

            specific_min_severities.push((path.to_owned(), sev));
            specific_min_severities.push((format!("{path}::"), sev));
        }

        // NOTE sorts in reverse by path
        specific_min_severities.sort_by(|(p1, _), (p2, _)| p2.cmp(p1));

        Self{ default_min_severity, specific_min_severities }
    }
}