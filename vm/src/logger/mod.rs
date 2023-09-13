use log::{LevelFilter, Metadata, Record};
use std::{
    env::{self},
    str::FromStr,
    time::{SystemTime, UNIX_EPOCH},
};

/// SimpleLogger is a logger with almost nothing dependency exclude std and log crates
pub struct SimpleLogger {
    log_limit: LevelFilter,
}

impl SimpleLogger {
    /// This will init logger with the log environment variable "log_level_name".
    /// if variable not exists or can't be parsed by LevelFilter::from_str,
    /// this function will set default_level as Error.
    pub fn init_from_env(log_level_name: &str) {
        Self::init_from_env_default(log_level_name, LevelFilter::Error);
    }

    /// This will init logger with the log environment variable "log_level_name".
    /// if variable not exists or can't be parsed by LevelFilter::from_str,
    /// this function will set default_level as given.
    pub fn init_from_env_default(log_level_name: &str, default_level: LevelFilter) {
        Self::init_default_max_level(SimpleLogger {
            log_limit: match LevelFilter::from_str(Self::get_env_log_level(log_level_name).as_str())
            {
                Ok(level_filter) => level_filter,
                Err(_) => default_level,
            },
        });
    }

    /// initialize log with the SimpleLogger
    /// the max_level in log will never make effects
    /// since the logger implement enabled function by *LevelFilter*
    fn init_default_max_level(logger: Self) {
        if log::set_boxed_logger(Box::new(logger)).is_ok() {
            // do nothing because succeeded
            // do nothing because logger is set before
        };
        log::set_max_level(LevelFilter::Trace);
    }

    /// get the environment variable named *log_level_name*
    /// return a default "null" string to lead a parse error
    fn get_env_log_level(log_level_name: &str) -> String {
        let env_result = env::var(log_level_name);
        match env_result {
            Ok(log_level) => log_level,
            Err(_) => {
                //"null" will definitely lead to parse error
                "null".to_string()
            }
        }
    }
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= self.log_limit
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            // 颜色 is_terminal去做
            let date = DateTime::get_now_date();
            eprintln!(
                "[{:0>4}-{:0>2}-{:0>2} {:0>2}:{:0>2}:{:0>2}] {} - {}",
                date.year,
                date.month,
                date.day,
                date.hour,
                date.minute,
                date.second,
                record.level(),
                record.args()
            );
        }
    }

    fn flush(&self) {}
}

/// DateTime contains 6 fields to represents common date format
/// provide get_now_date() to quickly get current date.
pub struct DateTime {
    year: u64,
    month: u64,
    day: u64,
    hour: u64,
    minute: u64,
    second: u64,
}
impl DateTime {
    pub fn get_now_date() -> Self {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(secs_since_unix) => {
                let seconds = secs_since_unix.as_secs();
                Self::from_days_since_unix(seconds)
            }
            Err(_) => Self::new_unix_date_utc0(),
        }
    }

    /// used to calculate the date using given *seconds* param
    /// the seconds is given to tell the duration since std::time::UNIX_EPOCH
    pub fn from_days_since_unix(seconds: u64) -> Self {
        const SECONDS_PER_DAY: u64 = 86400;
        let mut seconds = Self::apply_utc(seconds);
        let mut days = seconds / SECONDS_PER_DAY;
        seconds %= SECONDS_PER_DAY;

        let mut result_date = Self::new_unix_date_utc0();

        while days > 0 {
            let next_year_days = Self::get_days_this_year(&result_date);
            let next_month_days = Self::get_days_this_month(&result_date);
            if days >= next_year_days {
                result_date.year += 1;
                days -= next_year_days;
            } else if days >= next_month_days {
                result_date.month += 1;
                days -= next_month_days;
            } else {
                result_date.day += days;
                days = 0;
            }
        }

        Self::fill_within_day(&mut result_date, seconds);

        result_date
    }

    /// return a standard date at std::time::UNIX_EPOCH
    fn new_unix_date_utc0() -> Self {
        DateTime {
            year: 1970,
            month: 1,
            day: 1,
            hour: 0,
            minute: 0,
            second: 0,
        }
    }

    ///  set default utc to utc+8
    ///  remain this function to apply to multi-TimeZone
    fn get_utc() -> u64 {
        8
    }

    /// using the seconds remain to update date in a day
    fn fill_within_day(date: &mut DateTime, mut seconds: u64) {
        const SECONDS_PER_HOUR: u64 = 3600;
        const SECONDS_PER_MINUTE: u64 = 60;

        let hours = seconds / SECONDS_PER_HOUR;
        date.hour += hours;
        seconds %= SECONDS_PER_HOUR;

        let minutes = seconds / SECONDS_PER_MINUTE;
        date.minute += minutes;
        seconds %= SECONDS_PER_MINUTE;

        date.second = seconds;
    }

    /// judge if the year is a leap year
    pub fn is_leap_year(year: u64) -> bool {
        if year % 400 == 0 {
            // while every four hundreds leaps
            true
        } else if year % 100 == 0 {
            // but hundreds not
            false
        } else {
            // every four year leaps
            year % 4 == 0
        }
    }

    /// get the total days in this year
    fn get_days_this_year(date: &DateTime) -> u64 {
        if Self::is_leap_year(date.year) {
            366
        } else {
            365
        }
    }

    /// get the total days in this month
    fn get_days_this_month(date: &DateTime) -> u64 {
        match date.month {
            2 => {
                if Self::is_leap_year(date.year) {
                    29
                } else {
                    28
                }
            }
            1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
            4 | 6 | 9 | 11 => 30,
            _ => 0,
        }
    }

    /// apply the local system time with utc time zone
    /// take the original system time seconds and
    fn apply_utc(secs: u64) -> u64 {
        const SECONDS_PER_HOUR: u64 = 3600;
        Self::get_utc() * SECONDS_PER_HOUR + secs
    }
}

#[cfg(test)]
pub mod tests {
    use crate::logger::{DateTime, SimpleLogger};
    use log::{self, error, info, warn};
    #[test]
    fn test_logger() {
        SimpleLogger::init_from_env_default("GC_LOG", log::LevelFilter::Error);
        info!("test");
        error!("test");
        warn!("test");
    }

    #[test]
    fn test_logger_multi() {
        SimpleLogger::init_from_env_default("GC_LOG", log::LevelFilter::Error);
        SimpleLogger::init_from_env_default("GC_LOG", log::LevelFilter::Error);
        SimpleLogger::init_from_env_default("GC_LOG", log::LevelFilter::Error);
        error!("hello {}", "moonold");
    }

    #[test]
    fn test_from_days() {
        let secs = 1680016716;
        let date = DateTime::from_days_since_unix(secs);
        assert_eq!(date.year, 2023);
        assert_eq!(date.month, 3);
        assert_eq!(date.day, 28);
        assert_eq!(date.hour, 23);
        assert_eq!(date.minute, 18);
        assert_eq!(date.second, 36);
    }
}
