use log::{Record, Metadata,LevelFilter};
use std::{env::{self}, str::FromStr};

pub struct SimpleLogger{
    log_limit: LevelFilter
}


impl SimpleLogger{
    ///
    /// This will init logger with the log environment variable "log_level_name".
    /// if variable not exists or can't be parsed by LevelFilter::from_str,
    /// this function will set default_level as Error.
    pub fn init_from_env(log_level_name: &str){
        Self::init_from_env_default(log_level_name,LevelFilter::Error);
    }

    ///
    /// This will init logger with the log environment variable "log_level_name".
    /// if variable not exists or can't be parsed by LevelFilter::from_str,
    /// this function will set default_level as given.
    pub fn init_from_env_default(log_level_name: &str, default_level : LevelFilter){
        Self::init_default_max_level(SimpleLogger{
            log_limit:  match LevelFilter::from_str( Self::get_env_log_level(log_level_name).as_str()){
                Ok(level_filter)=> level_filter,
                Err(_) => default_level
            }
        });
    }

    // fn init(logger:Self) {
    //     Self::init_default_max_level(logger,LevelFilter::Trace);
    // }


    fn init_default_max_level(logger: Self) {
        match log::set_boxed_logger(Box::new(logger)){
            Ok(_)=>{
                // do nothing because succeeded
            }
            Err(_)=>{
                //do nothing because logger is set before
            }
        }
        log::set_max_level(LevelFilter::Trace);
    }

    fn get_env_log_level(log_level_name: &str)-> String{
        let env_result = env::var(log_level_name);
        match env_result{
            Ok(log_level)=>{
                log_level
            }
            Err(_)=>{

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
            println!("{} - {}", record.level(), record.args());
        
        }
    }

    fn flush(&self) {}
}

#[cfg(test)]
pub mod tests{
    use crate::logger::SimpleLogger;
    use log::{self, info, error, warn};
    #[test]
    fn test_logger(){
        SimpleLogger::init_from_env_default( "GC_LOG",log::LevelFilter::Error);
        info!("test");
        error!("test");
        warn!("test");
    }

    #[test]
    fn test_logger_multi(){
        SimpleLogger::init_from_env_default( "GC_LOG",log::LevelFilter::Error);
        SimpleLogger::init_from_env_default( "GC_LOG",log::LevelFilter::Error);
        SimpleLogger::init_from_env_default( "GC_LOG",log::LevelFilter::Error);
        error!("hello {}","moonold");

    }

    #[test]
    fn test_logger_in_log(){
        let a = log::logger();
        let _b = log::logger();
        let _c = log::logger();
        let _d = log::logger();
        let _e = log::logger();
        a.flush();
    }
}

