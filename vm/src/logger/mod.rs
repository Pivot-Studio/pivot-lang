use log::{Record, Level, Metadata,SetLoggerError,LevelFilter};

pub struct SimpleLogger;


impl SimpleLogger{
    pub fn init(&'static self) -> Result<(), SetLoggerError> {
       log::set_boxed_logger(Box::new(SimpleLogger))
            .map(|()| log::set_max_level(LevelFilter::Info))
    }
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
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
        let _ = SimpleLogger{}.init();
        info!("test");
        error!("test");
        warn!("test");

    }
}

