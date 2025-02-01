#[macro_export]
macro_rules! set_flag {
    ($flag:expr) => {
        unsafe {
            use crate::utils::flags::*;
            FLAGS.0 |= $flag;
        }
    };
}

#[macro_export]
macro_rules! set_flag_str {
    ($str:expr) => {
        unsafe {
            use crate::utils::flags::*;
            match $str {
                "debug" => FLAGS.0 |= FLAG_DEBUG,
                "no colour" => FLAGS.0 |= FLAG_NO_COLOR,
                _ => panic!("Invalid flag: {}", $str),
            }
        }
    };
}

#[macro_export]
macro_rules! is_flag_set {
    ($flag:expr) => {
        unsafe {
            use crate::utils::flags::*;
            FLAGS.0 & $flag == $flag
        }
    };
}

#[macro_export]
macro_rules! is_flag_set_str {
    ($str:expr) => {
        unsafe {
            use crate::utils::flags::*;
            match $str {
                "debug" => FLAGS.0 & FLAG_DEBUG == FLAG_DEBUG,
                "no colour" => FLAGS.0 & FLAG_NO_COLOR == FLAG_NO_COLOR,
                _ => panic!("Invalid flag: {}", $str),
            }
        }
    };
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if crate::is_flag_set_str!("debug") {
            eprintln!(concat!("[DEBUG] ", $($arg)*));
        }
    };
}