// Std
use std::{
    env,
};

static mut LEVEL: usize = 0;
static mut INITIALIZED: bool = false;
static mut IS_TRACE: bool = false;

unsafe fn init() {
    let args: Vec<String> = env::args().collect();
    for arg in args {
        if arg == "--trace" || arg == "-t" {
            IS_TRACE = true;
            break;
        }
    }
    INITIALIZED = true;
}

unsafe fn print(msg: (String, usize)) {
    if !INITIALIZED {
        init();
    }
    println!("{}{}", "\t".repeat(msg.1 - 1), msg.0);
}

pub fn trace(msg: &str) -> (&str, usize) {
    unsafe {
        if IS_TRACE {
            LEVEL = LEVEL + 1;
            print((format!("BEGIN {}", msg), LEVEL));
        }
        (msg, LEVEL)
    }
}

pub fn untrace(msg: (&str, usize)) {
    unsafe {
        if IS_TRACE {
            print((format!("END {}", msg.0), msg.1));
            LEVEL = LEVEL - 1;
        }
    }
}
