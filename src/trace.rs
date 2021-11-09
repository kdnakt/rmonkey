static mut LEVEL: usize = 0;

fn print(msg: (String, usize)) {
    println!("{}{}", "\t".repeat(msg.1 - 1), msg.0);
}

pub fn trace(msg: &str) -> (&str, usize) {
    unsafe {
        LEVEL = LEVEL + 1;
        print((format!("BEGIN {}", msg), LEVEL));
        (msg, LEVEL)
    }
}

pub fn untrace(msg: (&str, usize)) {
    print((format!("END {}", msg.0), msg.1));
    unsafe {
        LEVEL = LEVEL - 1;
    }
}
