macro_rules! static_assert {
    ($e:expr) => {
        const _: u8 = 0 - !{ $e } as u8;
    };
}

pub fn run() {
    static_assert!(10 > 9);
    // static_assert!('a' == 'b');
    // static_assert!('b' == 'c');
}
