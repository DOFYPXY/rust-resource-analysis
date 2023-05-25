fn tick() {}

fn f(x: u32) -> u32 {
    0
}

fn pow(x: u32, n: u32) -> u32 {
    if n > 0 {
        tick();
        x * pow(x, n - 1)
    } else {
        0
    }
}
