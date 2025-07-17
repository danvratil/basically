use basically::Basically;
use std::fs::read_to_string;
use std::cell::RefCell;
use std::rc::Rc;

use yare::parameterized;
use assert2::assert;

#[parameterized(
    basic_print = { "basic_print" },
    basic_arithmetic = { "basic_arithmetic" },
    basic_variables = { "basic_variables" },
)]
fn test_run_program(name: &str) {
    let input = read_to_string(format!("tests/test-resources/{}.bas", name)).unwrap();
    let expected_output = read_to_string(format!("tests/test-resources/{}.txt", name)).unwrap();

    let output = Rc::new(RefCell::new(String::new()));
    let output_clone = Rc::clone(&output);
    let basically = Basically::new_with_output_handler(move |s: String| {
        output_clone.borrow_mut().push_str(&s);
        output_clone.borrow_mut().push_str("\n");
    });
    basically.run(&input).expect("Failed to run program");

    self::assert!(output.borrow().to_string() == expected_output);
}