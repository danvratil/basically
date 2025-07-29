use basically::BasicallyBuilder;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::fs::read_to_string;
use std::path::Path;
use std::rc::Rc;

use assert2::assert;
use yare::parameterized;

#[parameterized(
    basic_print = { "basic_print" },
    basic_arithmetic = { "basic_arithmetic" },
    basic_variables = { "basic_variables" },
    basic_string = { "basic_string" },
    basic_input = { "basic_input" },
    // FIXME: Currently fails to due issues with Float precision
    //typed_variables = { "typed_variables" },
    basic_comments = { "basic_comments" },
    basic_array = { "basic_array" },
    basic_logic = { "basic_logic" },
    basic_if_simple = { "basic_if_simple" },
    basic_if = { "basic_if" },
    basic_for = { "basic_for" },
)]
fn test_run_program(name: &str) {
    let input = read_to_string(format!("tests/test-resources/{name}.bas")).unwrap();
    let expected_output = read_to_string(format!("tests/test-resources/{name}.txt")).unwrap();
    let mut expected_input = if Path::new(&format!("tests/test-resources/{name}.in")).exists() {
        Some(
            read_to_string(format!("tests/test-resources/{name}.in"))
                .unwrap()
                .lines()
                .map(|l| l.to_string())
                .collect::<VecDeque<_>>(),
        )
    } else {
        None
    };

    let output = Rc::new(RefCell::new(String::new()));
    let output_clone = Rc::clone(&output);
    let basically = BasicallyBuilder::default()
        .output_handler(move |s: String| {
            output_clone.borrow_mut().push_str(&s);
            output_clone.borrow_mut().push('\n');
        })
        .input_handler(move || match expected_input {
            Some(ref mut input) => input.pop_front().unwrap_or_default(),
            None => {
                panic!("No input expected in current test")
            }
        })
        .build();
    basically.run(&input).expect("Failed to run program");

    self::assert!(output.borrow().to_string() == expected_output);
}
