use std::fmt::Debug;
use std::str::FromStr;

pub fn parse_input<T>(input: &str) -> Vec<T>
    where T: FromStr,
          <T as FromStr>::Err: Debug
{
    input.lines().map(|s| s.parse().unwrap()).collect()
}