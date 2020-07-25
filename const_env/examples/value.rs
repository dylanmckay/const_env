extern crate const_env__value as const_env;

use const_env::value_from_env;


const SMOKE_U32: u32 = value_from_env!("SMOKE_U32": u32);

fn main() {
    assert_eq!(321, SMOKE_U32);
}
