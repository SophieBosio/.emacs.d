extern crate emacs;
extern crate fuzzy_matcher;

use emacs::{ Env, Result };

pub mod dynmod;

// Module Defintion
emacs::plugin_is_GPL_compatible!();

// Empty method to satisify emacs module
#[emacs::module(mod_in_name = false)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}
