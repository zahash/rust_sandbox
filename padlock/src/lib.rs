mod lex;
mod parse;

pub use lex::*;

// launch prompt. ask for master password

// add name='some name with spaces' user=zahash pass=asdf url='https://asdf.com'

// prev stores name of last added/set name

// set 'some name with spaces' user=zahash.z
// set prev user=zahash.z

// show name='some name with spaces' or (name contains asdf and url matches '.+asdf.+')
// show 'some name'
// show all
// show prev

// del 'some name'
// del prev

// history 'some name'
// history prev
