use crate::Token;

// <cmd> ::= add <value> {<assign>}*
//         | set <value> {<assign>}*
//         | delete <value>
//         | show <logical>
//         | show all
//         | history <value>

// <assign> ::= <attr> = <value>
// <attr> ::= user | pass | url

// <logical> ::= <or>
// <or> ::= <and> | <or> or <and>
// <and> ::= <filter> | <and> and <filter>
// <filter> ::= <contains> | <matches> | <cmp>
// <contains> ::= <attr> contains <value>
// <matches> ::= <attr> matches <value>
// <cmp> ::= <attr> = <value>


// add 'some name with spaces' user=zahash pass=asdf url='https://asdf.com'
// set 'some name with spaces' user=zahash.z
// del 'some name'
// show name='some name with spaces' or (name contains asdf and url matches '.+asdf.+')
// show 'some name'
// show all

// history 'some name'

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(usize, &'static str),
    ExpectedIdent(usize),
    Expected(Token<'static>, usize),
    ExpectedOneOf(Vec<Token<'static>>, usize),
    InvalidDeclarationSpecifiers(usize, String),
}
