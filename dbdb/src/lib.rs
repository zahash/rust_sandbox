use rustyline::error::ReadlineError;

#[derive(Debug)]
enum DbDbError {
    InSufficientSize(&'static str),
}

#[derive(Debug)]
struct Row {
    id: u32,
    username: String,
    email: String,
}

const USERNAME_MAX_SIZE_BYTES: usize = 30;
const EMAIL_MAX_SIZE_BYTES: usize = 200;
const ROW_SIZE_BYTES: usize = 4 + USERNAME_MAX_SIZE_BYTES + EMAIL_MAX_SIZE_BYTES;
const USERNAME_OFFSET: usize = 4;
const EMAIL_OFFSET: usize = USERNAME_OFFSET + USERNAME_MAX_SIZE_BYTES;

impl TryFrom<Row> for [u8; ROW_SIZE_BYTES] {
    type Error = DbDbError;

    fn try_from(row: Row) -> Result<Self, Self::Error> {
        let id = row.id.to_be_bytes();

        let username = row.username.into_bytes();
        // -1 because we store the length in the first byte
        if username.len() > (USERNAME_MAX_SIZE_BYTES - 1) as usize {
            return Err(DbDbError::InSufficientSize("username"));
        }
        // u8 is enough as long as USERNAME_MAX_SIZE_BYTES < 255
        let username_size_bytes = username.len() as u8;
        let username: [u8; USERNAME_MAX_SIZE_BYTES - 1] = username
            .into_iter()
            .chain(std::iter::repeat(0))
            .take(USERNAME_MAX_SIZE_BYTES - 1)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap();

        // -1 because we store the length in the first byte
        let email = row.email.into_bytes();
        if email.len() > (EMAIL_MAX_SIZE_BYTES - 1) as usize {
            return Err(DbDbError::InSufficientSize("email"));
        }
        // u8 is enough as long as EMAIL_MAX_SIZE_BYTES < 255
        let email_size_bytes = email.len() as u8;
        let email: [u8; EMAIL_MAX_SIZE_BYTES - 1] = email
            .into_iter()
            .chain(std::iter::repeat(0))
            .take(EMAIL_MAX_SIZE_BYTES - 1)
            .collect::<Vec<u8>>()
            .try_into()
            .unwrap();

        // populate this with id, username_size_bytes, username, email_size_bytes and email in that order
        let mut row_bytes = [0; ROW_SIZE_BYTES];
        row_bytes[0..USERNAME_OFFSET].copy_from_slice(&id);
        row_bytes[USERNAME_OFFSET] = username_size_bytes;
        row_bytes[USERNAME_OFFSET + 1..EMAIL_OFFSET].copy_from_slice(&username);
        row_bytes[EMAIL_OFFSET] = email_size_bytes;
        row_bytes[EMAIL_OFFSET + 1..].copy_from_slice(&email);

        Ok(row_bytes)
    }
}

impl TryFrom<[u8; ROW_SIZE_BYTES]> for Row {
    type Error = DbDbError;

    fn try_from(bytes: [u8; ROW_SIZE_BYTES]) -> Result<Self, Self::Error> {
        let id = u32::from_be_bytes(bytes[0..4].try_into().unwrap());

        let username_size = bytes[USERNAME_OFFSET] as usize;
        let username = String::from_utf8_lossy(
            &bytes[USERNAME_OFFSET + 1..USERNAME_OFFSET + 1 + username_size],
        )
        .to_string();

        let email_size = bytes[EMAIL_OFFSET] as usize;
        let email =
            String::from_utf8_lossy(&bytes[EMAIL_OFFSET + 1..EMAIL_OFFSET + 1 + email_size])
                .to_string();

        Ok(Row {
            id,
            username,
            email,
        })
    }
}

pub fn run() -> anyhow::Result<()> {
    let row = Row {
        id: 69,
        username: String::from("zahash"),
        email: String::from("zahash.z@gmail.com"),
    };
    println!("{:?}", row);

    let row_bytes: [u8; ROW_SIZE_BYTES] = row.try_into().unwrap();
    println!("{:?}", row_bytes);

    let decoded_row: Row = row_bytes.try_into().unwrap();
    println!("{:?}", decoded_row);

    let mut editor = rustyline::DefaultEditor::new()?;

    loop {
        match editor.readline("> ").as_deref() {
            Ok(".exit") => break,
            Ok(line) => {
                if !line.is_empty() {
                    editor.add_history_entry(line)?;
                    eprintln!("Unrecognized Command!!");
                }
            }
            Err(ReadlineError::Interrupted) => {
                eprintln!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                eprintln!("CTRL-D");
                break;
            }
            Err(e) => {
                eprintln!("!! Unexpected Error: {:?}", e);
                break;
            }
        }
    }

    Ok(())
}
