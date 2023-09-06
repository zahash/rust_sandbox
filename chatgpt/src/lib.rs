use rand::Rng;

fn generate_password(
    length: usize,
    use_lowercase: bool,
    use_uppercase: bool,
    use_digits: bool,
    use_special: bool,
) -> String {
    let mut password_chars = Vec::new();

    let lowercase_chars = "abcdefghijklmnopqrstuvwxyz";
    let uppercase_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    let digit_chars = "0123456789";
    let special_chars = "!@#$%^&*()_-+=<>?";

    let mut rng = rand::thread_rng();

    if use_lowercase {
        password_chars.extend(lowercase_chars.chars());
    }
    if use_uppercase {
        password_chars.extend(uppercase_chars.chars());
    }
    if use_digits {
        password_chars.extend(digit_chars.chars());
    }
    if use_special {
        password_chars.extend(special_chars.chars());
    }

    if password_chars.is_empty() {
        return String::from("No characters selected for password generation.");
    }

    let password: String = (0..length)
        .map(|_| password_chars[rng.gen_range(0..password_chars.len())])
        .collect();

    password
}

pub fn run() {
    println!("Random Password Generator");

    println!("Enter password length:");
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line");
    let length: usize = match input.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            println!("Invalid input. Please enter a number.");
            return;
        }
    };

    println!("Include lowercase letters? (y/n)");
    let include_lowercase = read_yes_no();

    println!("Include uppercase letters? (y/n)");
    let include_uppercase = read_yes_no();

    println!("Include digits? (y/n)");
    let include_digits = read_yes_no();

    println!("Include special characters? (y/n)");
    let include_special = read_yes_no();

    let password = generate_password(
        length,
        include_lowercase,
        include_uppercase,
        include_digits,
        include_special,
    );
    println!("Generated Password: {}", password);
}

fn read_yes_no() -> bool {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line");
    let cleaned_input = input.trim().to_lowercase();
    cleaned_input == "y" || cleaned_input == "yes"
}
