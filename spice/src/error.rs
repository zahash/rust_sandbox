use crate::Component;

pub enum SpiceError {
    DuplicateComponentName(Component),
}
