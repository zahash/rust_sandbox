use crate::spice::Component;

pub enum SpiceError {
    DuplicateComponentName(Component),
}
