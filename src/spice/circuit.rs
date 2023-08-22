use crate::spice::{Component, ComponentKind, SpiceError};

pub struct Circuit {
    components: Vec<Component>,
}

impl Circuit {
    pub fn new() -> Self {
        Self { components: vec![] }
    }

    pub fn add_component(&mut self, component: Component) -> Result<(), SpiceError> {
        if self.components.contains(&component) {
            return Ok(());
        }
        if let Some(c) = self.components.iter().find(|c| c.name == component.name) {
            return Err(SpiceError::DuplicateComponentName(c.clone()));
        }
        self.components.push(component);
        Ok(())
    }

    pub fn simulate(&self, time: f64, time_step: f64) {
        let num_steps = (time / time_step).ceil() as usize;

        // Initialize node voltages and currents
        let mut node_voltages = vec![0.0; self.max_node_number() + 1];
        let mut node_currents = vec![0.0; self.max_node_number() + 1];

        for _ in 0..num_steps {
            // Apply circuit analysis here (e.g., loop over components)
            for component in &self.components {
                match &component.kind {
                    ComponentKind::Resistor(resistance) => {
                        let voltage_drop =
                            node_voltages[component.nodes[0]] - node_voltages[component.nodes[1]];
                        let current = voltage_drop / resistance;

                        node_currents[component.nodes[0]] -= current;
                        node_currents[component.nodes[1]] += current;
                    }
                    // Implement other component types here
                    _ => {}
                }
            }

            // Update node voltages for next time step
            for (node, current) in node_currents.iter().enumerate() {
                node_voltages[node] += current * time_step;
            }
        }
    }

    fn max_node_number(&self) -> usize {
        self.components
            .iter()
            .flat_map(|c| c.nodes.iter())
            .cloned()
            .max()
            .unwrap_or(0)
    }
}
