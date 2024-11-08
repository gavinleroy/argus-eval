use super::system::{IntoSystem, Resource};

pub struct App {}

impl App {
    pub fn new() -> Self {
        App {}
    }

    pub fn insert_resource(self, _: impl Resource) -> Self {
        self
    }

    pub fn add_system<M>(self, _: impl IntoSystem<M>) -> Self {
        self
    }

    pub fn run(self) {}
}
