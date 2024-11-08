mod bevy;

use bevy::prelude::*;

// #[derive(Resource)]
impl Resource for Timer {}
struct Timer(usize);

fn run_timer(mut timer: Timer) {
    timer.0 += 1;
}

fn main() {
    App::new()
        .insert_resource(Timer(0))
        .add_system(run_timer)
        .run();
}
