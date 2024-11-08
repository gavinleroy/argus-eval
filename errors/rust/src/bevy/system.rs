use bevy_utils::all_tuples;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

pub trait Resource {}

pub trait SystemParam {}

pub trait SystemParamFunction<Marker> {}

pub trait System {}

pub trait IntoSystem<Marker> {}

pub struct IsFunctionSystem;

pub struct ResMut<'a, T> {
    _marker: PhantomData<&'a T>,
}

impl<T: Resource> SystemParam for ResMut<'_, T> {}

impl<T> Deref for ResMut<'_, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unimplemented!()
    }
}

impl<T> DerefMut for ResMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unimplemented!()
    }
}

impl<T: System> IntoSystem<()> for T {}

impl<Marker, F> IntoSystem<(IsFunctionSystem, Marker)> for F where F: SystemParamFunction<Marker> {}

macro_rules! impl_system_param_tuple {
    ($($param: ident),*) => {
        #[allow(non_snake_case)]
        impl<$($param: SystemParam),*> SystemParam for ($($param,)*) {}
    };
}

macro_rules! impl_probe_function {
    ($($param: ident),*) => {
        #[allow(non_snake_case)]
        impl<Func, Out, $($param: SystemParam),*> SystemParamFunction<fn($($param,)*) -> Out> for Func
        where
            Func: FnMut($($param),*) -> Out,
        {
        }
    };
}

all_tuples!(impl_system_param_tuple, 0, 1, P);

all_tuples!(impl_probe_function, 0, 1, F);
