//! STG machine

use std::cell::RefCell;
use std::rc::Rc;

use crate::common::sourcemap::Smid;

use self::{
    env::{EnvFrame, SynClosure},
    env_builder::EnvBuilder,
    vm::Machine,
};

use super::{
    emit::Emitter,
    error::ExecutionError,
    memory::{
        alloc::ScopedAllocator,
        loader::{load, load_lambdavec},
        mutator::Mutator,
        symbol::SymbolPool,
        syntax::RefPtr,
    },
    stg::{runtime::Runtime, syntax::StgSyn, StgSettings},
};

pub mod cont;
pub mod env;
pub mod env_builder;
pub mod intrinsic;
pub mod metrics;
pub mod vm;

pub struct Initialiser<'a> {
    syntax: Rc<StgSyn>,
    runtime: &'a dyn Runtime,
    /// Symbol pool populated during loading, extracted afterwards
    pool: &'a RefCell<SymbolPool>,
}

impl Mutator for Initialiser<'_> {
    type Input = ();
    type Output = (RefPtr<EnvFrame>, RefPtr<EnvFrame>, SynClosure);

    fn run(
        &self,
        view: &super::memory::mutator::MutatorHeapView,
        _input: Self::Input,
    ) -> Result<Self::Output, ExecutionError> {
        let root_env = view.alloc(EnvFrame::default())?.as_ptr();
        let mut pool = self.pool.borrow_mut();

        // create an env of globals
        let globals = view.from_let(
            load_lambdavec(view, &mut pool, self.runtime.globals().as_slice())
                .unwrap()
                .as_slice(),
            root_env,
            Smid::default(),
        );

        let closure = SynClosure::new(
            load(view, &mut pool, self.syntax.clone()).unwrap(),
            root_env,
        );
        Ok((root_env, globals?, closure))
    }
}

/// Create a standard machine, using the specified runtime and load
/// the STG supplied.
///
///
pub fn standard_machine<'a>(
    settings: &'a StgSettings,
    syntax: Rc<StgSyn>,
    emitter: Box<dyn Emitter + 'a>,
    runtime: &'a dyn Runtime,
) -> Result<Machine<'a>, ExecutionError> {
    let mut machine = Machine::new(
        emitter,
        settings.trace_steps,
        settings.heap_limit_mib,
        settings.heap_dump_at_gc,
    );
    let pool = RefCell::new(SymbolPool::new());
    let (root_env, globals, closure) = machine.mutate(
        Initialiser {
            syntax,
            runtime,
            pool: &pool,
        },
        (),
    )?;
    machine.set_symbol_pool(pool.into_inner());

    machine.initialise(root_env, globals, closure, runtime.intrinsics())?;
    Ok(machine)
}
