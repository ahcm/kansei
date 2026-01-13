use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fs;
use std::path::Path;
use std::rc::Rc;
use wasmi::{Engine, ExternType, Func, FuncType, Instance, Linker, Memory, Module, Store};

#[derive(Debug)]
pub struct WasmModule {
    pub store: Store<()>,
    pub instance: Instance,
    pub memory: Option<Memory>,
    pub alloc: Option<Func>,
    pub dealloc: Option<Func>,
    pub wbindgen_malloc: Option<Func>,
    pub wbindgen_free: Option<Func>,
    pub wbindgen_add_to_stack_pointer: Option<Func>,
    pub functions: FxHashMap<Rc<String>, Func>,
    pub func_types: FxHashMap<Rc<String>, FuncType>,
}

#[derive(Debug, Clone)]
pub struct WasmFunction {
    pub name: Rc<String>,
    pub module: Rc<RefCell<WasmModule>>,
}

impl WasmModule {
    pub fn load(path: &Path) -> Result<Rc<RefCell<WasmModule>>, String> {
        let engine = Engine::default();
        let bytes = fs::read(path).map_err(|e| format!("Failed to read wasm module: {}", e))?;
        let module = Module::new(&engine, &mut &bytes[..])
            .map_err(|e| format!("Failed to load wasm module: {}", e))?;
        let mut store = Store::new(&engine, ());
        let mut linker = Linker::new(&engine);

        for import in module.imports() {
            if let ExternType::Func(func_type) = import.ty() {
                let func = Func::new(&mut store, func_type.clone(), |_caller, _params, _results| {
                    Ok(())
                });
                linker
                    .define(import.module(), import.name(), func)
                    .map_err(|e| format!("Failed to define import {}::{}: {}", import.module(), import.name(), e))?;
            }
        }

        let instance = linker
            .instantiate(&mut store, &module)
            .map_err(|e| format!("Failed to instantiate wasm module: {}", e))?
            .start(&mut store)
            .map_err(|e| format!("Failed to start wasm module: {}", e))?;

        let memory = instance
            .get_export(&store, "memory")
            .and_then(|e| e.into_memory());

        let alloc = instance
            .get_export(&store, "alloc")
            .and_then(|e| e.into_func());

        let dealloc = instance
            .get_export(&store, "dealloc")
            .and_then(|e| e.into_func());

        let wbindgen_malloc = instance
            .get_export(&store, "__wbindgen_malloc")
            .and_then(|e| e.into_func());

        let wbindgen_free = instance
            .get_export(&store, "__wbindgen_free")
            .and_then(|e| e.into_func());

        let wbindgen_add_to_stack_pointer = instance
            .get_export(&store, "__wbindgen_add_to_stack_pointer")
            .and_then(|e| e.into_func());

        let mut functions = FxHashMap::default();
        let mut func_types = FxHashMap::default();
        for export in module.exports() {
            let name: &str = export.name();
            if let ExternType::Func(func_type) = export.ty() {
                if let Some(func) = instance
                    .get_export(&store, name)
                    .and_then(|e| e.into_func())
                {
                    let name_rc = Rc::new(name.to_string());
                    functions.insert(name_rc.clone(), func);
                    func_types.insert(name_rc, func_type.clone());
                }
            }
        }

        Ok(Rc::new(RefCell::new(WasmModule {
            store,
            instance,
            memory,
            alloc,
            dealloc,
            wbindgen_malloc,
            wbindgen_free,
            wbindgen_add_to_stack_pointer,
            functions,
            func_types,
        })))
    }
}
