use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::fs;
use std::path::Path;
use std::rc::Rc;
use wasmi::{
    Engine as WasmiEngine, ExternType as WasmiExternType, Func as WasmiFunc,
    FuncType as WasmiFuncType, Instance as WasmiInstance, Linker as WasmiLinker,
    Memory as WasmiMemory, Module as WasmiModule, Store as WasmiStore, Value as WasmiValue,
};
use wasmi::core::ValueType as WasmiValueType;

#[cfg(feature = "wasmtime")]
use wasmtime::{
    Engine as WasmtimeEngine, ExternType as WasmtimeExternType, Func as WasmtimeFunc,
    FuncType as WasmtimeFuncType, Instance as WasmtimeInstance, Linker as WasmtimeLinker,
    Memory as WasmtimeMemory, Module as WasmtimeModule, Store as WasmtimeStore, Val as WasmtimeVal,
    ValType as WasmtimeValType,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WasmBackend
{
    Wasmi,
    #[cfg(feature = "wasmtime")]
    Wasmtime,
}

impl WasmBackend
{
    #[allow(dead_code)]
    pub fn name(self) -> &'static str
    {
        match self
        {
            WasmBackend::Wasmi => "wasmi",
            #[cfg(feature = "wasmtime")]
            WasmBackend::Wasmtime => "wasmtime",
        }
    }
}

pub fn parse_wasm_backend(name: &str) -> Result<WasmBackend, String>
{
    match name
    {
        "wasmi" => Ok(WasmBackend::Wasmi),
        "wasmtime" =>
        {
            #[cfg(feature = "wasmtime")]
            {
                Ok(WasmBackend::Wasmtime)
            }
            #[cfg(not(feature = "wasmtime"))]
            {
                Err("wasmtime backend not enabled (compile with --features wasmtime)".to_string())
            }
        }
        _ => Err(format!("Unknown wasm backend '{}'", name)),
    }
}

pub fn available_wasm_backends() -> Vec<&'static str>
{
    #[cfg(feature = "wasmtime")]
    {
        vec!["wasmi", "wasmtime"]
    }
    #[cfg(not(feature = "wasmtime"))]
    {
        vec!["wasmi"]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WasmValueType
{
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy)]
pub enum WasmValue
{
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub struct WasmFuncType
{
    pub params: Vec<WasmValueType>,
    pub results: Vec<WasmValueType>,
}

#[derive(Debug, Clone)]
pub enum WasmFuncHandle
{
    Wasmi(WasmiFunc),
    #[cfg(feature = "wasmtime")]
    Wasmtime(WasmtimeFunc),
}

#[derive(Debug)]
pub enum WasmMemoryHandle
{
    Wasmi(WasmiMemory),
    #[cfg(feature = "wasmtime")]
    Wasmtime(WasmtimeMemory),
}

#[derive(Debug)]
struct WasmiBackend
{
    store: WasmiStore<()>,
    instance: WasmiInstance,
}

#[cfg(feature = "wasmtime")]
#[derive(Debug)]
struct WasmtimeBackend
{
    store: WasmtimeStore<()>,
    instance: WasmtimeInstance,
}

#[derive(Debug)]
pub struct WasmModule
{
    backend: WasmBackend,
    wasmi: Option<WasmiBackend>,
    #[cfg(feature = "wasmtime")]
    wasmtime: Option<WasmtimeBackend>,
    pub memory: Option<WasmMemoryHandle>,
    pub alloc: Option<WasmFuncHandle>,
    pub dealloc: Option<WasmFuncHandle>,
    pub wbindgen_malloc: Option<WasmFuncHandle>,
    pub wbindgen_free: Option<WasmFuncHandle>,
    pub wbindgen_add_to_stack_pointer: Option<WasmFuncHandle>,
    pub functions: FxHashMap<Rc<String>, WasmFuncHandle>,
    pub func_types: FxHashMap<Rc<String>, WasmFuncType>,
}

#[derive(Debug, Clone)]
pub struct WasmFunction
{
    pub name: Rc<String>,
    pub module: Rc<RefCell<WasmModule>>,
}

fn map_wasmi_valtype(value: WasmiValueType) -> Result<WasmValueType, String>
{
    match value
    {
        WasmiValueType::I32 => Ok(WasmValueType::I32),
        WasmiValueType::I64 => Ok(WasmValueType::I64),
        WasmiValueType::F32 => Ok(WasmValueType::F32),
        WasmiValueType::F64 => Ok(WasmValueType::F64),
        _ => Err("Unsupported wasm value type".to_string()),
    }
}

#[cfg(feature = "wasmtime")]
fn map_wasmtime_valtype(value: WasmtimeValType) -> Result<WasmValueType, String>
{
    match value
    {
        WasmtimeValType::I32 => Ok(WasmValueType::I32),
        WasmtimeValType::I64 => Ok(WasmValueType::I64),
        WasmtimeValType::F32 => Ok(WasmValueType::F32),
        WasmtimeValType::F64 => Ok(WasmValueType::F64),
        _ => Err("Unsupported wasm value type".to_string()),
    }
}

fn wasmi_value_from(value: WasmValue) -> WasmiValue
{
    match value
    {
        WasmValue::I32(v) => WasmiValue::I32(v),
        WasmValue::I64(v) => WasmiValue::I64(v),
        WasmValue::F32(v) => WasmiValue::F32(v.into()),
        WasmValue::F64(v) => WasmiValue::F64(v.into()),
    }
}

fn wasm_value_from_wasmi(value: WasmiValue) -> Result<WasmValue, String>
{
    match value
    {
        WasmiValue::I32(v) => Ok(WasmValue::I32(v)),
        WasmiValue::I64(v) => Ok(WasmValue::I64(v)),
        WasmiValue::F32(v) => Ok(WasmValue::F32(f32::from(v))),
        WasmiValue::F64(v) => Ok(WasmValue::F64(f64::from(v))),
        _ => Err("Unsupported wasm return type".to_string()),
    }
}

#[cfg(feature = "wasmtime")]
fn wasmtime_value_from(value: WasmValue) -> WasmtimeVal
{
    match value
    {
        WasmValue::I32(v) => WasmtimeVal::I32(v),
        WasmValue::I64(v) => WasmtimeVal::I64(v),
        WasmValue::F32(v) => WasmtimeVal::F32(v.to_bits()),
        WasmValue::F64(v) => WasmtimeVal::F64(v.to_bits()),
    }
}

#[cfg(feature = "wasmtime")]
fn wasm_value_from_wasmtime(value: WasmtimeVal) -> Result<WasmValue, String>
{
    match value
    {
        WasmtimeVal::I32(v) => Ok(WasmValue::I32(v)),
        WasmtimeVal::I64(v) => Ok(WasmValue::I64(v)),
        WasmtimeVal::F32(v) => Ok(WasmValue::F32(f32::from_bits(v))),
        WasmtimeVal::F64(v) => Ok(WasmValue::F64(f64::from_bits(v))),
        _ => Err("Unsupported wasm return type".to_string()),
    }
}

impl WasmFuncType
{
    fn from_wasmi(func_type: &WasmiFuncType) -> Result<Self, String>
    {
        let mut params = Vec::new();
        for val in func_type.params()
        {
            params.push(map_wasmi_valtype(*val)?);
        }
        let mut results = Vec::new();
        for val in func_type.results()
        {
            results.push(map_wasmi_valtype(*val)?);
        }
        Ok(Self { params, results })
    }

    #[cfg(feature = "wasmtime")]
    fn from_wasmtime(func_type: &WasmtimeFuncType) -> Result<Self, String>
    {
        let mut params = Vec::new();
        for val in func_type.params()
        {
            params.push(map_wasmtime_valtype(val)?);
        }
        let mut results = Vec::new();
        for val in func_type.results()
        {
            results.push(map_wasmtime_valtype(val)?);
        }
        Ok(Self { params, results })
    }
}

impl WasmModule
{
    pub fn load(path: &Path, backend: WasmBackend) -> Result<Rc<RefCell<WasmModule>>, String>
    {
        match backend
        {
            WasmBackend::Wasmi => Self::load_wasmi(path),
            #[cfg(feature = "wasmtime")]
            WasmBackend::Wasmtime => Self::load_wasmtime(path),
        }
    }

    #[allow(dead_code)]
    pub fn backend(&self) -> WasmBackend
    {
        self.backend
    }

    pub fn ensure_memory(&mut self)
    {
        if self.memory.is_some()
        {
            return;
        }
        match self.backend
        {
            WasmBackend::Wasmi =>
            {
                if let Some(backend) = self.wasmi.as_ref()
                {
                    let memory = backend
                        .instance
                        .get_export(&backend.store, "memory")
                        .and_then(|e| e.into_memory());
                    self.memory = memory.map(WasmMemoryHandle::Wasmi);
                }
            }
            #[cfg(feature = "wasmtime")]
            WasmBackend::Wasmtime =>
            {
                if let Some(backend) = self.wasmtime.as_mut()
                {
                    let memory = backend.instance.get_memory(&mut backend.store, "memory");
                    self.memory = memory.map(WasmMemoryHandle::Wasmtime);
                }
            }
        }
    }

    pub fn memory_data(&mut self) -> Option<&[u8]>
    {
        self.ensure_memory();
        match self.backend
        {
            WasmBackend::Wasmi =>
            {
                let backend = self.wasmi.as_mut()?;
                match self.memory.as_ref()?
                {
                    WasmMemoryHandle::Wasmi(mem) => Some(mem.data(&backend.store)),
                    #[cfg(feature = "wasmtime")]
                    WasmMemoryHandle::Wasmtime(_) => None,
                }
            }
            #[cfg(feature = "wasmtime")]
            WasmBackend::Wasmtime =>
            {
                let backend = self.wasmtime.as_mut()?;
                match self.memory.as_ref()?
                {
                    WasmMemoryHandle::Wasmtime(mem) => Some(mem.data(&backend.store)),
                    WasmMemoryHandle::Wasmi(_) => None,
                }
            }
        }
    }

    pub fn memory_data_mut(&mut self) -> Option<&mut [u8]>
    {
        self.ensure_memory();
        match self.backend
        {
            WasmBackend::Wasmi =>
            {
                let backend = self.wasmi.as_mut()?;
                match self.memory.as_ref()?
                {
                    WasmMemoryHandle::Wasmi(mem) => Some(mem.data_mut(&mut backend.store)),
                    #[cfg(feature = "wasmtime")]
                    WasmMemoryHandle::Wasmtime(_) => None,
                }
            }
            #[cfg(feature = "wasmtime")]
            WasmBackend::Wasmtime =>
            {
                let backend = self.wasmtime.as_mut()?;
                match self.memory.as_ref()?
                {
                    WasmMemoryHandle::Wasmtime(mem) => Some(mem.data_mut(&mut backend.store)),
                    WasmMemoryHandle::Wasmi(_) => None,
                }
            }
        }
    }

    pub fn call_func(
        &mut self,
        func: &WasmFuncHandle,
        args: &[WasmValue],
        results: &mut [WasmValue],
    ) -> Result<(), String>
    {
        match (self.backend, func)
        {
            (WasmBackend::Wasmi, WasmFuncHandle::Wasmi(func)) =>
            {
                let mut wasm_results: Vec<WasmiValue> =
                    vec![WasmiValue::I32(0); results.len()];
                let wasm_args: Vec<WasmiValue> = args.iter().cloned().map(wasmi_value_from).collect();
                let backend = self.wasmi.as_mut().ok_or_else(|| "Missing wasmi store".to_string())?;
                func.call(&mut backend.store, &wasm_args, &mut wasm_results)
                    .map_err(|e| format!("{}", e))?;
                for (slot, value) in results.iter_mut().zip(wasm_results)
                {
                    *slot = wasm_value_from_wasmi(value)?;
                }
                Ok(())
            }
            #[cfg(feature = "wasmtime")]
            (WasmBackend::Wasmtime, WasmFuncHandle::Wasmtime(func)) =>
            {
                let mut wasm_results: Vec<WasmtimeVal> =
                    vec![WasmtimeVal::I32(0); results.len()];
                let wasm_args: Vec<WasmtimeVal> =
                    args.iter().cloned().map(wasmtime_value_from).collect();
                let backend = self
                    .wasmtime
                    .as_mut()
                    .ok_or_else(|| "Missing wasmtime store".to_string())?;
                func.call(&mut backend.store, &wasm_args, &mut wasm_results)
                    .map_err(|e| format!("{}", e))?;
                for (slot, value) in results.iter_mut().zip(wasm_results)
                {
                    *slot = wasm_value_from_wasmtime(value)?;
                }
                Ok(())
            }
            #[cfg(feature = "wasmtime")]
            _ => Err("Wasm backend mismatch".to_string()),
        }
    }

    fn load_wasmi(path: &Path) -> Result<Rc<RefCell<WasmModule>>, String>
    {
        let engine = WasmiEngine::default();
        let bytes = fs::read(path).map_err(|e| format!("Failed to read wasm module: {}", e))?;
        let module = WasmiModule::new(&engine, &mut &bytes[..])
            .map_err(|e| format!("Failed to load wasm module: {}", e))?;
        let mut store = WasmiStore::new(&engine, ());
        let mut linker = WasmiLinker::new(&engine);

        for import in module.imports()
        {
            if let WasmiExternType::Func(func_type) = import.ty()
            {
                let func =
                    WasmiFunc::new(&mut store, func_type.clone(), |_caller, _params, _results| Ok(()));
                linker
                    .define(import.module(), import.name(), func)
                    .map_err(|e| {
                        format!(
                            "Failed to define import {}::{}: {}",
                            import.module(),
                            import.name(),
                            e
                        )
                    })?;
            }
        }

        let instance = linker
            .instantiate(&mut store, &module)
            .map_err(|e| format!("Failed to instantiate wasm module: {}", e))?
            .start(&mut store)
            .map_err(|e| format!("Failed to start wasm module: {}", e))?;

        let memory = instance
            .get_export(&store, "memory")
            .and_then(|e| e.into_memory())
            .map(WasmMemoryHandle::Wasmi);

        let alloc = instance
            .get_export(&store, "alloc")
            .and_then(|e| e.into_func())
            .map(WasmFuncHandle::Wasmi);

        let dealloc = instance
            .get_export(&store, "dealloc")
            .and_then(|e| e.into_func())
            .map(WasmFuncHandle::Wasmi);

        let wbindgen_malloc = instance
            .get_export(&store, "__wbindgen_malloc")
            .and_then(|e| e.into_func())
            .map(WasmFuncHandle::Wasmi);

        let wbindgen_free = instance
            .get_export(&store, "__wbindgen_free")
            .and_then(|e| e.into_func())
            .map(WasmFuncHandle::Wasmi);

        let wbindgen_add_to_stack_pointer = instance
            .get_export(&store, "__wbindgen_add_to_stack_pointer")
            .and_then(|e| e.into_func())
            .map(WasmFuncHandle::Wasmi);

        let mut functions = FxHashMap::default();
        let mut func_types = FxHashMap::default();
        for export in module.exports()
        {
            let name: &str = export.name();
            if let WasmiExternType::Func(func_type) = export.ty()
            {
                if let Some(func) = instance
                    .get_export(&store, name)
                    .and_then(|e| e.into_func())
                {
                    let name_rc = Rc::new(name.to_string());
                    functions.insert(name_rc.clone(), WasmFuncHandle::Wasmi(func));
                    func_types.insert(name_rc, WasmFuncType::from_wasmi(&func_type)?);
                }
            }
        }

        Ok(Rc::new(RefCell::new(WasmModule {
            backend: WasmBackend::Wasmi,
            wasmi: Some(WasmiBackend { store, instance }),
            #[cfg(feature = "wasmtime")]
            wasmtime: None,
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

    #[cfg(feature = "wasmtime")]
    fn load_wasmtime(path: &Path) -> Result<Rc<RefCell<WasmModule>>, String>
    {
        let engine = WasmtimeEngine::default();
        let bytes = fs::read(path).map_err(|e| format!("Failed to read wasm module: {}", e))?;
        let module = WasmtimeModule::new(&engine, &bytes)
            .map_err(|e| format!("Failed to load wasm module: {}", e))?;
        let mut store = WasmtimeStore::new(&engine, ());
        let mut linker = WasmtimeLinker::new(&engine);

        for import in module.imports()
        {
            if let WasmtimeExternType::Func(func_type) = import.ty()
            {
                let func = WasmtimeFunc::new(
                    &mut store,
                    func_type.clone(),
                    |_caller, _params, _results| Ok(()),
                );
                linker
                    .define(&mut store, import.module(), import.name(), func)
                    .map_err(|e| {
                        format!(
                            "Failed to define import {}::{}: {}",
                            import.module(),
                            import.name(),
                            e
                        )
                    })?;
            }
        }

        let instance = linker
            .instantiate(&mut store, &module)
            .map_err(|e| format!("Failed to instantiate wasm module: {}", e))?;

        let memory = instance
            .get_memory(&mut store, "memory")
            .map(WasmMemoryHandle::Wasmtime);

        let alloc = instance
            .get_func(&mut store, "alloc")
            .map(WasmFuncHandle::Wasmtime);

        let dealloc = instance
            .get_func(&mut store, "dealloc")
            .map(WasmFuncHandle::Wasmtime);

        let wbindgen_malloc = instance
            .get_func(&mut store, "__wbindgen_malloc")
            .map(WasmFuncHandle::Wasmtime);

        let wbindgen_free = instance
            .get_func(&mut store, "__wbindgen_free")
            .map(WasmFuncHandle::Wasmtime);

        let wbindgen_add_to_stack_pointer = instance
            .get_func(&mut store, "__wbindgen_add_to_stack_pointer")
            .map(WasmFuncHandle::Wasmtime);

        let mut functions = FxHashMap::default();
        let mut func_types = FxHashMap::default();
        for export in module.exports()
        {
            let name: &str = export.name();
            if let WasmtimeExternType::Func(func_type) = export.ty()
            {
                if let Some(func) = instance.get_func(&mut store, name)
                {
                    let name_rc = Rc::new(name.to_string());
                    functions.insert(name_rc.clone(), WasmFuncHandle::Wasmtime(func));
                    func_types.insert(name_rc, WasmFuncType::from_wasmtime(&func_type)?);
                }
            }
        }

        Ok(Rc::new(RefCell::new(WasmModule {
            backend: WasmBackend::Wasmtime,
            wasmi: None,
            wasmtime: Some(WasmtimeBackend { store, instance }),
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
