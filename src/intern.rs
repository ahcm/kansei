use rustc_hash::FxHashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::thread_local;

thread_local! {
    static INTERNER: RefCell<FxHashMap<String, Rc<String>>> = RefCell::new(FxHashMap::default());
}

pub type SymbolId = u32;

#[derive(Default)]
struct SymbolInterner {
    map: FxHashMap<String, SymbolId>,
    names: Vec<Rc<String>>,
}

thread_local! {
    static SYMBOLS: RefCell<SymbolInterner> = RefCell::new(SymbolInterner::default());
}

pub fn intern(s: &str) -> Rc<String> {
    INTERNER.with(|map| {
        let mut guard = map.borrow_mut();
        if let Some(existing) = guard.get(s) {
            return existing.clone();
        }
        let owned = s.to_string();
        let rc = Rc::new(owned.clone());
        guard.insert(owned, rc.clone());
        rc
    })
}

pub fn intern_owned(s: String) -> Rc<String> {
    INTERNER.with(|map| {
        let mut guard = map.borrow_mut();
        if let Some(existing) = guard.get(s.as_str()) {
            return existing.clone();
        }
        let rc = Rc::new(s.clone());
        guard.insert(s, rc.clone());
        rc
    })
}

pub fn intern_symbol(s: &str) -> SymbolId {
    SYMBOLS.with(|symbols| {
        let mut guard = symbols.borrow_mut();
        if let Some(existing) = guard.map.get(s) {
            return *existing;
        }
        let owned = s.to_string();
        let id = guard.names.len() as SymbolId;
        let rc = intern(&owned);
        guard.names.push(rc);
        guard.map.insert(owned, id);
        id
    })
}

pub fn intern_symbol_owned(s: String) -> SymbolId {
    SYMBOLS.with(|symbols| {
        let mut guard = symbols.borrow_mut();
        if let Some(existing) = guard.map.get(s.as_str()) {
            return *existing;
        }
        let id = guard.names.len() as SymbolId;
        let rc = intern(s.as_str());
        guard.names.push(rc);
        guard.map.insert(s, id);
        id
    })
}

pub fn symbol_name(id: SymbolId) -> Rc<String> {
    SYMBOLS.with(|symbols| {
        let guard = symbols.borrow();
        guard
            .names
            .get(id as usize)
            .cloned()
            .unwrap_or_else(|| intern("<unknown>"))
    })
}
