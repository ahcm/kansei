use rustc_hash::FxHashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::thread_local;

thread_local! {
    static INTERNER: RefCell<FxHashMap<String, Rc<String>>> = RefCell::new(FxHashMap::default());
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
