use alloc::{string::{String, ToString}, vec::Vec};
use hashbrown::HashMap;
use iced_x86::code_asm::CodeLabel;
use thiserror_no_std::Error;

#[derive(Debug, Clone)]
struct Scope {
    name: String,
    unnamed_scope_counter: usize,
    functions: HashMap<String, CodeLabel>,
}

impl Scope {
    fn new(name: String) -> Self {
        Self {
            name,
            unnamed_scope_counter: 0,
            functions: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeManager {
    global_scope: Scope,
    scopes: Vec<Scope>,
}

fn merge_scopes(dest: &mut Scope, mut src: Scope) {
    dest.functions.extend(
        src.functions
            .drain()
            .map(|(k, v)| (format!("{}{{{}", src.name, k), v)),
    );
}

impl ScopeManager {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::new("HF_GLOBAL_SCOPE_UNUSED".to_string()),
            scopes: Vec::new()
        }
    }

    pub fn get_global_functions(&self) -> &HashMap<String, CodeLabel> {
        if !self.scopes.is_empty() {
            unreachable!("global functions should only be accessed when no scopes are active")
        }
        &self.global_scope.functions
    }

    pub fn push_scope(&mut self, name: String) {
        self.scopes.push(Scope::new(name));
    }

    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            let popped = self.scopes.pop().unwrap();
            let last = self.scopes.last_mut().unwrap();
            merge_scopes(last, popped);
        } else if self.scopes.len() == 1 {
            let last = self.scopes.pop().unwrap();
            merge_scopes(&mut self.global_scope, last);
        }
    }

    pub fn push_fn(&mut self, function: (String, CodeLabel)) {
        let last = self.scopes.last_mut().unwrap_or(&mut self.global_scope);
        last.functions.insert(function.0, function.1);
    }

    pub fn get_fn(&self, name: &String) -> Option<CodeLabel> {
        for scope in self.scopes.iter().rev() {
            if let Some(label) = scope.functions.get(name) {
                return Some(label.clone());
            }
        }
        self.global_scope.functions.get(name).cloned()
    }

    pub fn get_top_scope_name(&self) -> Option<String> {
        self.scopes.last().map(|s| s.name.clone())
    }

    pub fn next_unnamed_scope_number(&mut self) -> usize {
        if let Some(scope) = self.scopes.last_mut() {
            scope.unnamed_scope_counter += 1;
            scope.unnamed_scope_counter
        } else {
            self.global_scope.unnamed_scope_counter += 1;
            self.global_scope.unnamed_scope_counter
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use iced_x86::code_asm::CodeAssembler;

    use super::*;

    #[test]
    fn test_basic_scope_manager() {
        let mut code_asm = CodeAssembler::new(64).unwrap();
        let label = code_asm.create_label();

        let mut scope_manager = ScopeManager::new();
        scope_manager.push_scope("outer".to_string());
        scope_manager.push_scope("inner".to_string());
        scope_manager.push_fn(("hello".to_string(), label));
        assert!(scope_manager.get_fn(&"hello".to_string()).is_some());
        scope_manager.pop_scope();

        assert!(scope_manager.get_fn(&"hello".to_string()).is_none());

        assert_eq!(
            scope_manager.scopes[0].functions.keys().map(|s| s.clone()).collect::<Vec<String>>(),
            vec!["inner{hello".to_string()]
        );

        assert!(scope_manager.get_fn(&"inner{hello".to_string()).is_some());
    }

    #[test]
    fn test_next_unnamed_scope_number() {
        let mut scope_manager = ScopeManager::new();
        assert_eq!(scope_manager.next_unnamed_scope_number(), 1);
        assert_eq!(scope_manager.next_unnamed_scope_number(), 2);

        scope_manager.push_scope("outer".to_string());

        assert_eq!(scope_manager.next_unnamed_scope_number(), 1);
        assert_eq!(scope_manager.next_unnamed_scope_number(), 2);
        assert_eq!(scope_manager.next_unnamed_scope_number(), 3);

        scope_manager.push_scope("inner".to_string());

        assert_eq!(scope_manager.next_unnamed_scope_number(), 1);
        assert_eq!(scope_manager.next_unnamed_scope_number(), 2);

        scope_manager.pop_scope();
        scope_manager.pop_scope();

        assert_eq!(scope_manager.next_unnamed_scope_number(), 3);
    }
}
