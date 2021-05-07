/// A simple environment type for tracking name bindings
use std::collections::HashMap;
use std::hash::Hash;

/// A simple environment map to track name bindings in a stack of frames
pub struct SimpleEnvironment<K, V>
where
    K: Eq + Hash,
{
    envs: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash, V> SimpleEnvironment<K, V> {
    pub fn new() -> Self {
        SimpleEnvironment { envs: Vec::new() }
    }

    pub fn push(&mut self, frame: HashMap<K, V>) {
        self.envs.push(frame)
    }

    pub fn pop(&mut self) {
        self.envs.pop();
    }

    /// Insert a binding in the top frame
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.envs.last_mut().unwrap().insert(key, value)
    }

    /// Get topmost binding for the key
    pub fn get(&self, key: &K) -> Option<&V> {
        for env in &self.envs {
            if let Some(v) = env.get(key) {
                return Some(v);
            }
        }
        None
    }
}

impl<K, V> Default for SimpleEnvironment<K, V>
where
    K: Eq + Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

/// An enhanced environment which defaults values on encounter
pub struct DefaultingEnvironment<K, V>
where
    K: Eq + Hash,
{
    env: SimpleEnvironment<K, V>,
    globals: HashMap<K, V>,
    defaulter: Box<dyn Fn(&K) -> V>,
}

impl<K: Eq + Hash + Clone, V> DefaultingEnvironment<K, V> {
    /// Create, setting a fn for defaulting values from keys
    pub fn new(defaulter: impl Fn(&K) -> V + 'static) -> Self {
        DefaultingEnvironment {
            env: SimpleEnvironment::new(),
            globals: HashMap::new(),
            defaulter: Box::new(defaulter),
        }
    }

    /// Push a frame of bindings that take precedence over everything else
    pub fn push(&mut self, frame: HashMap<K, V>) {
        self.env.push(frame)
    }

    /// Push a frame where values are defaulted from keys
    pub fn push_keys<I>(&mut self, keys: I)
    where
        I: IntoIterator<Item = K>,
    {
        let mut m = HashMap::new();
        for k in keys {
            let v = (self.defaulter)(&k);
            m.insert(k, v);
        }
        self.push(m);
    }

    /// Pop the top frame
    pub fn pop(&mut self) {
        self.env.pop();
    }

    /// Get from environment first, or globals next
    pub fn get(&self, key: &K) -> Option<&V> {
        self.env.get(key).or_else(|| self.globals.get(key))
    }

    /// encounter a new variable and mint a representation if necessary
    pub fn encounter(&mut self, key: K) {
        if self.get(&key).is_none() {
            let v = (self.defaulter)(&key);
            self.globals.insert(key, v);
        }
    }
}
