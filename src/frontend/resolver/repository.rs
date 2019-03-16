use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Result};
use crate::util::ScopeId;

/// A datastructure that stores items by name and index.
pub(crate) struct Repository<I, V> {
    map     : HashMap<(String, ScopeId), I>,
    data    : Vec<(V, ScopeId)>,
}

#[allow(dead_code)] // todo: remove and clean up
impl<I, V> Repository<I, V> where I: Copy + Into<usize> + From<usize> {
    /// Creates a new repository.
    pub fn new() -> Self {
        Repository {
            map: HashMap::new(),
            data: Vec::new(),
        }
    }
    /// Inserts an item into the repository and returns its index.
    pub fn insert(self: &mut Self, scope_id: ScopeId, name: Option<String>, element: V) -> I {
        let index = I::from(self.data.len());
        self.data.push((element, scope_id));
        if let Some(name) = name {
            self.map.insert((name, scope_id), index);
        }
        index
    }
    /// Sets a name for given index or removes it.
    pub fn set_name(self: &mut Self, index: I, name: Option<String>) where I: PartialEq {
        let scope_id = self.data[index.into()].1;
        if let Some(name) = name {
            self.map.entry((name, scope_id)).or_insert(index);
        } else if let Some(name) = self.map.iter().find(|&item| *item.1 == index).map(|item| (*item.0).clone()) {
            self.map.remove(&name);
        }
    }
    /// Fetches an item by its id.
    pub fn by_id(self: &Self, index: I) -> &V {
        &self.data[index.into()].0
    }
    /// Mutably fetches an item by its id.
    pub fn by_id_mut(self: &mut Self, index: I) -> &mut V {
        &mut self.data[index.into()].0
    }
    /// Fetches an item by name.
    pub fn by_name(self: &Self, scope_id: ScopeId, name: &str) -> Option<&V> {
        if let Some(index) = self.id_of(scope_id, name) {
            Some(&self.data[index.into()].0)
        } else {
            None
        }
    }
    /// Mutably fetches an item by name.
    pub fn by_name_mut(self: &mut Self, scope_id: ScopeId, name: &str) -> Option<&mut V> {
        if let Some(index) = self.id_of(scope_id, name) {
            Some(&mut self.data[index.into()].0)
        } else {
            None
        }
    }
    /// Returns the id of the named item.
    pub fn id_of(self: &Self, scope_id: ScopeId, name: &str) -> Option<I> {
        self.map.get(&(name.to_string(), scope_id)).map(|i| *i)
    }
    /// Returns the name of the given id.
    pub fn name_of(self: &Self, index: I) -> Option<&str> where I: PartialEq  {
        self.map.iter().find(|&item| *item.1 == index).map(|item| &*(item.0).0)
    }
    /// Returns an iterator over the items.
    pub fn values<'s>(self: &'s Self) -> impl Iterator<Item = &'s V> {
        self.data.iter().map(|item| &item.0)
    }
}

impl<I, V> Into<Vec<V>> for Repository<I, V> {
    fn into(self: Self) -> Vec<V> {
        self.data.into_iter().map(|item| item.0).collect()
    }
}

impl<I, V> Debug for Repository<I, V>
where
    V: Debug,
    I: Debug + Copy + Into<usize> + From<usize>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f
            .debug_map()
            .entries(self.map.iter().map(|(k, &v)| {
                ((k, v), &self.data[v.into()])
            }))
            .finish()
    }
}