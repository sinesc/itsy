use crate::prelude::*;
use crate::shared::typed_ids::ScopeId;

/// A datastructure that stores items by name and index.
pub(crate) struct Repository<K, I, V> {
    map     : UnorderedMap<(K, ScopeId), I>,
    data    : Vec<(V, ScopeId)>, // TODO: scope_id on data not required anymore. remove ScopeId and commented functions that used it
}

impl<K, I, V> Repository<K, I, V> where I: Copy + Into<usize> + From<usize>, K: Hash + Eq {
    /// Creates a new repository.
    pub fn new() -> Self {
        Repository {
            map: UnorderedMap::new(),
            data: Vec::new(),
        }
    }
    /// Inserts an item into the repository and returns its index.
    pub fn insert(self: &mut Self, scope_id: ScopeId, name: Option<K>, element: V) -> I {
        let index = I::from(self.data.len());
        self.data.push((element, scope_id));
        if let Some(name) = name {
            self.map.insert((name, scope_id), index);
        }
        index
    }
    /*
    /// Aliases an item with a new new.
    pub fn alias(self: &mut Self, alias_scope_id: ScopeId, alias_name: K, source_index: I) -> I {
        self.map.insert((alias_name, alias_scope_id), source_index);
        source_index
    }
    */
    /// Fetches an item by its id.
    pub fn value_by_id(self: &Self, index: I) -> &V {
        &self.data[index.into()].0
    }
    /// Mutably fetches an item by its id.
    pub fn value_by_id_mut(self: &mut Self, index: I) -> &mut V {
        &mut self.data[index.into()].0
    }
    /// Returns the id of the named item.
    pub fn id_by_name(self: &Self, scope_id: ScopeId, name: K) -> Option<I> {
        self.map.get(&(name, scope_id)).map(|i| *i)
    }
    /// Returns the name of the given id.
    pub fn name_by_id(self: &Self, index: I) -> Option<&K> where I: PartialEq  {
        self.map.iter().find(|&item| *item.1 == index).map(|item| &(item.0).0)
    }
    /// Returns an iterator over the items.
    pub fn values<'s>(self: &'s Self) -> impl Iterator<Item = &'s V> {
        self.data.iter().map(|item| &item.0)
    }
    /// Returns the number of items in this Repository.
    pub fn len(self: &Self) -> usize  {
        self.data.len()
    }
    /// Returns the id of the given value.
    pub fn id_by_value(self: &Self, value: &V) -> Option<I> where V: Eq {
        self.data.iter().enumerate().find_map(|item| if &item.1.0 == value { Some(item.0.into()) } else { None })
    }
    /// Returns the id of the given value.
    pub fn id_search(self: &Self, mut searcher: impl FnMut(&V) -> bool) -> Option<I> {
        self.data.iter().enumerate().find_map(|item| if searcher(&item.1.0) { Some(item.0.into()) } else { None })
    }
    /*
    /// Sets a name for given index or removes it.
    pub fn set_name(self: &mut Self, index: I, name: Option<String>) where I: PartialEq {
        let scope_id = self.data[index.into()].1;
        if let Some(name) = name {
            self.map.entry((name, scope_id)).or_insert(index);
        } else if let Some(name) = self.map.iter().find(|&item| *item.1 == index).map(|item| (*item.0).clone()) {
            self.map.remove(&name);
        }
    }
    /// Fetches an item scope by item id.
    pub fn scope_by_id(self: &Self, index: I) -> ScopeId {
        self.data[index.into()].1
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
    */
}

impl<K, I, V> Into<Vec<V>> for Repository<K, I, V> {
    fn into(self: Self) -> Vec<V> {
        self.data.into_iter().map(|item| item.0).collect()
    }
}

/*
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
*/