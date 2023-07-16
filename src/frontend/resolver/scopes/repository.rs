use crate::prelude::*;

/// A datastructure that stores items by name and index.
pub(crate) struct Repository<K, I, V> {
    map     : UnorderedMap<K, I>,
    data    : Vec<V>,
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
    pub fn insert(self: &mut Self, name: Option<K>, element: V) -> I {
        let index = I::from(self.data.len());
        self.data.push(element);
        if let Some(name) = name {
            self.map.insert(name, index);
        }
        index
    }
    /// Fetches an item by its id.
    pub fn value_by_id(self: &Self, index: I) -> &V {
        &self.data[index.into()]
    }
    /// Mutably fetches an item by its id.
    pub fn value_by_id_mut(self: &mut Self, index: I) -> &mut V {
        &mut self.data[index.into()]
    }
    /// Returns the id of the named item.
    pub fn id_by_name(self: &Self, name: &K) -> Option<I> {
        self.map.get(name).map(|i| *i)
    }
    /// Returns the name of the given id.
    pub fn name_by_id(self: &Self, index: I) -> Option<&K> where I: PartialEq  {
        self.map.iter().find(|&item| *item.1 == index).map(|item| item.0)
    }
    /// Returns an iterator over the items.
    pub fn values<'s>(self: &'s Self) -> impl Iterator<Item = &'s V> {
        self.data.iter().map(|item| item)
    }
    /// Returns the number of items in this Repository.
    pub fn len(self: &Self) -> usize  {
        self.data.len()
    }
    /// Returns the id of the given value.
    pub fn id_by_value(self: &Self, value: &V) -> Option<I> where V: Eq {
        self.data.iter().enumerate().find_map(|item| if item.1 == value { Some(item.0.into()) } else { None })
    }
    /// Returns the id of the given value.
    pub fn id_search(self: &Self, mut searcher: impl FnMut(&V) -> bool) -> Option<I> {
        self.data.iter().enumerate().find_map(|item| if searcher(item.1) { Some(item.0.into()) } else { None })
    }
}

impl<K, I, V> Into<Vec<V>> for Repository<K, I, V> {
    fn into(self: Self) -> Vec<V> {
        self.data.into_iter().map(|item| item).collect()
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