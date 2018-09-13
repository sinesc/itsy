use std::collections::HashMap;
use std::cmp::Eq;
use std::hash::Hash;
use std::fmt::{Debug, Formatter, Result};
use std::ops::Index;
use std::borrow::Borrow;
//use std::slice::IterMut;
use std::iter::FromIterator;

/// A datastructure that stores items by name and index.
pub(crate) struct Repository<V, I = usize, K = String> {
    map: HashMap<K, I>,
    data: Vec<V>,
}

impl<V, I, K> Repository<V, I, K> where K: Eq + Hash, I: Copy + Into<usize> + From<usize> {
    /// Creates a new repository.
    pub fn new() -> Self {
        Repository {
            map: HashMap::new(),
            data: Vec::new(),
        }
    }
    /* /// Creates a new repository with given capacity.
    pub fn with_capacity(cap: usize) -> Self {
        Repository {
            map: HashMap::with_capacity(cap),
            data: Vec::with_capacity(cap),
        }
    }*/
    /// Inserts an item into the repository and returns its index.
    pub fn insert(self: &mut Self, name: K, element: V) -> I {
        let index = I::from(self.data.len());
        self.data.push(element);
        self.map.insert(name, index);
        index
    }
    /// Fetches an item by index.
    pub fn index(self: &Self, index: I) -> &V {
        &self.data[index.into()]
    }
    /// Fetches an item by index.
    pub fn index_mut(self: &mut Self, index: I) -> &mut V {
        &mut self.data[index.into()]
    }
    /// Fetches an item by name.
    pub fn name<Q: ?Sized>(self: &Self, name: &Q) -> Option<&V> where K: Borrow<Q>, Q: Hash + Eq {
        if let Some(&index) = self.map.get(name) {
            Some(&self.data[index.into()])
        } else {
            None
        }
    }
    /* /// Fetches an item mutably by name.
    pub fn name_mut<Q: ?Sized>(self: &mut Self, name: & Q) -> Option<& mut V> where K: Borrow<Q>, Q: Hash + Eq {
        if let Some(&index) = self.map.get(name) {
            Some(&mut self.data[Into::<usize>::into(index)])
        } else {
            None
        }
    }*/
    /// Returns the index of the named item.
    pub fn index_of<Q: ?Sized>(self: &Self, name: &Q) -> Option<I> where K: Borrow<Q>, Q: Hash + Eq {
        self.map.get(name).map(|i| *i)
    }
    /* /// Returns an iterator over the mutable items.
    pub fn values_mut(self: &mut Self) -> IterMut<V> {
        self.data.iter_mut()
    }*/
}

impl<V, I, K> Into<Vec<V>> for Repository<V, I, K> {
    fn into(self: Self) -> Vec<V> {
        self.data
    }
}

impl<'a, K, Q: ?Sized, V, I> Index<&'a Q> for Repository<V, I, K>
    where K: Eq + Hash + Borrow<Q>,
          Q: Eq + Hash,
          I: Debug + Copy + Into<usize> + From<usize>
{
    type Output = V;

    #[inline]
    fn index(&self, key: &Q) -> &V {
        self.name(key).expect("no entry found for key")
    }
}

impl<V, I, K> Debug for Repository<V, I, K>
where
    K: Eq + Hash + Debug,
    V: Debug,
    I: Debug + Copy + Into<usize> + From<usize>
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        f
            .debug_map()
            .entries(self.map.iter().map(|(k, &v)| {
                ((k, v), &self.data[v.into()])
            }))
            .finish()
    }
}

impl<V, I, K> FromIterator<(K, V)> for Repository<V, I, K>
where
    K: Eq + Hash,
    I: Copy + Into<usize> + From<usize>
{
    fn from_iter<F: IntoIterator<Item=(K, V)>>(iter: F) -> Self {

        let mut repository = Repository::new();

        for (k, v) in iter {
            repository.insert(k, v);
        }

        repository
    }
}