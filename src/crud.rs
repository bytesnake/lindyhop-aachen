pub trait Crud {
    type Id;
    type Item;

    fn create(&mut self, item: Self::Item) -> Self::Id;
    fn read(&self, id: &Self::Id) -> &Self::Item;
    fn update(&mut self, id: Self::Id, item: Self::Item);
    fn delete(&mut self, id: Self::Id) -> Self::Item;
}

pub struct CrudNotifier<S, C> where S: Crud, C: Fn(&S) {
    store: S,
    callback: C,
}

impl<S, C> CrudNotifier<S, C> where S: Crud, C: Fn(&S){
    pub fn from(&mut self, store: S, callback: C) -> CrudNotifier<S,C> {
        CrudNotifier {
            store,
            callback
        }
    }
}

impl<S,C> Crud for CrudNotifier<S,C> where S: Crud, C: Fn(&S)  {
    type Id = S::Id;
    type Item = S::Item;

    fn create(&mut self, item: Self::Item) -> Self::Id {
        let id = self.store.create(item);
        (self.callback)(&self.store); 

        id
    }

    fn read(&self, id: &Self::Id) -> &Self::Item {
        self.store.read(id)
    }

    fn update(&mut self, id: Self::Id, item: Self::Item) {
        self.store.update(id, item);
        (self.callback)(&self.store);
    }

    fn delete(&mut self, id: Self::Id) -> Self::Item {
        let deleted = self.store.delete(id);
        (self.callback)(&self.store);

        deleted
    }
}