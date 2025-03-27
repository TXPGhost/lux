use crate::interpreter::*;

impl<T: Interpret + Clone + Debug> Interpret for Node<List<T>>
where
    T::Output: Clone + Debug,
{
    type Output = Node<List<T::Output>>;
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        let mut new_elements = Vec::with_capacity(self.val.elements.len());
        for element in self.val.elements {
            new_elements.push(element.interp(context)?)
        }
        Ok(List::new(new_elements).node(loc))
    }
}
