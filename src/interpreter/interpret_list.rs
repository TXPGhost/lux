use crate::interpreter::*;

impl<T: Interpret + Clone + Debug> Interpret for Node<Vec<T>>
where
    T::Output: Clone + Debug,
{
    type Output = Node<Vec<T::Output>>;
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        let mut new_elements = Vec::with_capacity(self.val.len());
        for element in self.val {
            new_elements.push(element.interp(context)?)
        }
        Ok(new_elements.node(loc))
    }
}
