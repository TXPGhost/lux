use crate::interpreter::*;

impl<T: Interpret + Clone + Debug> Interpret for Node<List<T>>
where
    T::Output: Clone + Debug,
{
    type Output = Node<List<T::Output>>;
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        let mut new_elements = Vec::with_capacity(self.value.elements.len());
        for element in self.value.elements {
            new_elements.push(element.interp(context)?)
        }
        Ok(Node {
            value: List {
                elements: new_elements,
            },
            loc,
        })
    }
}
