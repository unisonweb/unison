<!-- The route the topics teach: one `move` per name. `move` takes the term, the type and the namespace under a name
     together, so the doc, the tests and the constructor follow. The constructor keeps its own last segment, though:
     `move Sku Item` makes it `Item.Sku`, so it takes a move of its own. -->

``` ucm
scratch/main> move Stock.restock Stock.replenish
scratch/main> move Sku Item
scratch/main> move Item.Sku Item.Item
scratch/main> find
scratch/main> names Stock.replenish
scratch/main> names Item
scratch/main> names Item.parse
scratch/main> names Stock.receiveAll
scratch/main> docs Stock.replenish
scratch/main> view Stock.receiveAll
scratch/main> test
scratch/main> branches
scratch/main> todo
```
