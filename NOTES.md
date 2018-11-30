```elm
viewEntryList model.entries

viewEntryList : List Entry -> Html msg
viewEntryList = entries
    ul  []  [li [] [], li [] []]
```

Para hacer la lista de `li`s utilizaremos la func `List.map`

```elm
List.map: (a -> b) -> List a -> List b

viewEntryList = entries
    ul  [] (List.map function entries)
```

La function tiene que ser del tipo `(a -> b)` `(Entry -> Html.li)`

```elm
viewEntryItem: Entry -> Html msg
viewENtryItem entrie =
    li []   [ text entry.prhase, text (fromInt entry.points)]
```

y entonces la funcion `viewEntryList` quedaria:

```elm
viewEntryList entries =
    ul  [] (List.map viewEntryItem entries)
```

lo podemos poner mejor con una let expression

```elm
viewEntryList entries =
    let
       listOfEntries =  List.map viewEntryItem entries
    in
        ul []  listOfEntries
```
