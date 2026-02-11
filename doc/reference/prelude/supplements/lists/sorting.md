### Sorting Examples

```eu
nums: [3, 1, 4, 1, 5] sort-nums          # [1, 1, 3, 4, 5]
words: ["banana", "apple", "cherry"] sort-strs  # ["apple", "banana", "cherry"]

people: [{name: "Zara" age: 30}, {name: "Alice" age: 25}]
by-name: people sort-by-str(_.name)       # sorted by name
by-age: people sort-by-num(_.age)         # sorted by age
```
