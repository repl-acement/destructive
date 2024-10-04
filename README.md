# destructive
Make the [Clojure destructuring guide](https://clojure.org/guides/destructuring) into code

## How it works
`(let->destructured-let code-string)`

Takes a string of code containing only a `let` statement. 

When it has a map and some bindings or expressions to access its properties, give back an updated form that uses destructuring.

### TODO
- [X] Parse let and get
- [ ] Bindings, top level keys
  - [X] Round-trip unqualified key access in bindings
  - [X] Round-trip multiple unqualified key access
  - [ ] Round-trip namespaced key access
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Bindings, nested keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Round-trip multiple unqualified key access
  - [ ] Round-trip namespaced key access
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Bindings, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions, top level keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Round-trip multiple unqualified key access
  - [ ] Round-trip namespaced key access
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, nested keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Round-trip multiple unqualified key access
  - [ ] Round-trip namespaced key access
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions and bindings
  - [ ] Tests
- [ ] Linter checks

