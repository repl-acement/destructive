# destructive
Make the [Clojure destructuring guide](https://clojure.org/guides/destructuring) into code

## How it works
`(let->destructured-let code-string)`

Takes a string of code containing only a `let` statement. 

When it has a map and some bindings or expressions to access its properties, give back an updated form that uses destructuring.

### TODO
- [X] Parse let and get
- [ ] Bindings, top level keys
  - [ ] Convert unqualified key access in bindings
  - [ ] Convert multiple unqualified key access
  - [ ] Convert namespaced key access
  - [ ] Convert multiple namespaced key access
- [ ] Bindings, nested keys
  - [ ] Convert unqualified key access in bindings
  - [ ] Convert multiple unqualified key access
  - [ ] Convert namespaced key access
  - [ ] Convert multiple namespaced key access
  - [ ] Tests
- [ ] Bindings, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions, top level keys
  - [ ] Convert unqualified key access in bindings
  - [ ] Convert multiple unqualified key access
  - [ ] Convert namespaced key access
  - [ ] Convert multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, nested keys
  - [ ] Convert unqualified key access in bindings
  - [ ] Convert multiple unqualified key access
  - [ ] Convert namespaced key access
  - [ ] Convert multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions and bindings
  - [ ] Tests
- [ ] Linter checks

