[![Clojure](https://github.com/repl-acement/destructive/actions/workflows/clojure.yml/badge.svg)](https://github.com/repl-acement/destructive/actions/workflows/clojure.yml)

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
  - [X] Tests using get and direct map lookups
  - [X] Round-trip multiple unqualified key access
  - [X] Tests using get and direct map lookups
  - [X] Round-trip namespaced key access
  - [X] Tests
  - [X] Round-trip multiple namespaced key access
  - [X] Tests
  - [ ] Namespaced keys with binding X are renamed
  - [ ] Tests
- [ ] Bindings, nested keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Tests
  - [ ] Round-trip multiple unqualified key access
  - [ ] Tests
  - [ ] Round-trip namespaced key access
  - [ ] Tests
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Bindings, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions, top level keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Tests
  - [ ] Round-trip multiple unqualified key access
  - [ ] Tests
  - [ ] Round-trip namespaced key access
  - [ ] Tests
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, nested keys
  - [ ] Round-trip unqualified key access in bindings
  - [ ] Tests
  - [ ] Round-trip multiple unqualified key access
  - [ ] Tests
  - [ ] Round-trip namespaced key access
  - [ ] Tests
  - [ ] Round-trip multiple namespaced key access
  - [ ] Tests
- [ ] Expressions, mix of top level and nested keys
  - [ ] Tests
- [ ] Expressions and bindings
  - [ ] Tests
- [ ] Linter checks

## References

I found this detailed post from [Yehonathan Sharvit](https://blog.klipse.tech/) especially useful in the understanding of how to manipulate the data that is produced by `conform` and ensure that it stays well-formed for the `unform` function.

https://blog.klipse.tech/clojure/2019/03/08/spec-custom-defn.html


