[![Clojure](https://github.com/repl-acement/destructive/actions/workflows/clojure.yml/badge.svg)](https://github.com/repl-acement/destructive/actions/workflows/clojure.yml)

# destructive
Make the [Clojure destructuring guide](https://clojure.org/guides/destructuring) into code.

# How it works

## API
`(let->destructured-let code-string)`

## Details
Takes a string of code containing only a `let` statement. 

Produces a result that comprises data attached to the following keys:

`:inputs` - the input data and spec
`:parse` - the fully parsed form
`:analysis` - details of the bindings
`:transform` - perform the destructuring logic by applying the analysis data to the parsed data
`:unform` - the `unform` data and the `unformed` form

The `unformed` form can be evaluated

## Tests
You can see expected usage patterns in the tests.

## Example

```clojure
(let [in-bindings '(let [m {:a/k1 1 :b/k2 2 :c/k3 3}
                         k1 (get m :a/k1)
                         k2 (:b/k2 m)]
                     (+ k1 k2))]
  (->> (pr-str in-bindings)
       let->destructured-let))
=>
{:inputs {:string-form "(let [m {:a/k1 1, :b/k2 2, :c/k3 3} k1 (get m :a/k1) k2 (:b/k2 m)] (+ k1 k2))",
          :edn-form (let [m {:a/k1 1, :b/k2 2, :c/k3 3} k1 (get m :a/k1) k2 (:b/k2 m)] (+ k1 k2)),
          :spec :destructive.destructure/form},
 :parse {:form-name :let,
         :parsed-form {:name let,
                       :bindings [{:form [:local-symbol m], :init-expr {:a/k1 1, :b/k2 2, :c/k3 3}}
                                  {:form [:local-symbol k1], :init-expr (get m :a/k1)}
                                  {:form [:local-symbol k2], :init-expr (:b/k2 m)}],
                       :exprs (+ k1 k2)},
         :bindings-symbols {m {:literal {:a/k1 1, :b/k2 2, :c/k3 3}},
                            k1 {:form-name :get,
                                :parsed-form {:name get, :map [:symbol m], :key :a/k1},
                                :key :a/k1,
                                :map {:ref m}},
                            k2 {:form-name :lookup,
                                :parsed-form {:key :b/k2, :map [:symbol m]},
                                :key :b/k2,
                                :map {:ref m}}}},
 :analysis {:bindings {:map-accessors {m [{:symbol m,
                                           :accessor k1,
                                           :key {:keyword :a/k1, :name "k1", :namespace "a"}}
                                          {:symbol m,
                                           :accessor k2,
                                           :key {:keyword :b/k2, :name "k2", :namespace "b"}}]}}},
 :transform {:bindings [{:form [:local-symbol m], :init-expr {:a/k1 1, :b/k2 2, :c/k3 3}}
                        {:form [:map-destructure {:a/keys [k1], :b/keys [k2]}], :init-expr m}]},
 :unform {:unform-form {:name let,
                        :bindings [{:form [:local-symbol m], :init-expr {:a/k1 1, :b/k2 2, :c/k3 3}}
                                   {:form [:map-destructure {:a/keys [k1], :b/keys [k2]}],
                                    :init-expr m}],
                        :exprs (+ k1 k2)},
          :unformed (let [m {:a/k1 1, :b/k2 2, :c/k3 3} {:a/keys [k1], :b/keys [k2]} m] (+ k1 k2))}}
```

# FAQ

### Why are you doing this?
To see how far spec will take us in parsing and analysing Clojure code.

### Why are you using spec?
Although it's incomplete and buggy, we have it shipped with Clojure.

### Why not use spec2?
Yeah, when it's "done" we (or anyone else) can do this again with spec2.

### Why accept strings as inputs?
Editors operate on strings and this tool should be usable from an editor.

### How far are you along with the features?
See TODO.

# TODO
## Associative destructuring 
- [X] Parse let and get
- [X] Bindings, top level keys & tests
  - [X] key access in bindings
  - [X] multiple unqualified key access
  - [X] namespaced key access
  - [X] multiple namespaced key access
  - [X] renamed keys
  - [X] namespaced keys with binding X are renamed
  - [X] binding to missing key produces nil
  - [X] default binding results (`:or`)
- [ ] :strs and :syms for string and symbol keys respectively.
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
## Sequential destructuring
- [ ] Become motivated to do this


## Credits, thanks and references

### `conform` and `unform`
I found this [detailed post](https://blog.klipse.tech/clojure/2019/03/08/spec-custom-defn.html) from [Yehonathan Sharvit](https://blog.klipse.tech/).

It was especially useful in understanding how to manipulate the data that is produced by `conform` and ensure that it stays well-formed for the `unform` function.

### Handling namespaced keywords

I was stuck with how qualified keys are emitted. 

Thanks to Lasse Määttä who suggested using [*print-namespace-maps*](https://clojuredocs.org/clojure.core/*print-namespace-maps*) via `#clojure-spec` on the Clojurian Slack

### spec.alpha vs spec2

Thanks to Alex Miller from the core team for suggesting that spec2 is dormant not dead. 

Kinda reminded me of the Monty Python Norwegian parrot sketch, but hopefully we will get a better result.

In the meantime, we'll make the most of what we have, and no pining for the fjords.

