# informish

Experiments with implementing inform7 style syntax with clojure and core.logic.  My intent is to learn more about declarative logic, constraints, predicates, and natural language.

### logic proxies

I'd like to track logical values with a proxy, at least until I understand core.logic types better.

A cvar type tracks the constraints, dependencies, and relation facts that have been stated.  Solving a cvar returns a dependent cvar.  Solving can query logic if it has a finite solution domain (and filter with the predicate tests).

Some solving queries can be answered by the predicates alone, during a prepass phase where common constraint patterns have obvious answers.

```clj
(solve count (cvar))
;#<CVar #13number?(count #14)>
;attempting to solve count resulted in a dependent cvar constrained to a number.

(solve count (c- (cvar) empty?))
;0
;the empty? constraint provided a solution.

the to-string of cvars shows a #uid, constraints, and the dependent inputs.

```

### definite clause grammar (DCG)

DCG parses can find valid expressions from a sentance.

The solutions are converted to code containing literals and unsolved logic variables.

```clj
(parse '[:name of player])
;{(solve :name player) "john"}
```


```clj
(parse '[:name of first :siblings of thing])
;(solve :name (solve first (solve :siblings thing)))
;#<CVar #15(:name #16(first #17(:siblings #18)))>
```



### constraint dependencies

When a logic query is used as an input for the value of a definition, it becomes a derivative.  The dependencies are tracked.

### Persistance

Cvars use unique identifiers.


### constraints

Constraints are sets of predicates, when a logic variable has a finite solution space, the constraints filter.

```inform7
'[The name of the culprit has a count of 5.]

;His name has no relations to query, so it's just an abstract with a #(= (count %) 5) constraint

'[The culprit's name is an entry in the list of the names of streets]

;Now his name is a constraint on a finite domain, and could be solved and converted to a literal.
```

### constraints as types

Inform7 uses single inheritence for types.
```A sign is a kind of thing with a string called message. The message of a sign is usually "nothing".```

Use a cvar for a type.  Any cvars that have type relations would be a merge of cvars in order of relation precedence.

```inform7
A safe is a kind of box with a number called combination.

Assets are a type of furniture, assets have a number called value.

A safe is a type of box, safes have a number called combination {:combination (cvar number?)}

A vault is a kind of safe, and a kind of asset. The combination of a vault is a number
above 100. {:value (cvar number?) :combination (cvar number? #(> (count %) 100))}
```


http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf
