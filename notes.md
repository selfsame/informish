
(comment
"
a number is provided type.
a character provided type.
a list is a provided type.
an association is a provided type.
a concept is a kind of association.
a thing is a kind of association.
Every thing has something called it's key and something called it's value.
An identifier is a list of characters.
A pattern is an association between a unique identifier and a regex.
A name is an association between a thing and a pattern.
A map is a collection of associations between locally unique identifiers (keys) and things (value).
Functions are a type of list that have a unique identifier.
The first item of a function is always a list of symbols (arguments).
Arguments replace symbol association for the contents of the function.


HYPOTHETICAL DOMAINS

  One of the best thigns about inform7 is placing loose constaints values that resolve at runtime.
  (The height of a man is usually 4'6 to 6'3)



  1. (X is an integral positive number.)
  Constraints are combined from boolean predicate functions, and take the place of a value.
  The example would have constraint meta data for (integer? pos?)
  2. (Y is less than X)
  Definitions can have constraint dependencies. (#(< % X))
  3. Logic solvers can unify to values, note infinite domains, or fail.
  4. Small domains could catch quieries that fall within their domain.
     (factualize (range -1000 1000) pos? odd?)
     This has a huge overhead for the relation database
  5. A constraint could provide a subset of it's values to the solver.
     (take 30 (repeatedly #(rand-int 100000)))
  6. The constraint predicates could be used to filter this subset so it lazily gives the requested
     values. For loose domains this quickly fills a subset with valid members.  Tight domains fail to
     sample solutions.
  7. Let's take numbers, number? could realize as a random quickly,
     (number? pos? #((= 0 (mod % 119674)))) would not.  For hypothetical solutions,
     larger domains are better, and could order the sampling, so we find 30 random numbers,
     15 of which are positive, and none which meet our modulo constraint.
  8. Declaring defaults for constraints only help if that default happens to unify with other constraints
     in the stack.
  9. The best solution seems to be keeping the value as a constraint stack in hopes that it will resolve.
     Small domains of factualized values can be solved first, then filtered with the predicates.


ADJECTIVES
  An adjective describes something, but what would that mean for a natural language on top of clojure?

  1. (A positive number) It defines a new type by constraining another.
  2. (The first argument) It clarifies the noun in the context.
  3. (The reversed argument) Implies a noun from transforming and the stated noun,
      if we haven't declared this operation it's hypothetical.
      this could be a clarification to a previously transformed noun.

  clojure's predicate functions (odd? sequential?) are important for this concept.

  Example adjective declaration:

  'Invert is a function that takes numerical values and multiplies each one with negative one.'
  'The adjective inverted implies two things where the first thing is equal to inverting the second thing.'

(defn invert
  ([n] (* n -1))
  ([n & more]
    (map invert (cons n more))))

(defn inverted? [a b]
  (if (= a (invert b)) true false))


  Notes: http://clojuredocs.org/clojure_core/1.3.0/clojure.core/every-pred
         http://en.wikipedia.org/wiki/Predicate_(grammar)

(verb undefine takes a string,
  if a pattern matches the string then the pattern is disjoined)


 Fizz Buzz is a function that takes a whole number called input.
  Let's say result is an empty list.
  If the input divided by 5 equals 0 append 'Fizz' to the result.
  If the input divided by 3 equals 0 append 'Buzz' to the result.
  Apply string to the result.

[{:definition
    [{:ref nil :pattern 'fizz buzz'}
     {:type fn
      :constraints
        [{:param {:symbol 'input'
                  :article :singular
                  :constraints #{:numerical :int}}]}
  {:let}]


  ")
