# Lispn't
Lisp written by catgirls who don't know lisp.

## Basics
Lispn't replaces list based s-expressions with curry based thunks, which are stored as compositions (`$`) of atoms (`:atom`).
For example here is the structure of `(:true :x :y)`:
```ruby
$
    $
        :true
        :x
    :y
```
This is important as every single behaviour in Lispn't is defined - in some scope - in terms of pattern matching on thunks:
```ruby
{
    :true a b = a;
    :false a b = b;
}
```
Because expressions are lazy evaluated, and behaviour definitions appear in scopes, atoms also carry where they were written so that thunk evaluation is predictable.

## Evaluation
Scopes will shadow the behaviours of their parent scopes and behaviours will be interpreted first to last.
Here is an example to deponstrate this concept:
```ruby
:f x = :third;

:g x = {
    :f (:succ (:succ x)) = :first;
    :f (:succ x) = :second;
} :f x;
```
In reality a thunk is not just a single object, but a list of successively reduced expressions. (given that thunk evaluation is context independent, this is well defined):
```ruby
$
    $
        $
            :id
            :true
        :x
    :y
# =>
$
    $
        :true
        :x
    :y
# =>
:x
```
When reducing a thunk, the callers behaviour is used to try and find a pattern to match.

> **The following behaviour is not finalised as the neuance of its implimentation has huge implications on performance:**

* If this does not succeed then the caller is successively reduced, each time a reavaulation is attempted.
* If this process fails then the argument is successivelt reduces, each time each caller is retried in order.
* If all combinations of partially reduced caller and argument fails then the reduction fails and the thunk cannot be reduced further.
