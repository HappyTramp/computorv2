# computorv2

Calculator REPL

## Types

| Name     | Letter | Example              |
|----------|--------|----------------------|
| Rational | Q      | `1.5`                |
| Complex  | C      | `1.5i + 1.5`         |
| Matrix   | M      | `[ [1, 2]; [3, 4] ]` |

Imaginary number are converted to Complex.

## Operations

* `+` Addition
* `-` Substraction
* `*` Multiplication
* `/` Division
* `%` Modulo
* `^` Exponent
* `**` Matrix multiplication

|   | Q                            | C                       | M                   |
|---|------------------------------|-------------------------|---------------------|
| Q | `+`, `-`, `*`, `/`, `^`, `%` | `+`, `-`, `*`, `/`, `^` | `*`                 |
| C |                              | `+`, `-`, `*`, `/`, `^` | `*`                 |
| M |                              |                         | `**`, `+`, `-`, `*` |

## Expressions

* Declaration
    * Variable
    * Function (with one parameter)
* Evaluation

### Examples

```
> a = 1 + 3
4
> f(x) = x * 2
x * 2
> a = ?
4
> f(4) = ?
16
> f(4) + a + 5 = ?
25
```

Uses eager evaluation, variable value is known after assignment, function value is reduced to the maximum (except for parameter).

```
> a = 3
3
> b = a + 3
6
> f(x) = 2 * 3 * 4 * x
24 * x
```

## TODO

- [ ] Reduce functions expression
- [ ] `i` is not a valid label name
- [ ] Check if matrix are rectangular
- [ ] Polynomial detection and resolution
- [ ] More operators

- Bonus
    - [ ] Better error message
    - [ ] Builtin functions (sin, cos, ...)
    - [ ] Add realline library
    - [ ] Command to print environment
