---
title       : "C++ Modify In-Place Operators"
date        : "2017-01-19"
author      : "Luke Smith"
description : "Recreating C++ operators in R."
tags        : [r, rcpp, c++]
---





I recently started the [Rcpp chapter](http://adv-r.had.co.nz/Rcpp.html) in Hadley Wickham's Advanced R. C++ has some similarities with R (`sprintf()` and `if else` syntax both come to mind) - afterall R is written in both native R and C, and C++ extends C with "classes". Of course, there are many differences between C++ and R, such as scope, class, member functions, type declaration, etc.

However, C++ has one thing that R currently does not have and is something that I believe can be implemented in R: the modify in-place operators:

C++ Modify In-Place Operator | Use         | Translation
-----------------------------|-------------|---------------
`+=`                         | `x += x[i]` | `x = x + x[i]`
`-=`                         | `x -= x[i]` | `x = x - x[i]`
`*=`                         | `x *= x[i]` | `x = x * x[i]`
`/=`                         | `x /= x[i]` | `x = x / x[i]`

As a "fun" activity, I decided to write R-equivalents for these operators.

## A simple function
Essentially, these C++ operators can be expressed as special infix operators in R: functions wrapped in `%%` and used in between two R objects.  I decided to use `magrittr`'s compound assignment pipe-operator, `%<>%`, as a basis for these new operators - though, it's probably not necessary. 

In order to reassign the new value to the original `lhs` argument's name, I decided to use `assign()`.

Here is my first-take:


```r
# Load magrittr
library(magrittr)

# Write the function
`%+=%` <- function(lhs, rhs) {
    name <- substitute(lhs)
    lhs %<>% `+`(rhs)
    assign(as.character(name), lhs, envir = parent.frame())
}
```

## An efficient function
### A factory
Extending the above example to the remaining operators would require writing a function for each operator. Using a function factory, a factory (closure) for making new functions, would be a far more efficient approach:


```r
# Write a function factory
make_compound_incrementer <- function(fun) {
  fun <- match.fun(fun)
  function(lhs, rhs) {
    name <- substitute(lhs)
    lhs %<>% fun(rhs)
    assign(as.character(name), lhs, envir = parent.frame())
    }
}
```

Now we're talking.

### And it's products
It is now just a matter of calling the function with the appropriate operator and then assigning that output to an appropriate name:


```r
# Create individual functions using the factory
`%+=%` <- make_compound_incrementer("+")
`%-=%` <- make_compound_incrementer("-")
`%*=%` <- make_compound_incrementer("*")
`%/=%` <- make_compound_incrementer("/")
```

## Examples
### A simple `rhs`
Let's try them out.


```r
# Use x
x <- 1:5

# %+=%
x %+=% 1
x %+=% 1
x %+=% 1
x
```

```
## [1] 4 5 6 7 8
```

```r
# %-=%
x %-=% 1
x %-=% 1
x %-=% 1
x
```

```
## [1] 1 2 3 4 5
```

```r
# %*=%
x %*=% 2
x %*=% 2
x %*=% 2
x
```

```
## [1]  8 16 24 32 40
```

```r
# %/=%
x %/=% 2
x %/=% 2
x %/=% 2
x
```

```
## [1] 1 2 3 4 5
```

### A more verbose `rhs`
More complex `rhs` expressions can be written with the use of a lambda expression (chains wrapped in `{}`). The following is just an example of such a chain, though not very useful in practice:


```r
x <- 1:5

# The rhs (wrapped in {}) is a lambda expression with its own chain
x %+=% {
  x[2] %>%
    seq(., length.out = 5, by = .) %>%
    mean()
}
x
```

```
## [1]  7  8  9 10 11
```

## Possible uses
### Something that works
The only real possible use for such operators would be the same use for such operators in C++: control flow.


```r
x <- 1:5
out <- rep(1, 5)

# A very simple for loop that works
for (i in seq_along(x)) {
  out %*=% x[i]
}

out
```

```
## [1] 120 120 120 120 120
```

### Something that does not work
If the `lhs` in the above `for` loop is subsetted, we get both a warning message and an undesired result:


```r
x <- 1:5
out <- rep(1, 5)

# A for loop which produces undesired results
for (i in seq_along(x)) {
  out[i] %*=% x[i]
}
```

```
## Warning in assign(as.character(name), lhs, envir = parent.frame()): only
## the first element is used as variable name

## Warning in assign(as.character(name), lhs, envir = parent.frame()): only
## the first element is used as variable name

## Warning in assign(as.character(name), lhs, envir = parent.frame()): only
## the first element is used as variable name

## Warning in assign(as.character(name), lhs, envir = parent.frame()): only
## the first element is used as variable name

## Warning in assign(as.character(name), lhs, envir = parent.frame()): only
## the first element is used as variable name
```

```r
out
```

```
## [1] 1 1 1 1 1
```

To fix this feature, off the top of my head, the infix operators would require some new steps:

  1. Parsing the parse tree of the `lhs` for a subsetting function.
  2. Evaluating `i` and `lhs[i]`.
  3. Assigning the new value back into `lhs[i]`

However, that's more than I want to cover and it's a good stopping point.
