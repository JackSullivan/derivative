
- [ ] The readme example doesn't actually work right now, we need to add checks to splice together nested operators of the same type
- [ ] The simplify methods for operators are highly redundant, and it will only get worse as more are added. We need some proper abstractions to fix that
- [ ] It would be nice to have a function that takes the result of simplify and writes out a latex string for it
- [ ] Add chain rule, inverse function rule, reciprocal rule
- [ ] support for log/e
- [ ] ability to evaluate function at certain values of variables.
- [ ] factor out the util functions properly
- [ ] support for piecewise functions (using `if` and `cond`)
- [ ] support for max and min
- [ ] first-class support for sum and product (using `reduce +` and `reduce *`)
- [ ] support for trig functions