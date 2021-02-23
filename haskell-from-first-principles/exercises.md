# Exercises

The following are my answers and solutions to the questions and problems posed in the chapter exercises.

## Chapter 01:

### Equivalence: We'll give you a lambda expression. Keeping in mind both alpha equivalence and how multiple heads are nested, choose an answer that is equivalent to the listed lambda term:

1. *&lambda;xy.xz*
   * b: *&lambda;mn.mz*
2. *&lambda;xy.xxy*
   * c: *&lambda;a.(&lambda;b.aab)*
3. *&lambda;xyz.zx*
   * b: *&lambda;tos.st*

### Combinators: Determine whether each of the following functions are combinators or not:

1. *&lambda;x.xxx*
   * yes
2. *&lambda;xy.zx*
   * no
3. *&lambda;xyz.xy(zx)*
   * yes
4. *&lambda;xyz.xy(zxy)*
   * yes
5. *&lambda;xy.xy(zxy)*
   * no

### Normal form or diverge?: Determine whether each of the following expressions can be reduced to a normal form or if they diverge:

1. *&lambda;x.xxx*
   * normal form
2. *(&lambda;z.zz)(&lambda;y.yy)*
   * diverges: *(&lambda;z.zz)(&lambda;y.yy) &rArr; (&lambda;y.yy)(&lambda;y.yy) ...*
3. *(&lambda;x.xxx)z*
   * normal form: *(&lambda;x.xxx)z &rArr; zzz* 

### Beta reduce: Evaluate (that is, beta reduce) each of the following expressions to normal form.

1. *(&lambda;abc.cba)zz(&lambda;wv.w)*
   * &rArr; *z*
2. *(&lambda;x.&lambda;y.xyy)(&lambda;a.a)b*
   * &rArr; *bb*
3. *(&lambda;y.y)(&lambda;x.xx)(&lambda;z.zq)*
   * &rArr; *qq*
4. *(&lambda;z.z)(&lambda;z.zz)(&lambda;z.zy)*
   * &rArr; *yy*
5. *(&lambda;x.&lambda;y.xyy)(&lambda;y.y)y*
   * &rArr; *yy*
6. *(&lambda;a.aa)(&lambda;b.ba)c*
   * &rArr; *aac*
7. *(&lambda;xyz.xz(yz))(&lambda;x.z)(&lambda;x.a)*
   * &rArr; *&lambda;z1.za*

## Chapter 02:

### Heal the sick: The following code samples are broken and won't compile. The first two are as you might enter them into the REPL. The third is from a source file. Find the mistakes, and fix them so that the code will compile:

1. `area x = 3. 14 * (x * x)`
   * `area x = 3.14 * (x * x)` -- Removed space from `3. 14`
2. `double x = b * 2`
   * `double x = x * 2` -- Variable `b` is not defined. Renamed to `x`.
3. Code snippet below, followed by correction: indentation is not consistent.

```haskell
-- Original
x = 7
 y = 10
f = x + y

-- Corrected
x = 7
y = 10
f = x + y
```

### A head code: Determine in your head what the following expressions will return. These examples are prefixed with `let`, because they are not declarations, they are expressions:

1. `let x = 5 in x`
   * 5
2. `let x = 5 in x * x`
   * 25
3. `let x = 5; y = 6 in x * y`
   * 30
4. `let x = 3; y = 1000 in x + 3`
   * 6
    
### Rewrite these `let` expressions with `where` clauses:

1. `let x = 3; y = 1000 in x * 3 + y`
2. `let y = 10, x = 10 * 5 + y in x * 5`
3. `let x = 7; y = negate x; z = y * 10 in z / x + y`

```haskell
rewrite1  = x * 3 + y
  where x = 3
        y = 1000


rewrite2  = x * 5
  where y = 10
        x = 10 * 5 + y


rewrite3  = z / x + y
  where x = 7
        y = negate x
        z = y * 10
```

### Parenthesization: Parenthesize the following expressions more explicitly without changing their results:

1. `2 + 2 * 3 - 1`
   * `2 + (2 * 3) - 1`
2. `(^) 10 $ 1 + 1`
   * `(^) 10 (1 + 1)`
3. `2 ^ 2 * 4 ^ 5 + 1`
   * `((2 ^ 2) * (4 ^ 5)) + 1`

### Equivalent expressions: Which of the following pairs of expressions will return the same result when evaluated?

1. `1 + 1`, `2`
2. `10 ^ 2`, `10 + 9 * 10`
3. `400 - 37`, `(-) 37 400`
4. ```100 `div` 3```, `100 / 3`
5. `2 * 5 + 18`, `2 * (5 + 18)`

* Answer: 1, 2

### More fun with functions: Given a bit of code as it might be entered into a source file, rewrite it such that it could be evaluated in the REPL:

```haskell
z = 7


x = y ^ 2


waxOn = x * 5


y = z + 8
```

* Answer: `waxOn = let z = 7; y = z + 8; x = y ^ 2 in x * 5`

### Rewrite `waxOn` as an expression with a `where` clause:

```haskell
waxOn     = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2
```
