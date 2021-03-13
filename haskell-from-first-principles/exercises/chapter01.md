# Chapter 01:

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
    