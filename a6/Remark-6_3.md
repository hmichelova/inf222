# 6. 3. 7.

## Sum of two numbers
`> rundslintegerhaskell "DSLInteger-add.dipl" "35000 35000"`

`"70000"`

`> rundslintegeruint16 "DSLInteger-add.dipl" "35000 35000"`

`"4464"`

## Multiplication of two numbers
`> rundslintegerhaskell "DSLInteger-mult.dipl" "32768 2"`

`"65536"`

`> rundslintegeruint16 "DSLInteger-mult.dipl" "32768 2"`

`"0"`

## Sum of three numbers
`> rundslintegerhaskell "DSLInteger-sum3.dipl" "32768 32767 1"`

`"65536"`

`> rundslintegeruint16 "DSLInteger-sum3.dipl" "32768 32767 1"`

`"0"`

## Explanation
Results are different because Haskell's implementation of `Integer` is "unbounded", which means that (almost) in any case the result of arithmetic functions is correct. 

Haskell implementation of `Word16` has only 16 bits, which means that the result of any arithmetic operation has to be between `0` and `2 ^ 16 - 1 = 65535`. 

Results are the same iff all arithmetic operations are within bounds, e. g.


`> rundslintegerhaskell "DSLInteger-mult.dipl" "32767 2"`

`"65534"`

`> rundslintegeruint16 "DSLInteger-mult.dipl" "32767 2"`

`"65534"`

