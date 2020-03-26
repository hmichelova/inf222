# 6. 4. 4. 

## Trivial test cases
### String
`> rundslsortstring "DSLSorting-algorithm.dipl" "[]"`

`"[]"`
\newline

`> rundslsortstring "DSLSorting-algorithm.dipl" "[\"a\",\"b\"]"`

`"[\"a\",\"b\"]"`
\newline

`> rundslsortstring "DSLSorting-algorithm.dipl" "[\"b\",\"a\"]"`

`"[\"a\",\"b\"]"`
\newline

`> rundslsortstring "DSLSorting-algorithm.dipl"`

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`"[\"a\",\"d\",\"c\",\"b\",\"2\",\"!\"]"`

`"[\"!\",\"2\",\"a\",\"b\",\"c\",\"d\"]"`
\newline

### Integer
`> rundslsortinteger "DSLSorting-algorithm.dipl" "[]"`

`"[]"`
\newline

`> rundslsortinteger "DSLSorting-algorithm.dipl" "[1,2,3,4,5,6]"`

`"[1,2,3,4,5,6]"`
\newline

`> rundslsortinteger "DSLSorting-algorithm.dipl" "[4,3,1,0]"`

`"[0,1,3,4]"`
\pagebreak

## Non-trivial test cases
### String
`> rundslsortstring "DSLSorting-algorithm.dipl"`

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`"[\"DIPL\",\"is\",\"a\",\"language\",\"framework\",\"for\",\"DSLs\"]"`

`"[\"DIPL\",\"DSLs\",\"a\",\"for\",\"framework\",\"is\",\"language\"]"`
\newline

`> rundslsortstring "DSLSorting-algorithm.dipl" "[\"inf222\",\"inf\",\"222\"]"`

`"[\"222\",\"inf\",\"inf222\"]"`
\newline

`> rundslsortstring "DSLSorting-algorithm.dipl"`

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`"[\"42\",\"2\",\"4\",\"21\",\"69\",\"-1\"]"`

`"[\"-1\",\"2\",\"21\",\"4\",\"42\",\"69\"]"`
\newline

### Integer
`> rundslsortinteger "DSLSorting-algorithm.dipl" "[24,67,12,345,1,0,345]"`

`"[0,1,12,24,67,345,345]"`
\newline

`> rundslsortinteger "DSLSorting-algorithm.dipl"`

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`"[1,2,5,65536,42,21,2,65537,46,44,1,0]"`

`"[0,1,1,2,2,5,21,42,44,46,65536,65537]"`
\newline

`> rundslsortinteger "DSLSorting-algorithm.dipl"`

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`"[-1,1,0,123456789,-123456789]"`

`"[-123456789,-1,0,1,123456789]"`

