# Solved problems
- 8.3.1
- 8.4.3
- 8.4.4
- 8.4.5
- 8.4.6
- 8.4.7
- 8.4.8
- 8.4.9
- 8.4.10
- 8.4.11

# Skipped problems
- 8.3.2
- 8.3.3
- 8.3.4
- 8.3.5
- 8.3.6
- 8.3.7

# Non-code questions answares

## 8.4.7
Tests in task 8.4.6 fail for values `Int16`, but do not fail for `Integer`.
-- ze sme len konecnej mnozine cisel


The reason is, that data type `Int16` is defined on `-32768 <= x <= 32767`, therefore if the result of addiction (in test `prop_addiction`) or multiplication (in test `prop_zero_compare`) is out of these bounds then the mathematical ordering law is failed. 
On the other hand data type `Integer` is "unbounded" (it is bounded, but it is really large/small numbers), therefore the law passed. 
This law is significant mainly for mathematical theory and for the results in bounds. Programmer should choose "correct" data type for storing numbers to be sure that all the results are in bounds of data type. 

These failing laws can cause serious software volnerabilities, because the result is overflow/undeflow, which means that the code can have undefined behavior. 

In many programming languages there are multiple integer variants for storing numbers. The reason iss that somethimes it is important to have "large" integer values to be able to calculate large numbers and do not have overflow/underflow. On the other hand somethime we are sure, that input values are small enough, so it is "better" to use small data type to save memory space and make the calculation quicker. 
-- vyvoj pamati

## 8.4.8
`1 MHz = 1 000 000 ticks per second`

### 16 bit and 2 MHz
We want to make test for `2^16 * 2^16 * 2^16 = 2^48` combinations. 

`2^48 / 2 000 000 ~ 140 737 488 s ~ 1 629 days ~ 4,5 years`

### 16 bit and 5 GHz
We want to make test for `2^16 * 2^16 * 2^16 = 2^48` combinations. 

`2^48 / 5 000 000 000 ~ 56 294 s ~ 16 hours`

### 64 bit and 5 GHz
We want to make test for `2^64 * 2^64 * 2^64 = 2^192` combinations. 

`2^192 / 5 000 000 000 ~ 1.26 * 10^48 s ~ 1,46 * 10^43 days`

Doing complete test is valuable, because we calculate all possible combination of inputs and check them. So after we are "sure" (except some physical mistackes) that everything have wanted behaviur. 

On the other hand it is impossible to do complete test, becase it takes really long time only for testing. So we make only partial tests usually to check corner cases. 

## 8.4.11
### 1
Both `Int16` and `Word16` are both 16 bit representation of integers, so they are able to represent the same number of numbers. So in 8.4.9 we only use change types, before use and after use, which is means that we use numbers from the same residue class mod `2^16`. 

The problem with tests in 8.4.10 (tests failed) is that there can be overflows/underflows and modulo (number `x`) is different and not congruent to `2^16`, so the results can be different (they are not in the same residue classes). 

### 2
The reason can be, because in the changing between `Int16` and `Word16` is modulo operation and it is in the same residue class, but in the second task there is different non-congruent modulus, so it can make wrong intuition. 

Secondly, in mathematics we use infinity integer numbers, so we do not have to thing about the 16 bit modulus and then the rules are correct. 

### 3
It can be because, integer is usually signed and 32-bit integer has the maximum value almost the same as the array element and then the sum of `low` and `high` can overflow and then is the middle element wrongly calculated (in the high indexes). 

### 4
When we do not change the number of array elements, then the indexing wil be okay. It always depands on how large in array and the maximum value of integer. The maximum value has to be at least two times larger then maximum element count of array. Otherwise, there can be problems. 

# Notes
Tests are written with QuickCheck, so it is necessary to install it before use. 

All codes are prepare tu test all test cases by function `runTests`. 
