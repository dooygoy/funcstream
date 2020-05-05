# CIS 194: Homework 1

When solving the homework, strive to create not just code that works, but code that is stylish and concise. Try to write small functions which perform just a single task, and then combine those smaller pieces to create more complex functions. Don't repeat yourself: write one function for each logical task, and reuse functions as necessary.


## Validating Credit Card Numbers

How do websites validate your credit card number when you shop online? They don't check a massive database of numbers, and they don't use magic. In fact, most credit providers rely on a checksum formula for distinguishing valid numbers from random collections of digits.

> What is a checksum?

Wiki: [Checksum](https://en.wikipedia.org/wiki/Checksum)

A **checksum** is a small-sized datum derived from a block of digital data for the purpose of detecting errors that may have been introduced during its transmission or storage. By themselves, checksums are often used to verify data integrity but are not relied upon to verify data authenticity. The procedure which generates this checksum is called a **checksum function** or checksum algorithm. Depending on its design goals, a good checksum algorithm will usually output a significantly different value, even for small changes made to the input. 

In this section, you will implement the validation algorithm for credit cards. It follows these steps:

* Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled; the third-to-last digit is unchanged; and so on. For example,
`[1,3,8,6]` becomes [2,3,16,6] 

* Add the digits of the doubled values and the undoubled digits from the original number. For example, `[2,3,16,6]` becomes `2+3+1+6+6 = 18`

:confused:

> Note: I do not understand how is this calculation done.

How am I supposed to add `[2,3,16,6]` and `[1,3,8,6]`

> Note: So 16 is not a digit?

[Numerical digit](https://en.wikipedia.org/wiki/Numerical_digit)

A **numerical digit** is a single symbol (such as "2" or "5") used alone, or in combinations (such as "25"), to represent numbers (such as the number 25) according to some positional numeral systems. The single digits (as one-digit-numerals) and their combinations (such as "25") are the numerals of the numeral system they belong to. For example,the decimal system (base 10) requires ten digits (0 through to 9), whereas the binary system (base2) has two digits (e.g.: 0 and 1)

> Note: add the digits of the doubled values and the undoubled digits from the original number.

so `[2,3,16,6]` = `2 + 3 + 1 + 6 + 6 = 18`
and `[1,3,8,6]` = `1 + 3 + 8 + 6 = 18`

oh wow now I see, this is cool, never seen this one before. 

* calculate the remainder when the sum is divided by 10. 
`rem 18 10 => 8`

**Exercise 1** We need to first find the digits of a number. Define the functions.

```haskell
toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

-- toDigits should convert positive Integers to a list of digits.
-- for 0 or negative inputs, toDigits should return the empty list.
-- toDigitsRev should do the same, but with the digits reversed.
```
> Note: So it is about the positional system, we somehow need to decompose a decimal number into its digits, aka it's tenths, hundreds?, thousands,.. example: a number 12345 into 5, 45, 345, 2345.. at least that is the first idea. Later we have to somehow add single digits, for now be begin to extract the last digit.

```haskell
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)
-- this will give us the last digit, or the 10th column
-- for example to get 100ths or 1000ths we would done
lastDigit 1234 
4

lastTwoDigits = (`mod` 100)
lastTwoDigits 1234
34

-- or 
lastThreeDigits = (`mod` 1000)
lastThreeDigits 1234
234 

-- also
divMod 1234 10
(123,4)

-- now check this magic

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

dropLastDigit 1234
123
```

So our dropLastDigit and lastDigit are like basic operators on lists!
Usual haskell tutorials mention these but too bad there are no examples like the credit card because here we have extraction of digits from a single number which at first seems like a difficult problem. What are our list operators? `init` and..`last`


-- have to put tea :)
-- gosh, still have to learn vim..

```haskell
head [1,2,3,4]
5

tail [1,2,3,4]
[2,3,4]

last [1,2,3,4]
4

init [1,2,3,4]
[1,2,3]
```
Next we have to write a function that splits a number into its digits in reverse order called, `toDigitsRev`

```haskell
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lastDigit n : toDigitsRev (dropLastDigit n)
```

> But how to just get digits of some number in Haskell?

[stack overflow question](https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell)

`135 `div` 10 = 13`
`135 `mod` 10 = 5`

generalize into a function:

```haskell
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- and reverse

toRevDigits :: Integral n => n -> [n]
toRevDigits 0 = []
toRevDigits n = n `mod` 10 : toRevDigits (n `div` 10)
```
* [ ] - Check more interesting answers on stack on this question.

> Note: Notice lexi-lambdas solution is more elegant since the functions are already abstracted. Before beginning to just solve the function provided here we see two smaller functions composed into one. 

Next lexi-lambda observes:

> Sure enough, that fixed things. Still, something about me doesn't like the asymmetry between the two expressions on either side of the cons (is it still called that in Haskell?). I remember something about `where` clauses; let's try that.

```haskell
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = d : toRevDigits ds
     where d  = lastDigit n
           ds = dropLastDigit n
```

> Well, that seemed to work, but is it nicer...? I don't know. Now it just seems more verbose, honestly. I can't really decide..

End of **Exercise 1**

(break) -- gotta do lunch..
