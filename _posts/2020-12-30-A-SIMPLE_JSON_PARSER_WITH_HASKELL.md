---
layout: post
title: A Simple JOSN Parser with Haskell
author: "José María Landa Chávez"
categories: Programming
tags: [programming,haskell]
image: A_SIMPLE_PARSER_WITH_HASKELL.png
---

**Parsing** (also called syntactic analysis) is one of the most developed branches of computer science. Parsers are used extensively in a number of disciplines: compiler construction, artificial intelligence (NLP), in linguistics and many more.

Parsing is the process of structuring a linear representation in accordance with a given **grammar**. Today we will be defining a simple grammar for the **JSON** language. 

We begin by defining the grammar for JSON:

```haskell
data JsonValue = JsonNull 
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject JsonMap
               deriving (Show, Eq)

type JsonMap = [(String, JsonValue)]
```

We will keep it simple and manage parsing errors with Maybe instead of the more appropiate choice Either.

We now define our parser type:

```haskell
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
```

You may have noticed we are using `newtype` instead of `data`. 

From the Haskell wiki:

> A `newtype` declaration creates a new type in much the same way as `data`. The syntax and usage of newtypes is virtually identical to that of data declarations - in fact, you can replace the `newtype` keyword with `data` and it'll still compile, indeed there's even a good chance your program will still work. The converse is not true, however - `data` can only be replaced with `newtype` if the type has *exactly one constructor* with *exactly one field* inside it.
> 
> **Some examples:**
> 
> ```haskell
> newtype Fd = Fd CInt
> -- data Fd = Fd CInt would also be valid
> 
> -- newtypes can have deriving clauses just like normal types
> newtype Identity a = Identity a
>   deriving (Eq, Ord, Read, Show)
> 
> -- record syntax is still allowed, but only for one field
> newtype State s a = State { runState :: s -> (s, a) }
> 
> -- this is *not* allowed:
> -- newtype Pair a b = Pair { pairFst :: a, pairSnd :: b }
> -- but this is:
> data Pair a b = Pair { pairFst :: a, pairSnd :: b }
> -- and so is this:
> newtype NPair a b = NPair (a, b)
> ```

The reason we use `newtype` instead of `data` is because the restriction to one constructor with one field means that the new type and the type of the field are in direct correspondence:

```haskell
State :: (s -> (s, a)) -> State s a
runState :: State s a -> (s -> (s, a))
```

or in mathematical terms they are **isomorphic**.

This means that after the type is checked at compile time, at run time the two types can be treated essentially the same, without the overhead or indirection normally associated with a data constructor.

Lets take a closer look at our `Parser` `newtype`

```haskell
newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }
```

The type of `Parser` is:

```haskell
Parser :: (String -> Maybe (String, a)) -> Parser a
```

That is, in order to construct a `Parser` type we need to supply a function that thakes in a string and maybe returns a tuple `(String, a))`.

Using record syntax generates a function `runParser` which 'extracts' this function from our `Parser` `newtype`.

The following is such a function:

```haskell
charP :: Char -> Parser Char
charP x = Parser f
  where  f (y:ys) = if y == x then Just (ys,x) else Nothing
         f [] = Nothing
```

Given a single character, this function creates a parser of type `Parser Char`: a parser that parses single character from a list of characters (String).

In the second line we wrap out function `f` with the type constructor `Parser` since this is what our `charP` function expects. `f` is then defined as a function of type `f :: String -> Maybe (String, Char)`, that is, it takes in a list of characters and returns a wrapped tuple consisting of the parsed character and the rest of the input.

**Example:**

```haskell
parser = charP 'h'
```

is a parser that parses the character `'h'`.

The type of `parser` is `Parser Char`.

To use our parser we must use the function `runParser` which 'unwraps' our function.

```haskell
*Main> runParser parser "hello"

Just ("ello",'h')
```

Now, what if we wanted to parse not just a single charater, but a list of characters. `charP` is quite useless in this case.

We could try mapping `charP` to a list of charactrs:

```haskell
*Main> f1 = map charP "hello"
*Main> :t f1

f1 :: [Parser Char]
```

`f1` is a list of parsers of type `Parser Char`. If we could peek inside `f1` we would see the following:

```haskell
f1 == [charP 'h', charP 'e', charP 'l', charP 'l', charP 'o']
```

If we take `head` of the list and evaluate the parser with `hello` we get the same result as before.

```haskell
*Main> parser = head f1
*Main> :t parser

parser :: Parser Char

*Main> runParser parser "hello"

Just ("ello",'h')
```

We want to somehow pipe our input `"hello"` through this list of parsers.

Let us define a piping function:

```haskell
pipe :: [a -> b] -> a -> [b]
pipe fs x =
    case fs of
      f:[] -> [f x]
      f:fs' -> f x : pipe fs' x
```

Now we can do the following:

```haskell
*Main> f2= map runParser f1
*Main> :t f2

f2:: [String -> Maybe (String, Char)]

*Main> pipe f2 "hello"

[Just ("ello",'h'),Nothing,Nothing,Nothing,Nothing]
```

Oops! Something went wrong. We only managed to parse the first `'h'`. Why?

Well, because we're not **combining** our parsers correctly, `pipe` just passes along the same input `"hello"` to all the different character parsers in `f1`. The first parser succedes in parsing `'h'`  from `"hello"` but the second parser `charP 'e'` obviously fails when trying to parse `'e'` from `"hello"`. 

It just so happens there is a brilliant function in the standard Haskell prelude called `sequenceA`. 

NOTE: In order to use `sequenceA` we must import `Control.Applicative`.

```haskell
*Main> :t sequenceA

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```

`sequenceA` takes a **Functor** type wrapped in a **Traversable** type and returns a **Functor** type wrapped in a **Traversable** type. In our case, `[]` is our Traversable type. We need only make `Parser` an instance of the Applicative**typeclass**, and we can take advantage of `sequenceA`.

Taking a look at the Applictive typeclass we can see that in order to make `Parser` an instance of Applicative it must first implement Functor.

```haskell
*Main> :info Applicative

class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
        -- Defined in `GHC.Base'
```

```haskell
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x  ) <- p input
    return $ (input', f x)                  
```

What this is basically doing is telling Hsakell how to look inside our wrapped type `Parser a`, apply a function `f` to `a` and return `Parser (f a)`. 

I highly recommend you check out [Functors, Applicatives, And Monads In Pictures]([Functors, Applicatives, And Monads In Pictures - adit.io](https://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)) to gain a deeper understanding of Functors and Monads.

**Example:**

```haskell
parser :: Parser Char -- is a parser that parses characters
parser = charP 'h'


parser' :: Parser Int -- is now a parser that parses Ints
parser' = fmap ord parser -- ord :: Char -> Int
```

Now doing `runParser parser` with `"hello"` yields ` Just ("ello",104)`.

We can now make `Parser` an instance of Applicative:

NOTE: GHC only requires us to implment `pure` and `<*>`.

```haskell
instance Applicative Parser where
  pure x = Parser $ \input -> return (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    return $ (input'', f a)
```

1. `pure` simply wraps a type with some functorial context.

2. `<*>` tells Haskell how to chain parsers together. i.e if we want to do the following: `input -> parser1 -> parser2 -> ...` Haskell would look for our implementation of `<*>` in Applicative.

Step by step:

1. The first parser `p1` returns a function f and the next string input `input'`.

2. Then we take `input'` and feed it into parser `p2` which returns `input''` and a
   
   parsed item `a`.

3. Finally we return `input''` and `f a`.

This is precisely the chaining functionality we are looking for. We can now parse Strings, sequencing char parsers. 

**Example:**

Recall that `"hello" == ['h','e','l','l','o']` thus, we can do the following: `map charP "string"` and this yields a new list of `Char` parsers.

```haskell
parsers = map charP ['h','e','l','l','o'] = 
  [charP 'h',charP 'e',charP 'l',charP 'l',charP 'o']
```

However we want a parser of Strings, that is `Parser [Char] == Parser String`. i.e we want to somehow invert our list of `Char` parsers into a parser of strings. This way we would have a parser that is capable of parsing an entire string and not just a single character.

Recall the type of `sequenceA`,

`sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)`.

Lists are Traversable and our parsers are Applicatives! Thus, we can use `sequenceA`and obtain a parser of strings from a list of parsers.

```haskell
*Main> f1 = (sequenceA . map charP) "hello"
*Main> runParser f1 $ "hello"

Just ("","hello")
```

Yay!!!

Since parsing Strings is such a common operation we define `stringP`:

```haskell
stringP :: String -> Parser String
stringP = sequenceA . map charP
```

We are now ready to parse `JsonValue` types. 

```haskell
jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"
```

`(_ -> JsonNull)` plays the role of `ord` in `ord charP 'n'`. We first parse a String `"null"` and instead of returning `Just (rest, "null")` we discard `"null"` and return `Just (rest, JsonNul)`.

```haskell
jsonBool :: Parser JsonValue
jsonBool = ...
```

Since we need now parse not only one, but two sequences of characters, we must first try to parse `"true"` and if that fails try and parse `"false"`, if that also fails, we return `Nothing`. i.e we want to combine two parsers into a single parser that tries for `"true"` and `"false"` in sequence and picks the one that is successful.

We can use the Alternative (in `Control.Applicative`) typeclass to acheive this:

```haskell
*Main> :info Alternative

class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
        -- Defined in `GHC.Base'.Base'
```

Our parser is already Applicative, so we need only implement the Alternative interface. 

```haskell
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input -- We can take advantage of the fact that
                          -- Maybe is already an instance of Alternative
```

We can now define `jsonBool` as follows:

```haskell
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        f _       = undefined
```

We will use a couple of standard functions in the implementation of `jsonNumber`, namely:

1. `read :: Read a => String -> a`
   
   1. `read` Takes a String and returns a readable type. 

2. `span :: (a -> Bool) -> [a] -> ([a], [a])`
   
   1. `span` is kind of like `takeWhile`, only it returns a pair of lists. The first list contains everything the resulting list from `takeWhile` would contain if it were called with the same predicate and the same list. The second list contains the part of the list that would have been dropped. Actually, `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`.

```haskell
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
    in return $ (rest, token) 

notNull :: Parser String -> Parser String
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs then Nothing
             else return (input', xs)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
  where f digits = JsonNumber $ read digits
```

1. `spanP` takes a  predicate and returns a `Parser String`. It works by spanning the input with the supplied predicate and returning (in the Haskell sense, i.e `Just x`)  that tuple wrapped in our `Parser` context.

2. `notNull` takes a parser and returns `Parser Nothing` if the interior value is `null`, and `Just (input', xs)` if not.

3. Finally, `jsonNumber` maps the function `f digits = JsonNumber $ read digits` (which just reads digits from characters and wraps the function in the `JsonNumber` context) to the parser `notNull (spanP isDigit)`.

We now want to parse string literals of the form `\"hello\"`.

We need to check when a string starts and when a string ends. A string starts with `"` and ends with another `"`. Everything in between is our desired string literal. 

```haskell
stringLiteral :: Parser String
stringLiteral = (charP '"' *> spanP (/= '"') <* charP '"')
```

Since we want to 







Since we want to ignore the `"` at the begninning and at the end of a string we need to first parse these and discard them, leaving us with only the string literal Parser. This can be acheived with *> and <* which are methods in applicative:



(<*) :: Applicative f => f a -> f b -> f a

(*>) :: Applicative f => f a -> f b -> f b

These function chain parsers but discard the result of the parsers.

Writing `jsonString` is straightforward

```haskell
jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral
```

**Example:**

```haskell
*Main> runParser jsonString "\"hello\""

Just ("","hello")
```

Our parser still has one flaw, observe what happens when we try to parse `"nullnullnullnull"`

```haskell
*Main> runParser jsonString "\"nullnullnullnull\""

Just ("nullnullnull",JsonNull)
```

It ignores every `null` after the first `null`. We want to continue parsing until the parser fails.
How do we do this?

There is a function defined in the Alternative type class called many:

```haskell
many :: Alternative f => f a -> f [a]
```

`many` function takes an Alternative `f a` and returns an Alternative `f [a]`.

`many` goes on (it repeatedly applies the parser) until it reaches an empty element (empty = Parser $ \_ -> Nothing)

some and many can be defined as:

```haskell
some f = (:) <$> f <*> many f
many f = some f <|> pure []
```

`some` is one or more, `many` is 0 or more results collected from performing the same computation over and over by the **maximal munch rule**. For this to make sense, some state passing (and alteration) must take place reducing the domain
of possibilities, otherwise it will result in a non exhaustive recursive pattern.

It can be helpful to see how some would be written with monadic do syntax:


```haskell
some f = do
  x <- f
  xs <- many f
  return (x:xs)
```

So `some f` runs `f` once, then `many` times, and conses the results. `many f` runs `f` `some` times,
or alternatively just returns the empty list. The idea is that they both run `f` as often as possible until it fails, collecting the results in a list.
The difference is that `some f` fails if `f` fails immediately, while `many f` will succeed and return the empty list. But what this all means exactly depends on how `<|>` is defined.


```haskell
whitespaces :: Parser String
whitespaces = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (fmap (:) element) <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray =  JsonArray <$> (charP '[' *> whitespaces *> elements <* whitespaces <* charP ']')
  where
    elements = sepBy (whitespaces *> charP ',' <* whitespaces) jsonValue 
```

1. `whitespaces` parses a series of whitespaces and returns `(rest, spaces)`
2. `sepBy` takes a separator and a parser of what we want to parse (any type of `JsonValue`) and returns a parser of lists of elements.
	1. Parser `a` parses separator elements.
	2. Parser `b` parses `JsonValue` types.

```haskell
jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> whitespaces *> object <* whitespaces <* charP '}')
  where object = sepBy (whitespaces *> charP ',' <* whitespaces) pair
        pair = (\key _ value -> (key,value)) <$> stringLiteral 
                           <*> (whitespaces *> charP ':' *> whitespaces) 
                           <*> jsonValue
```

Lastly we define a general `JsonValue` parser


```haskell
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
```

Which simply tries out all possible parsers on a given input and returns the non-failing result.


Voila! a (very basic) JSON parser.

The source code is available at [Simple-JSON-parser](https://github.com/Josemarialanda/Simple-JSON-parser).