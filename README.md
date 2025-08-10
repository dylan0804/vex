# Vex Programming Language

A delightfully unconventional programming language that speaks in whispers and shouts, where variables are just suppositions and conditions are maybe true.

## Demo

Try it here!
https://www.dylanch.pro/vex/playground

## Language Features

### Variables (or should we say, suppositions?)
```
suppose x = 42
suppose name = "world"
suppose pi = 3.14159
```

### Output with Style
- `whisper` - prints without a newline
- `shout` - prints with a newline for emphasis

```
whisper("Hello ")
shout("World!")
whisper("The answer is {}", 42)
shout("Result: {}", x + y)
```

### Conditional Logic (with uncertainty)
```
suppose age = 25

maybe age > 18 {
    shout("You're an adult!")
} perhaps age > 13 {
    shout("You're a teenager!")
} nah {
    shout("You're a kid!")
}
```

### Math Operations
All the usual suspects: `+`, `-`, `*`, `/`, with proper operator precedence and parentheses support.

```
suppose result = (10 + 5) * 2 - 3
shout("Result: {}", result)
```

### Arrays (Collections of Possibilities)
```
suppose numbers = [1, 2, 3, 4, 5]
suppose names = ["alice", "bob", "charlie"]
suppose mixed = [true, false, true]
shout("First number: {}", numbers[0])
```

### Comparisons
- `>` greater than
- `<` less than  
- `>=` greater than or equal
- `<=` less than or equal
- `==` equal

### Boolean Values
```
suppose is_ready = true
suppose is_done = false
maybe is_ready == true {
    shout("Let's go!")
}
```

## Example Programs

### Simple Calculator
```
suppose a = 15
suppose b = 7
suppose sum = a + b
shout("The sum is: {}", sum)
```

### Conditional Greetings
```
suppose hour = 14

maybe hour < 12 {
    shout("Good morning!")
} perhaps hour < 18 {
    shout("Good afternoon!")  
} nah {
    shout("Good evening!")
}
```

### Nested Conditions
```
suppose score = 85

maybe score >= 90 {
    maybe score >= 95 {
        shout("Outstanding!")
    } nah {
        shout("Excellent!")
    }
} perhaps score >= 70 {
    shout("Good job!")
} nah {
    shout("Keep trying!")
}
```

## Philosophy

Vex embraces uncertainty and playfulness in programming. Instead of definitive statements, we make suppositions. Instead of boring if-else chains, we explore maybes and perhaps. And when all else fails, we just say nah.

The language encourages a more conversational, less rigid approach to coding - perfect for those who think programming should be more human and less robotic.

## Technical Details

### Architecture
Vex follows a classic interpreter architecture with three main phases:

**1. Lexical Analysis (Tokenizer)**
- `src/lexer.rs` - Converts source code into tokens
- Handles keywords (`suppose`, `maybe`, `whisper`), operators (`+`, `==`), literals, and identifiers
- Supports string parsing, number parsing (including decimals), and error reporting

**2. Parsing (AST Generation)**
- `src/parser.rs` - Builds an Abstract Syntax Tree from tokens
- Uses recursive descent parsing with proper operator precedence
- Generates AST nodes for expressions, statements, and control structures
- AST types include:
  - `Expr`: Variables, values, arrays, binary operations
  - `Statement`: Declarations, assignments, print statements, if-else blocks
  - `Value`: Numbers, strings, booleans, arrays

**3. Interpretation (Tree Walking)**
- `src/interpreter.rs` - Executes the AST directly
- Implements variable scoping with a stack of hash maps
- Handles type checking and runtime error reporting
- Supports dynamic typing with runtime type coercion

### Language Features
- **Type System**: Dynamically typed with support for numbers (f64), strings, booleans, and arrays
- **Scoping**: Block-scoped variables with proper shadowing support
- **Error Handling**: Comprehensive error messages with context
- **Memory Model**: Garbage-collected through Rust's ownership system

## Coming Soon

We're working on loops and functions. Because even uncertain languages need to iterate through possibilities and organize their thoughts.
