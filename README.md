# swen90010-ass-3
## Intro
Full mark project of Ada calculator. If it is helpful, please give us a star. While refering to this assignment, make sure you follow the regulation from the Unimelb:

```
By submitting work for assessment I hereby declare that I understand the University’s policy on academic integrity and statement on the use of artificial intelligence software. In accordance with these documents, I declare that the work submitted is original and solely my work, and that I have not been assisted by a third party (collusion) apart from where the submitted work is for a designated collaborative task, in which case the individual contributions are indicated. I also declare that I have not used any writing tools or sources without proper acknowledgment (plagiarism). Where the submitted work is a computer program or code, I further declare that any copied code is declared in comments identifying the source at the start of the program or in a header file, that comments inline identify the start and end of the copied code, and that any modifications to code sources elsewhere are commented upon as to the nature of the modification.
```

## Description
The program you have to implement is a command-line utility for performing numerical calculations. It takes input from the terminal (i.e. standard input, aka stdin).

### Commands
Each line of input is a command. Commands conform to the following grammar:
```
<COMMAND> ::= “+”
“-”
“*”
“/”
“push” <NAME>
“pop”
“load” <NAME>
“store” <NAME>
“remove” <NAME>
“list”
“unlock” <NUMBER>
“lock” <NUMBER>
```

Tokens in the grammar above are separated by one or more whitespace characters, namely space,
tab, and line-feed. Each <NAME> is a string of non-whitespace characters. <NUMBER> is a
4-digit string of non-whitespace characters that represents a non-negative number (i.e. a natural
number) in the range 0000 . . . 9999.

- The calculator can be in one of two states, either locked or unlocked.
- When the user starts the calculator, they supply (via a command-line argument) a 4-digit
string masterpin that represents a 4-digit PIN (i.e. a number in the range 0000 . . . 9999),
which is the master PIN needed to unlock the calculator. If no master PIN is supplied,
the calculator should exit immediately.
- The calculator begins in the locked state.
- For a string pin that represents a 4-digit PIN, the command “unlock pin” does nothing
when the calculator is in the unlocked state. Otherwise, it checks whether pin is equal to
the master PIN and, if so, changes the state of the calculator to unlocked. If pin is not
equal to the master PIN, then the state of the calculator is not changed.
- For a string newpin that represents a 4-digit PIN, the command “lock newpin” does nothing
when the calculator is in the locked state. Otherwise, it updates the master PIN to become
newpin and changes the state of the calculator to locked.
- For a string num representing a decimal integer, e.g. “5”, the command “push num” pushes
the value represented by num onto operand stack.
- The command “pop” pops the value from the top of the operand stack, discarding it.
- The commands “+”, “-”, “*” and “/” each pop the top two values from the operand stack
and compute the corresponding arithmetic operation on them (addition, subtraction, multiplication and division, respectively), and push the result onto the stack.
- For a string var, the command “load var ” loads the value stored in variable var and pushes
it onto the stack.
- For a string var, the command “store var ” pops the value from the top of the stack and
stores it into variable var, defining that variable if it is not already defined.
- The command “list” prints out all currently defined variables and their corresponding values.
- For a string var, the command “remove var ” makes variable var undefined (i.e. it will not
be printed by subsequent “list” commands).


