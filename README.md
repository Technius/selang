# Selang

This is a simple __S__-__E__xpression-based __Language__.

## Grammar

The grammar of this language is specified using EBNF below:

```plain
whitespace ::= { " " } ;

digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

letter ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l"
         | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x"
         | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J"
         | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V"
         | "W" | "X" | "Y" | "Z" ;

number ::= [ "-" ] digit { digit };

character ::= { letter | digit | "\'" | "\"" } ;

boolean ::= "true" | "false"

string ::= '"' { character | whitespace } '"' ;

identifier ::= letter { letter | digit } ;

atom := identifier | string | number | boolean | term

list := "(" [ atom { " " atom } ] ")"

term := atom | list
```

## Evaluation rules

TODO

```plain
```

## License

Copyright 2018 Bryan Tan

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
