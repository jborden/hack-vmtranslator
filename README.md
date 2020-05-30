# hack-vmtranslator

A translator of hack bytecode to hack assembly for the hack programming language written in clojure

Written as part of the https://www.nand2tetris.org/ course

## Usage

$ lein repl
```clojure
hack-vmtranslator.core> (translate-file "/Users/james/nand2tetris/projects/07/StackArithmetic/SimpleAdd/SimpleAdd.vm")
nil
```

will output the file "SimpleAdd.asm" in the same directory as the asm file

## License

Copyright © 2020 James Borden

MIT License
