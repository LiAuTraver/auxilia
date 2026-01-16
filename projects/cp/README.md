# cp

Implementations and experiments related to the `Compiler Principles` course at my univ.

- [Lexer](./include/Lexing.hpp): simple lexer to tokenize source. I copied my old lexer and tweaked it a bit.
- [AutomataBase](./include/AutomataBase.hpp): mixin class for `NFA` and `DFA`.
- [NFA](./include/NFA.hpp): Nondeterministic Finite Automaton. input regex. determinization via McNaughton-Yamada-Thompson algorithm.
- [DFA](./include/DFA.hpp): Deterministic Finite Automaton. Minimization via Hopcroft algorithm or plain Moore construction. todo: add Brzozowski algorithm.
- [Grammar](./include/Grammar.hpp): able to parse simple phrase and sentences up to LL(1); stores productions, first/follow sets, parse table.
