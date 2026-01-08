What I did
- Read the full project you provided (Main, Shell, DSLParser, DSLInterpreter, Computer, TerminalUI).
- Mapped responsibilities across classes and traced how expressions, variables, file I/O, REPL and UI are wired together.
- Identified correctness bugs, structural/architectural weak points, error/edge cases, and places where a C#-idiomatic refactor will improve maintainability, performance and extensibility.
- Prepared a prioritized plan and a set of C# class skeletons (lexer/parser/AST/evaluator/symbol table/exceptions) you can use as the basis for a structural refactor instead of a line-by-line translation.

Below is a detailed System Design Review with recommendations prioritized for maintainability and correctness.

1) High-level summary / primary problems
- Responsibilities are mixed. Computer blends evaluation, variable storage and parsing helpers; DSLInterpreter mixes persistence, filesystem parsing and the REPL behavior; DSLParser is a token extractor, not a proper parser.
- Parsing is fragile and incomplete. The current "compute" method splits on whitespace only, ignores parentheses, operator precedence, unary minus, implicit multiplication, and expects every binary operator to have two operands ready — it will crash on malformed input.
- Variable parsing/persistence is brittle and buggy (parseVarName/parseVariable). The text-based variables.txt approach is unstructured and parsing of it is ad-hoc.
- Error handling is sparse: NumberFormatException messages are printed and then code proceeds; many places pop from stacks without checking availability — leads to runtime exceptions.
- Inconsistent math semantics: trig functions mix degrees and radians conversions inconsistently (sin/cos/tan: convert operand degrees -> radians; asin/acos/atan: convert return radians -> degrees). No configuration for radians vs degrees.
- Global mutable static state (variables HashMap) makes reasoning/testability difficult and prevents safe concurrency.
- UI/console code is coupled to interpreter logic in multiple places (TerminalUI calls Shell.processInput, Shell has static Scanner/state, DSLInterpreter does I/O).

2) Architectural bottlenecks and where the design limits extensibility or performance
- No proper lexer/parser/AST: compute is a single-line evaluator using token split + stacks; this prevents supporting:
  - Parentheses and grouping
  - Operator precedence and associativity
  - Custom operators or easy extension to new functions
  - Compiling expressions to efficient delegates (re-evaluation cost per expression remains high)
- Computer class mixes variable storage and evaluation: prevents injecting alternate symbol tables or persistence layers (hard to write tests or support multiple sessions).
- File format for variables is plain text without structure; reading requires scanning whole file and fragile string parsing.
- Re-evaluation performance: every compute rebuilds state and runs stacks each time. With an AST + compilation you can cache parsed forms and compile them to delegates (System.Linq.Expressions) for much faster repeated evaluation.
- Error propagation: errors are printed to System.out in many places and evaluation continues. That makes CLI/web integration and automated testing awkward.
- Factorial implemented via recursion on int — for large inputs this will stack overflow; also casted from double to int without validation (silently truncates).

3) C#-idiomatic improvements (patterns and features to use)
- Use a clean separation: Lexer -> Parser -> AST -> Evaluator. Map to these C# artifacts:
  - Tokenizer/Lexer: yields tokens (number, identifier, operator, parentheses).
  - Parser: recursive-descent or Pratt parser (recommended) implementing precedence.
  - AST nodes: immutable classes (Expression base, BinaryExpression, UnaryExpression, LiteralExpression, VariableExpression, FunctionCallExpression).
  - Evaluator: walks AST and returns double; accept a symbol lookup delegate or an ISymbolTable interface for DI.
- Use System.Linq.Expressions to optionally compile AST to an Expression<Func<IDictionary<string,double>, double>> or a delegate for faster repeated evaluation. This lets you JIT compile evaluation for heavy workloads.
- Use LINQ for collection operations and transforming token lists.
- Use strong typed exceptions (ParseException, EvaluationException) and propagate them — don’t print inside library code.
- Use built-in parsing helpers: double.TryParse with CultureInfo.InvariantCulture.
- Use IDictionary<string,double> or a custom ISymbolTable rather than static global Dictionary. For concurrency consider ConcurrentDictionary or lock inside symbol table.
- Use properties and dependency injection instead of static singletons (improves testability).
- For UI separation, expose the interpreter as a library (methods that accept strings and return results or exceptions), and let TerminalUI/Console/GUI layer handle presentation.

4) Error handling & edge cases (where logic fails today)
- Tokenization and parsing:
  - compute uses input.split("\\s+"): expressions like "2+2", "(3+4)*5", or "sin(45)" will fail.
  - No handling for parentheses or nested function calls -> cannot parse common algebraic expressions.
  - Missing operand checks: popping operands without verifying stack size -> EmptyStackException.
  - No check for malformed tokens (unknown identifiers) — variables.get(token) can return null -> NullPointer when used with % 1 or arithmetic.
- Numeric parsing:
  - stringToNumber prints "ERROR: NaN && aN" and returns 0.0 on error — silent failure.
  - Uses default locale when parsing — in C# use invariant culture to avoid decimal comma confusion.
- Math edge cases:
  - Division by zero: binaryOperator "/" returns a/b with no guard -> produces Infinity/NaN, no user-friendly error.
  - Factorial: cast operand to int and compute factorial recursively. This:
    - Truncates non-integers silently.
    - Accepts negative numbers by throwing IllegalArgumentException (but that’s thrown inside factorial; compute wraps no handling).
    - Recursion causes stack overflow for large numbers; integer overflow not handled.
    - Factorial returns int, then is returned as double due to cast path, causing inconsistent types.
  - Log and sqrt of negative numbers produce NaN; no error messaging.
  - Inverse trig range and domain errors are not checked (e.g., asin argument outside [-1,1]).
  - Trig function semantics: ambiguous degrees vs radians; inconsistent conversions in code.
- Variable parsing & persistence:
  - parseVarName only extracts one non-digit character (bug), so names longer than one char break.
  - parseVariable uses substring(1) and splits by whitespace, assumes variable assignment is of a specific format; brittle.
  - readFile converts parsed numeric string to integer via Integer.parseInt without try/catch; will crash for doubles.
  - In general the persistence format is ad-hoc and fragile for many edge cases.
- REPL/UI gaps:
  - Shell.shell() does not call DSLParser/DSLInterpreter — commented code. The REPL loop as shipped will read input and do nothing.
  - TerminalUI tries to call Shell.processInput which delegates to DSLParser.parserManager -> DSLParser.parser -> DSLInterpreter.detect -> computer/IO. But error handling is inconsistent and the UI can get uncaught exceptions.

5) What to keep (robust parts to port directly)
- The conceptual command set is fine: commands like cp (compute), let/store/get/clear, parse, graph, quit. Keep the overall DSL command design.
- The set of classic functions (+, -, *, /, ^, %, sin, cos, tan, asin, acos, atan, log, ln, sqrt, abs, factorial) is a good starting function list.
- The unit-level arithmetic implementations (add, binaryOperator switch cases, unaryOperator mapping) are the basis for function mapping — but should be refactored into a data-driven registry (map string -> delegate) rather than long switch statements.
- The desire to persist variables across sessions is reasonable — but store them in a robust structured format (JSON, CSV with proper escaping, or name=value per line with strict parsing).
- The TerminalUI Swing code is usable conceptually; in C# you’d replace it with a WinForms/WPF console-like UI (or keep Console) and have the interpreter be a pure library.

6) Concrete recommendations and prioritized plan (start here)
Priority 1 — Parsing & AST (fix correctness first)
- Implement a clear lexer that recognizes numbers (support floats, exponent notation), identifiers, parentheses, operators and commas.
- Implement a parser (I recommend Pratt parsing or recursive descent with precedence) to produce an immutable AST. Support:
  - Parentheses
  - Function calls with parentheses and multiple arguments (future-proof)
  - Unary operators (negation, factorial)
  - Operator precedence and associativity (including right-associative ^)
- Provide helpful ParseException with location/index and message.

Priority 2 — Evaluation and symbol table
- Implement an Evaluator that walks AST and returns double (or a richer Value type if you want multiple types later).
- Use an ISymbolTable interface for variables:
  - double? TryGet(string name)
  - void Set(string name, double value)
  - bool Remove(string name)
  - IEnumerable<KeyValuePair<string,double>> Enumerate() for persistence.
- Keep evaluator pure: do not write to console or files — let callers handle I/O. Return EvaluationResult or throw EvaluationException.

Priority 3 — Error handling & domain checks
- Validate operand counts before popping.
- For each function/operator add domain checks and throw EvaluationException with helpful message:
  - Division: check b == 0
  - Factorial: only non-negative integers — or implement Gamma for non-integers if desired; disallow large inputs or return appropriate error.
  - Log/ln: x > 0
  - Sqrt: x >= 0 (or decide on complex numbers)
  - asin/acos: x in [-1,1]
- Use double.IsFinite to check for NaN/Infinity and throw exceptions.
- Use culture-invariant parsing for numbers (CultureInfo.InvariantCulture).

Priority 4 — C# idioms & performance improvements
- Provide two evaluation paths:
  - Simple interpreter (walk AST).
  - Compiled delegate via System.Linq.Expressions for hot expressions; cache compiled delegates keyed by normalized expression string.
- Use Dictionary<string, Func<double,double>> or similar to map function names to delegates (easy to extend).
- Use exceptions (ParseException, EvaluationException) instead of printing — the UI will catch and print user messages.
- Make core library code not use static state; create an Interpreter object that holds symbol table and function map.

Priority 5 — Persistence & UI separation
- Replace variables.txt with structured persistence:
  - JSON dictionary (System.Text.Json) is straightforward and robust.
  - Or use INI-like name=value with strict parsing and validation.
- Expose an API to export/import symbol table.
- Keep TerminalUI / Console UI as thin adapters that call interpreter methods and handle formatting & printing.

7) C# class layout suggestion (API surface)
- Namespace: DosLib (or Davidos)
- Core types:
  - Token (enum TokenType, Value, Position)
  - Lexer
  - Parser -> Expression (Expression base + derived nodes)
  - IEvaluator or Evaluator class (Evaluate(Expression expr, ISymbolTable symbols))
  - ISymbolTable + InMemorySymbolTable (implements IDictionary semantics)
  - FunctionRegistry (maps string -> delegate)
  - Interpreter (public method Evaluate(string expression) that does lex+parse+eval)
  - Exceptions: ParseException, EvaluationException
- Utilities: ExpressionCompiler (converts AST to System.Linq.Expressions and compiles delegate), ParserOptions (angleMode degrees/radians), EvaluationOptions (overflow behavior).

8) Implementation details and tips
- Parser: implement precedence table (examples):
  - Highest: function call, parenthesis, literal, variable
  - Unary: +, - (prefix), factorial (!) (postfix)
  - Exponentiation ^ (right-assoc)
  - *, /, %
  - +, -
- Tokenization: accept identifiers [A-Za-z_][A-Za-z0-9_]*. Allow multi-char function names (sin, asin).
- Factorial: treat as postfix operator with high precedence (e.g., 5!).
- Variable naming & assignment: add a 'let' or 'varname = expression' parser if you want inline assignments (or keep commands).
- For persistent state: save a simple JSON file variable -> value (double).
- Use unit tests for:
  - Parsing bracket nesting, precedence
  - Error conditions (missing paren, insufficient operands)
  - Numeric edge cases (0 division, large factorial)
  - Variable set/get and persistence

9) Bugs & code smells found (line-level / function-level)
- Computer.compute:
  - Splitting on whitespace prevents expressions without spaces (e.g., "2+2").
  - Binary ops are applied immediately ignoring precedence; this is effectively postfix/RPN-style but input isn't RPN. This will produce wrong results for normal infix expressions.
  - No checks before popping operands (EmptyStackException).
  - isVariable uses variables.get(token)!=null; if variable value is 0.0 this still returns valid, but using get may return null if missing — okay but usage later assumes non-null.
- Computer.unaryOperator:
  - Inconsistent degree/radian handling (see earlier).
  - Default prints "ERROR: BINARY OPERATOR DOES NOT EXIST" which is misleading for unaryOperator.
- DSLInterpreter.parseVarName:
  - The method enqueues all non-digits but only returns variable.remove() (first character). So it returns only the first non-digit char and throws if queue empty. This is a bug. If variable name is "x", works; but names longer than one character fail.
- DSLInterpreter.parseVariable:
  - substrings the subject (subject.substring(1)) in an ad-hoc way — brittle.
  - Loops tokens and sets result to last numeric token (overwriting previous ones) — unclear intent.
- DSLParser.getCommand/tokenize:
  - Using command_token_index logic is confusing; sometimes index constants differ. Better to split input on whitespace and pick [0] as command and the rest joined as the subject.
- Shell.shell:
  - The loop reads input but does not process it; code that calls detect() is commented out. Shell.processInput is provided but shell() doesn't use it.
- TerminalUI:
  - Calls Shell.processInput(command) which returns output string; but processInput uses DSLParser which may write to stdout rather than returning consistent strings.

10) Suggested C# file skeletons to start (minimal, core pieces)
Below are a few skeleton files you can use as starting points for the structural refactor. These are intentionally minimal but idiomatic.

```csharp name=Token.cs
namespace Davidos.Dsl
{
    public enum TokenType { Number, Identifier, Plus, Minus, Star, Slash, Percent, Caret, Bang, LParen, RParen, Comma, End, Unknown }

    public sealed class Token
    {
        public TokenType Type { get; }
        public string Text { get; }
        public int Position { get; }

        public Token(TokenType type, string text, int position)
        {
            Type = type;
            Text = text;
            Position = position;
        }

        public override string ToString() => $"{Type}:'{Text}'@{Position}";
    }
}
```

```csharp name=Lexer.cs
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace Davidos.Dsl
{
    public class Lexer
    {
        private readonly string _text;
        private int _pos;

        public Lexer(string text) { _text = text ?? ""; _pos = 0; }

        public IEnumerable<Token> Lex()
        {
            while (_pos < _text.Length)
            {
                var ch = _text[_pos];
                if (char.IsWhiteSpace(ch)) { _pos++; continue; }
                if (char.IsDigit(ch) || ch == '.') { yield return ReadNumber(); continue; }
                if (char.IsLetter(ch) || ch == '_') { yield return ReadIdentifier(); continue; }
                switch (ch)
                {
                    case '+': yield return New(TokenType.Plus); break;
                    case '-': yield return New(TokenType.Minus); break;
                    case '*': yield return New(TokenType.Star); break;
                    case '/': yield return New(TokenType.Slash); break;
                    case '%': yield return New(TokenType.Percent); break;
                    case '^': yield return New(TokenType.Caret); break;
                    case '!': yield return New(TokenType.Bang); break;
                    case '(' : yield return New(TokenType.LParen); break;
                    case ')' : yield return New(TokenType.RParen); break;
                    case ',' : yield return New(TokenType.Comma); break;
                    default:
                        yield return New(TokenType.Unknown);
                        break;
                }
                _pos++;
            }
            yield return new Token(TokenType.End, string.Empty, _pos);
        }

        private Token New(TokenType t) => new Token(t, _text[_pos].ToString(), _pos);

        private Token ReadNumber()
        {
            var start = _pos;
            while (_pos < _text.Length && (char.IsDigit(_text[_pos]) || _text[_pos]=='.' || _text[_pos]=='e' || _text[_pos]=='E' || _text[_pos]=='+' || _text[_pos]=='-'))
            {
                // simple number scanning — refine for correctness (exponent handling)
                _pos++;
            }
            var txt = _text.Substring(start, _pos - start);
            return new Token(TokenType.Number, txt, start);
        }

        private Token ReadIdentifier()
        {
            var start = _pos;
            while (_pos < _text.Length && (char.IsLetterOrDigit(_text[_pos]) || _text[_pos]=='_')) _pos++;
            var txt = _text.Substring(start, _pos - start);
            return new Token(TokenType.Identifier, txt, start);
        }
    }
}
```

```csharp name=Expressions.cs
using System;
using System.Collections.Generic;

namespace Davidos.Dsl
{
    public abstract class Expression { }

    public sealed class LiteralExpression : Expression
    {
        public double Value { get; }
        public LiteralExpression(double value) => Value = value;
    }

    public sealed class VariableExpression : Expression
    {
        public string Name { get; }
        public VariableExpression(string name) => Name = name;
    }

    public sealed class UnaryExpression : Expression
    {
        public string Operator { get; }
        public Expression Operand { get; }
        public UnaryExpression(string op, Expression operand) { Operator = op; Operand = operand; }
    }

    public sealed class BinaryExpression : Expression
    {
        public string Operator { get; }
        public Expression Left { get; }
        public Expression Right { get; }
        public BinaryExpression(string op, Expression left, Expression right) { Operator = op; Left = left; Right = right; }
    }

    public sealed class FunctionCallExpression : Expression
    {
        public string Name { get; }
        public IReadOnlyList<Expression> Arguments { get; }
        public FunctionCallExpression(string name, IReadOnlyList<Expression> args) { Name = name; Arguments = args; }
    }
}
```

```csharp name=IEvaluator.cs
using System;
using System.Collections.Generic;

namespace Davidos.Dsl
{
    public interface ISymbolTable
    {
        bool TryGet(string name, out double value);
        void Set(string name, double value);
        bool Remove(string name);
        void Clear();
        IReadOnlyDictionary<string,double> Snapshot();
    }

    public class InMemorySymbolTable : ISymbolTable
    {
        private readonly Dictionary<string,double> _dict = new Dictionary<string,double>(StringComparer.OrdinalIgnoreCase);
        public bool TryGet(string name, out double value) => _dict.TryGetValue(name, out value);
        public void Set(string name, double value) => _dict[name] = value;
        public bool Remove(string name) => _dict.Remove(name);
        public void Clear() => _dict.Clear();
        public IReadOnlyDictionary<string,double> Snapshot() => new Dictionary<string,double>(_dict);
    }

    public class EvaluationException : Exception { public EvaluationException(string m):base(m){} }
}
```

```csharp name=Evaluator.cs
using System;
using System.Collections.Generic;
using System.Globalization;

namespace Davidos.Dsl
{
    public class Evaluator
    {
        private readonly ISymbolTable _symbols;
        private readonly Dictionary<string, Func<double[], double>> _functions;

        public Evaluator(ISymbolTable symbols)
        {
            _symbols = symbols;
            _functions = DefaultFunctions();
        }

        public double Evaluate(Expression expr)
        {
            return expr switch
            {
                LiteralExpression lit => lit.Value,
                VariableExpression v => _symbols.TryGet(v.Name, out var val) ? val : throw new EvaluationException($"Unknown variable '{v.Name}'"),
                UnaryExpression u => EvaluateUnary(u),
                BinaryExpression b => EvaluateBinary(b),
                FunctionCallExpression f => EvaluateFunction(f),
                _ => throw new EvaluationException("Unsupported expression type")
            };
        }

        private double EvaluateUnary(UnaryExpression u)
        {
            var val = Evaluate(u.Operand);
            return u.Operator switch
            {
                "-" => -val,
                "+" => +val,
                "!" => Factorial(val),
                _ => throw new EvaluationException($"Unknown unary operator {u.Operator}")
            };
        }

        private double EvaluateBinary(BinaryExpression b)
        {
            var left = Evaluate(b.Left);
            var right = Evaluate(b.Right);
            return b.Operator switch
            {
                "+" => left + right,
                "-" => left - right,
                "*" => left * right,
                "/" => right == 0 ? throw new EvaluationException("Division by zero") : left / right,
                "%" => right == 0 ? throw new EvaluationException("Modulo by zero") : left % right,
                "^" => Math.Pow(left, right),
                _ => throw new EvaluationException($"Unknown binary operator {b.Operator}")
            };
        }

        private double EvaluateFunction(FunctionCallExpression f)
        {
            if (!_functions.TryGetValue(f.Name.ToLowerInvariant(), out var del))
                throw new EvaluationException($"Unknown function '{f.Name}'");

            var args = new double[f.Arguments.Count];
            for (int i=0;i<args.Length;i++) args[i] = Evaluate(f.Arguments[i]);
            return del(args);
        }

        private static double Factorial(double d)
        {
            if (double.IsNaN(d) || double.IsInfinity(d)) throw new EvaluationException("Invalid operand for factorial");
            if (d < 0) throw new EvaluationException("Factorial is not defined for negative numbers.");
            if (Math.Abs(d - Math.Round(d)) > 0) throw new EvaluationException("Factorial is defined for integers only in this implementation.");
            var n = (int)Math.Round(d);
            long acc = 1;
            for (int i=2;i<=n;i++)
            {
                acc = checked(acc * i); // may throw OverflowException for large n
            }
            return acc;
        }

        private static Dictionary<string, Func<double[],double>> DefaultFunctions()
        {
            return new Dictionary<string, Func<double[],double>>(StringComparer.OrdinalIgnoreCase)
            {
                ["sin"] = args => Math.Sin(args[0]),
                ["cos"] = args => Math.Cos(args[0]),
                ["tan"] = args => Math.Tan(args[0]),
                ["asin"] = args => Math.Asin(args[0]),
                ["acos"] = args => Math.Acos(args[0]),
                ["atan"] = args => Math.Atan(args[0]),
                ["sqrt"] = args => args[0] < 0 ? throw new EvaluationException("sqrt domain error") : Math.Sqrt(args[0]),
                ["log"] = args => args[0] <= 0 ? throw new EvaluationException("log domain error") : Math.Log10(args[0]),
                ["ln"] = args => args[0] <= 0 ? throw new EvaluationException("ln domain error") : Math.Log(args[0]),
                ["abs"] = args => Math.Abs(args[0]),
            };
        }
    }
}
```

Notes on the skeletons:
- The evaluator skeleton uses radians by default for trig; make an option to use degrees (or wrap functions that convert degrees to radians).
- The factorial uses checked integer multiplication to surface overflow. Consider BigInteger if you expect huge values or return an error for n > 20 to avoid overflow in double.

11) Testing & validation
- Add unit tests for:
  - Lexer: numbers, identifiers, punctuation
  - Parser: operator precedence, parentheses, unary/postfix
  - Evaluator: arithmetic, division by zero, domain errors, variable resolution
  - Persistence: save/restore symbol table
- Use continuous integration (GitHub Actions) to run tests on PRs.

12) Migration strategy (how to port incrementally)
- Step 1: Create the new C# library project (DosLib or Davidos.Dsl) and implement Lexer, Parser, AST, Evaluator and tests.
- Step 2: Replace the Java Computer.compute usage: build a C# Interpreter.Evaluate(expression) method that returns double or throws parse/eval exceptions.
- Step 3: Implement a SymbolTable and persistence (JSON).
- Step 4: Port the Shell/REPL and TerminalUI to call the new interpreter. Keep UI thin and handle exceptions with friendly messages.
- Step 5: Add optional compilation layer (Expression trees) for performance and implement caching of compiled delegates for frequent expressions.
- Step 6: Add more features (graphing, file parsing) only after stable core is in place.

13) Final prioritized checklist (short)
- [ ] Implement Lexer & robust Parser (parentheses & precedence).
- [ ] Replace compute with AST-evaluator; remove ad-hoc stack logic.
- [ ] Replace static global variable storage with ISymbolTable.
- [ ] Add structured persistence (JSON).
- [ ] Replace switch statements for functions with a registry/dictionary.
- [ ] Add ParseException/EvaluationException types and propagate errors.
- [ ] Normalize trig semantics and make degrees/radians configurable.
- [ ] Add unit tests for parsing/evaluation/persistence.
- [ ] Optionally implement Expression compilation + caching for speed.

If you’d like, I can:
- Provide a complete Parser implementation (recursive-descent or Pratt) in C# that handles precedence, parentheses, function calls and unary/postfix operators.
- Produce a minimal working C# console REPL that uses the new library and demonstrates variable persistence and expression evaluation.
- Produce unit tests (xUnit or NUnit) covering the edge cases above.

Which of the next steps do you want me to do now? (I can produce the C# parser + evaluator next, or a translation plan with more detailed API signatures, or a set of unit tests.)
