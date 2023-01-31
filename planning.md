
```lisp
(defun 'average (args...)
 (/ (+ (... args)) / (lccdr args)))

(defun 'average
 (/ (+ (arglist)) / (lccdr arglist)))

(average 1 2 3)

(let (args) (unpack (1 2 3))
 (/ (+ (... args)) / (lccdr args))
)
```

```js
atom   <- PLUS / MINUS / MULT / DIV / MOD
        / FN / LET / MACR / EVAL / CURRY
        / GLOBAL / 
        / ATOM / SEXPR
        / EXPANDS
        / PRINTABLE
        / PRINT / PRINTLN
        / BUILTIN
        / HEAD / TAIL / INS / REM / PUSH / POP
        / NUMBER
        / CHAR

builtin <- LCBRACK { /* Yoink, count occurrences of '}'. */ } RCBRACK 

sexpr  <- atom
        / builtin
        / STRING
        / (HASH / EXCLAIM)? OPEN sexpr* CLOSE
```

```c
typedef struct {
  Sexpr** items;
  char* atomname;
  uint32_t len;
  uint32_t cap;
  Sexpr* call(Sexpr* arglist);
  bool expands;
  bool kind;
  bool printable
} Sexpr;

bool isatom(Sexpr* sex) {
  return !!sex->atomname;
}

bool issexpr(Sexpr* sex) {
  return !!sex->buf;
}
```
