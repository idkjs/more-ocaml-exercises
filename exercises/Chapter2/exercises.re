type lazylist('a) =
  | Cons('a, unit => lazylist('a));

/* 1 */
let rec ldouble = n => [@implicit_arity] Cons(n, () => ldouble(n * 2));

let thedoubles = ldouble(1);

/* 2 */
let rec lnth = ([@implicit_arity] Cons(h, tf), n) =>
  switch (n) {
  | 0 => h
  | _ => lnth(tf(), n - 1)
  };

/* 3 */
let rec lrepeating_inner = (c, l) =>
  switch (c) {
  | [] => raise(Invalid_argument("lrepeating: empty list"))
  | [x] => [@implicit_arity] Cons(x, () => lrepeating_inner(l, l))
  | [h, ...t] => [@implicit_arity] Cons(h, () => lrepeating_inner(t, l))
  };

let lrepeating = l => lrepeating_inner(l, l);

/* 4 */
let rec fibonacci_inner = (x, y) =>
  [@implicit_arity] Cons(x, () => fibonacci_inner(y, x + y));

let fibonacci = fibonacci_inner(0, 1);

/* 5 */
let rec unleave = ([@implicit_arity] Cons(h, tf)) => {
  let [@implicit_arity] Cons(h', tf') = tf();
  let t = tf'();
  (
    [@implicit_arity] Cons(h, () => fst(unleave(t))),
    [@implicit_arity] Cons(h', () => snd(unleave(t))),
  );
};

/* 6 */
let rec letter_string = n =>
  if (n <= 26) {
    Char.escaped(char_of_int(n + 64));
  } else {
    letter_string((n - 1) / 26) ++ letter_string((n - 1) mod 26 + 1);
  };

let rec lseq = n => [@implicit_arity] Cons(n, () => lseq(n + 1));

let rec lmap = (f, [@implicit_arity] Cons(h, tf)) =>
  [@implicit_arity] Cons(f(h), () => lmap(f, tf()));

let alphas = lmap(letter_string, lseq(1));
