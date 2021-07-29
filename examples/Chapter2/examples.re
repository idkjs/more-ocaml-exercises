type lazylist('a) =
  | Cons('a, unit => lazylist('a));

let rec lseq = n => [@implicit_arity] Cons(n, () => lseq(n + 1));

let lhd = ([@implicit_arity] Cons(n, _)) => n;

let ltl = ([@implicit_arity] Cons(_, tf)) => tf();

let rec ltake = ([@implicit_arity] Cons(h, tf), n) =>
  switch (n) {
  | 0 => []
  | _ => [h, ...ltake(tf(), n - 1)]
  };

let rec ldrop = ([@implicit_arity] Cons(h, tf) as ll, n) =>
  switch (n) {
  | 0 => ll
  | _ => ldrop(tf(), n - 1)
  };

let rec lmap = (f, [@implicit_arity] Cons(h, tf)) =>
  [@implicit_arity] Cons(f(h), () => lmap(f, tf()));

let rec lfilter = (f, [@implicit_arity] Cons(h, tf)) =>
  if (f(h)) {
    [@implicit_arity] Cons(h, () => lfilter(f, tf()));
  } else {
    lfilter(f, tf());
  };

let cubes = lfilter(x => x mod 5 == 0, lmap(x => x * x * x, lseq(1)));

let rec mkprimes = ([@implicit_arity] Cons(h, tf)) =>
  [@implicit_arity]
  Cons(h, () => mkprimes(lfilter(x => x mod h != 0, tf())));

let primes = mkprimes(lseq(2));

let rec interleave = ([@implicit_arity] Cons(h, tf), l) =>
  [@implicit_arity] Cons(h, () => interleave(l, tf()));

let rec lconst = n => [@implicit_arity] Cons(n, () => lconst(n));

let interleaved = interleave(lconst(0), lconst(1));

let rec allfrom = l =>
  [@implicit_arity]
  Cons(l, () => interleave(allfrom([0, ...l]), allfrom([1, ...l])));

let allones = allfrom([]);
