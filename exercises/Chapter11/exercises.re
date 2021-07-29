/* Set representations */
open More;

/* The answer to Question 1 is found in the examples. */

/* Questions 2 & 4 combined */
module type SetType = {
  type t('a);
  let set_of_list: list('a) => t('a);
  let list_of_set: t('a) => list('a);
  let insert: ('a, t('a)) => t('a);
  let size: t('a) => int;
  let member: ('a, t('a)) => bool;
  let union: (t('a), t('a)) => t('a);
};

module SetList: {include SetType;} = {
  type t('a) = list('a);

  let list_of_set = x => x;

  let insert = (x, l) =>
    if (List.mem(x, l)) {
      l;
    } else {
      [x, ...l];
    };

  let rec set_of_list = l =>
    switch (l) {
    | [] => []
    | [h, ...t] => insert(h, set_of_list(t))
    };

  let size = List.length;

  let member = List.mem;

  let union = (a, b) => List.fold_left((x, y) => insert(y, x), a, b);
};

module SetTree: {include SetType;} = {
  type t('a) =
    | Lf
    | Br(t('a), 'a, t('a));

  let rec list_of_set = s =>
    switch (s) {
    | Lf => []
    | [@implicit_arity] Br(l, x, r) =>
      list_of_set(l) @ [x] @ list_of_set(r)
    };

  let rec insert = (x, s) =>
    switch (s) {
    | Lf => [@implicit_arity] Br(Lf, x, Lf)
    | [@implicit_arity] Br(l, y, r) =>
      if (x == y) {
        [@implicit_arity] Br(l, y, r);
      } else if (x < y) {
        [@implicit_arity] Br(insert(x, l), y, r);
      } else {
        [@implicit_arity] Br(l, y, insert(x, r));
      }
    };

  let rec set_of_list = l =>
    switch (l) {
    | [] => Lf
    | [h, ...t] => insert(h, set_of_list(t))
    };

  let rec size = s =>
    switch (s) {
    | Lf => 0
    | [@implicit_arity] Br(l, _, r) => 1 + size(l) + size(r)
    };

  let rec member = (x, s) =>
    switch (s) {
    | Lf => false
    | [@implicit_arity] Br(_, y, _) when x == y => true
    | [@implicit_arity] Br(l, y, r) =>
      if (x < y) {
        member(x, l);
      } else {
        member(x, r);
      }
    };

  let union = (a, b) =>
    List.fold_left((x, y) => insert(y, x), a, list_of_set(b));
};

/* And with different Br nodes */
module SetRedBlack: {include SetType;} = {
  type t('a) =
    | Lf
    | BrR(t('a), 'a, t('a))
    | BrB(t('a), 'a, t('a));

  let rec list_of_set = s =>
    switch (s) {
    | Lf => []
    | [@implicit_arity] BrR(l, x, r)
    | [@implicit_arity] BrB(l, x, r) =>
      [x, ...list_of_set(l)] @ list_of_set(r)
    };

  let balance = n =>
    switch (n) {
    | [@implicit_arity]
      BrB([@implicit_arity] BrR([@implicit_arity] BrR(a, x, b), y, c), z, d)
    | [@implicit_arity]
      BrB([@implicit_arity] BrR(a, x, [@implicit_arity] BrR(b, y, c)), z, d)
    | [@implicit_arity]
      BrB(a, x, [@implicit_arity] BrR([@implicit_arity] BrR(b, y, c), z, d))
    | [@implicit_arity]
      BrB(a, x, [@implicit_arity] BrR(b, y, [@implicit_arity] BrR(c, z, d))) =>
      [@implicit_arity]
      BrR([@implicit_arity] BrB(a, x, b), y, [@implicit_arity] BrB(c, z, d))
    | [@implicit_arity] BrR(a, b, c) => [@implicit_arity] BrR(a, b, c)
    | [@implicit_arity] BrB(a, b, c) => [@implicit_arity] BrB(a, b, c)
    | Lf => Lf
    };

  let rec add_inner = (x, s) =>
    switch (s) {
    | Lf => [@implicit_arity] BrR(Lf, x, Lf)
    | [@implicit_arity] BrR(l, y, r) =>
      if (x < y) {
        balance([@implicit_arity] BrR(add_inner(x, l), y, r));
      } else if (x > y) {
        balance([@implicit_arity] BrR(l, y, add_inner(x, r)));
      } else {
        [@implicit_arity] BrR(l, y, r);
      }
    | [@implicit_arity] BrB(l, y, r) =>
      if (x < y) {
        balance([@implicit_arity] BrB(add_inner(x, l), y, r));
      } else if (x > y) {
        balance([@implicit_arity] BrB(l, y, add_inner(x, r)));
      } else {
        [@implicit_arity] BrB(l, y, r);
      }
    };

  let insert = (x, s) =>
    switch (add_inner(x, s)) {
    | [@implicit_arity] BrR(l, y, r)
    | [@implicit_arity] BrB(l, y, r) => [@implicit_arity] BrB(l, y, r)
    | Lf => assert(false)
    };

  let rec set_of_list = l =>
    switch (l) {
    | [] => Lf
    | [h, ...t] => insert(h, set_of_list(t))
    };

  let rec size = s =>
    switch (s) {
    | Lf => 0
    | [@implicit_arity] BrR(l, _, r)
    | [@implicit_arity] BrB(l, _, r) => 1 + size(l) + size(r)
    };

  let rec member = (x, s) =>
    switch (s) {
    | Lf => false
    | [@implicit_arity] BrR(l, y, r)
    | [@implicit_arity] BrB(l, y, r) =>
      x == y
      || (
        if (x > y) {
          member(x, r);
        } else {
          member(x, l);
        }
      )
    };

  let union = (a, b) =>
    List.fold_left((x, y) => insert(y, x), a, list_of_set(b));
};

module SetHashtbl: {include SetType;} = {
  type t('a) = Hashtbl.t('a, unit);

  let list_of_set = s => Hashtbl.fold((x, (), l) => [x, ...l], s, []);

  let set_of_list = l => {
    let s = Hashtbl.create(List.length(l));
    List.iter(x => Hashtbl.add(s, x, ()), l);
    s;
  };

  let member = (x, s) => Hashtbl.mem(s, x);

  let insert = (x, s) => {
    if (!member(x, s)) {
      Hashtbl.add(s, x, ());
    };
    s;
  };

  let size = Hashtbl.length;

  let union = (a, b) => set_of_list(list_of_set(a) @ list_of_set(b));
};

/* Question 3. */
module IntSet: {
  type t;
  let set_of_list: list(int) => t;
  let list_of_set: t => list(int);
  let insert: (int, t) => t;
  let size: t => int;
  let member: (int, t) => bool;
} = {
  module S =
    Set.Make({
      type t = int;
      let compare = compare;
    });

  type t = S.t;

  let list_of_set = s => S.elements(s);

  let set_of_list = l => List.fold_right(S.add, l, S.empty);

  let member = S.mem;

  let insert = S.add;

  let size = S.cardinal;
};

let nums = Util.from(1, 50000);

let rand = Array.to_list(Array.init(50000, _ => Random.int(1073741823)));

/* Benchmark sets from sets. */
let benchmark_intset = (name, ns) => {
  let a = Unix.gettimeofday();
  let set = IntSet.set_of_list(ns);
  let b = Unix.gettimeofday();
  List.iter(x => ignore(IntSet.member(x, set)), ns);
  let c = Unix.gettimeofday();
  Printf.printf(
    "For %s, insertion took %f, membership %f\n",
    name,
    b -. a,
    c -. b,
  );
};

let benchmark = () => {
  benchmark_intset("ordered", nums);
  benchmark_intset("unordered", rand);
};

/* The memory benchmark is in the examples */
