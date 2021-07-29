/* Set representations */
open More;

module type SetType = {
  type t('a);
  let set_of_list: list('a) => t('a);
  let list_of_set: t('a) => list('a);
  let insert: ('a, t('a)) => t('a);
  let size: t('a) => int;
  let member: ('a, t('a)) => bool;
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
};

module SetRedBlack: {include SetType;} = {
  type colour =
    | R
    | B;

  type t('a) =
    | Lf
    | Br(colour, t('a), 'a, t('a));

  let rec list_of_set = s =>
    switch (s) {
    | Lf => []
    | [@implicit_arity] Br(_, l, x, r) =>
      [x, ...list_of_set(l)] @ list_of_set(r)
    };

  let balance = t =>
    switch (t) {
    | (
        B,
        [@implicit_arity] Br(R, [@implicit_arity] Br(R, a, x, b), y, c),
        z,
        d,
      )
    | (
        B,
        [@implicit_arity] Br(R, a, x, [@implicit_arity] Br(R, b, y, c)),
        z,
        d,
      )
    | (
        B,
        a,
        x,
        [@implicit_arity] Br(R, [@implicit_arity] Br(R, b, y, c), z, d),
      )
    | (
        B,
        a,
        x,
        [@implicit_arity] Br(R, b, y, [@implicit_arity] Br(R, c, z, d)),
      ) =>
      [@implicit_arity]
      Br(
        R,
        [@implicit_arity] Br(B, a, x, b),
        y,
        [@implicit_arity] Br(B, c, z, d),
      )
    | (a, b, c, d) => [@implicit_arity] Br(a, b, c, d)
    };

  let rec insert_inner = (x, s) =>
    switch (s) {
    | Lf => [@implicit_arity] Br(R, Lf, x, Lf)
    | [@implicit_arity] Br(c, l, y, r) =>
      if (x < y) {
        balance((c, insert_inner(x, l), y, r));
      } else if (x > y) {
        balance((c, l, y, insert_inner(x, r)));
      } else {
        [@implicit_arity] Br(c, l, y, r);
      }
    };

  let insert = (x, s) =>
    switch (insert_inner(x, s)) {
    | [@implicit_arity] Br(_, l, y, r) => [@implicit_arity] Br(B, l, y, r)
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
    | [@implicit_arity] Br(_, l, _, r) => 1 + size(l) + size(r)
    };

  let rec member = (x, s) =>
    switch (s) {
    | Lf => false
    | [@implicit_arity] Br(_, l, y, r) =>
      x == y
      || (
        if (x > y) {
          member(x, r);
        } else {
          member(x, l);
        }
      )
    };
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
};

/* Numbers from 1 to 50000 */
let nums = Util.from(1, 50000);

/* 50000 pseudorandom numbers between 0 and 1073741823 */
let rand = Array.to_list(Array.init(50000, _ => Random.int(1073741823)));

/* Build modules for each using first class modules syntax not discussed in the
 * text. */
let implementations = [
  ("Lists", (module SetList): (module SetType)),
  ("Trees", (module SetTree): (module SetType)),
  ("Red-black trees", (module SetRedBlack): (module SetType)),
  ("Hash tables", (module SetHashtbl): (module SetType)),
];

/* Insert items into a set */
let insertion_benchmark = (str, l) =>
  List.iter(
    ((n, s)) => {
      module S = (val s: SetType);
      let t_start = Unix.gettimeofday();
      let _ = S.set_of_list(l);
      let t_end = Unix.gettimeofday();
      Printf.printf(
        "Insertion of 50000 %s elements with %s took %f seconds\n%!",
        str,
        n,
        t_end -. t_start,
      );
    },
    implementations,
  );

/* Test membership of all items in a set, once populated with them */
let membership_benchmark = (str, l) =>
  List.iter(
    ((n, s)) => {
      module S = (val s: SetType);
      let set = S.set_of_list(l);
      let t_start = Unix.gettimeofday();
      List.iter(x => ignore(S.member(x, set)), nums);
      let t_end = Unix.gettimeofday();
      Printf.printf(
        "Membership in a set made from %s items with %s took %f seconds\n%!",
        str,
        n,
        t_end -. t_start,
      );
    },
    implementations,
  );

/* Return a list of all items in a set */
let elements_benchmark = (str, l) =>
  List.iter(
    ((n, s)) => {
      module S = (val s: SetType);
      let set = S.set_of_list(l);
      let t_start = Unix.gettimeofday();
      ignore(S.list_of_set(set));
      let t_end = Unix.gettimeofday();
      Printf.printf(
        "Elements of a set made from %s items with %s took %f seconds\n%!",
        str,
        n,
        t_end -. t_start,
      );
    },
    implementations,
  );

/* Find the size of a set */
let size_benchmark = (str, l) =>
  List.iter(
    ((n, s)) => {
      module S = (val s: SetType);
      let set = S.set_of_list(l);
      let t_start = Unix.gettimeofday();
      ignore(S.size(set));
      let t_end = Unix.gettimeofday();
      Printf.printf(
        "Size of a set made from %s items with %s took %f seconds\n%!",
        str,
        n,
        t_end -. t_start,
      );
    },
    implementations,
  );

/* Find how much memory is used by inserting 50000 random elements into a set
 * in each set representation */
let memory_benchmark = l =>
  List.iter(
    ((n, s)) => {
      module S = (val s: SetType);
      let (min, prom, maj) = Gc.counters();
      let _ = S.set_of_list(l);
      let (min2, prom2, maj2) = Gc.counters();
      Printf.printf(
        "Memory used creating set made from %s items is %f \n%!",
        n,
        min2 +. maj2 -. prom2 -. (min +. maj -. prom),
      );
    },
    implementations,
  );

/*let _ =
  insertion_benchmark "ordered" nums;
  print_newline ();
  insertion_benchmark "unordered" rand;
  print_newline ();
  membership_benchmark "ordered" nums;
  print_newline ();
  membership_benchmark "unordered" rand;
  print_newline ();
  elements_benchmark "ordered" nums;
  print_newline ();
  elements_benchmark "unordered" rand;
  print_newline ();
  size_benchmark "ordered" nums;
  print_newline ();
  size_benchmark "unordered" rand;
  print_newline ();
  memory_benchmark nums;
  memory_benchmark rand */
