open More;

/* 1 */
let deduct = (budget, expenses) => List.fold_left((-), budget, expenses);

let deduct = List.fold_left((-));

/* 2 */
let length = l => List.fold_left((a, _) => a + 1, 0, l);

/* 3 */
let last = l =>
  switch (l) {
  | [] => None
  | _ => Some(List.fold_left((_, e) => e, List.hd(l), l))
  };

/* 4 */
let rev = l => List.fold_left((a, e) => [e, ...a], [], l);

/* 5 */
let member = (x, l) => List.fold_left((a, e) => e == x || a, false, l);

/* 6 */
let sentence = words =>
  List.fold_left(
    (a, e) =>
      if (a == "") {
        e;
      } else {
        a ++ " " ++ e;
      },
    "",
    words,
  );

/* 7 */
type tree('a) =
  | Lf
  | Br('a, tree('a), tree('a));

let rec fold_tree = (f, e, t) =>
  switch (t) {
  | Lf => e
  | [@implicit_arity] Br(x, l, r) =>
    f(x, fold_tree(f, e, l), fold_tree(f, e, r))
  };

let max_depth = l => fold_tree((_, l, r) => 1 + max(l, r), 0, l);

/* 8 */
let l = [1, 2, 3, 2, 1, 2, 2, 56, 32, 2, 34, 4, 2];

let t = Unix.gettimeofday();

let _ =
  for (x in 1 to 10_000_000) {
    ignore(member(56, l));
  };

let t' = Unix.gettimeofday();

let _ =
  for (x in 1 to 10_000_000) {
    ignore(List.mem(56, l));
  };

let t'' = Unix.gettimeofday();

let _ = {
  Printf.printf("Our member took %f seconds\n", t' -. t);
  Printf.printf("List.mem took %f seconds\n", t'' -. t');
};
