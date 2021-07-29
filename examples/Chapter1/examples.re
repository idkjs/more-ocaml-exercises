let rec fold_left = (f, a, l) =>
  switch (l) {
  | [] => a
  | [h, ...t] => fold_left(f, f(a, h), t)
  };

let rec fold_right = (f, l, a) =>
  switch (l) {
  | [] => a
  | [h, ...t] => f(h, fold_right(f, t, a))
  };

let sum = l => List.fold_left((+), 0, l);

let maxlist = l => List.fold_left(max, min_int, l);

let all = l => List.fold_left((&&), true, l);

let any = l => List.fold_left((||), false, l);

let setify = l =>
  List.fold_left(
    (a, e) =>
      if (List.mem(e, a)) {
        a;
      } else {
        [e, ...a];
      },
    [],
    l,
  );

let map = (f, l) => List.fold_right((e, a) => [f(e), ...a], l, []);

let fold_right = (f, l, e) =>
  List.fold_left((x, y) => f(y, x), e, List.rev(l));

let copy = l => List.fold_right((e, a) => [e, ...a], l, []);

let append = (x, y) => List.fold_right((e, a) => [e, ...a], x, y);

let split = l =>
  List.fold_right(
    ((x, y), (xs, ys)) => ([x, ...xs], [y, ...ys]),
    l,
    ([], []),
  );

let concat = l => List.fold_left((@), [], l);

type tree('a) =
  | Lf
  | Br('a, tree('a), tree('a));

let rec fold_tree = (f, e, t) =>
  switch (t) {
  | Lf => e
  | [@implicit_arity] Br(x, l, r) =>
    f(x, fold_tree(f, e, l), fold_tree(f, e, r))
  };

let example =
  [@implicit_arity]
  Br(
    1,
    [@implicit_arity] Br(0, Lf, Lf),
    [@implicit_arity] Br(6, [@implicit_arity] Br(4, Lf, Lf), Lf),
  );

let tree_size = t => fold_tree((_, l, r) => 1 + l + r, 0, t);

let tree_sum = t => fold_tree((x, l, r) => x + l + r, 0, t);

let tree_preorder = t => fold_tree((x, l, r) => [x] @ l @ r, [], t);

let tree_inorder = t => fold_tree((x, l, r) => l @ [x] @ r, [], t);

let tree_postorder = t => fold_tree((x, l, r) => l @ r @ [x], [], t);
