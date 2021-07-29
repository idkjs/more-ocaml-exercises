/* Tail-recursive lists */
module List: {include (module type of List);} = {
  include List;

  let map = (f, l) => List.rev(List.rev_map(f, l));

  let append = (a, b) => List.rev_append(List.rev(a), b);

  let (@) = append;

  let map = (f, l) => List.rev(List.rev_map(f, l));

  let rev_map2 = (f, l, l2) => {
    let rec rev_map_inner = (acc, a, b) =>
      switch (a, b) {
      | ([], []) => acc
      | ([x, ...xs], [y, ...ys]) =>
        rev_map_inner([f(x, y), ...acc], xs, ys)
      | _ => raise(Invalid_argument("List.map2"))
      };

    rev_map_inner([], l, l2);
  };

  let map2 = (f, l, l2) => List.rev(rev_map2(f, l, l2));

  let concat = lists => {
    let rec concat = (out, acc) =>
      switch (acc) {
      | [] => out
      | [l, ...ls] => concat(append(l, out), ls)
      };

    concat([], List.rev(lists));
  };

  let fold_right = (f, l, e) =>
    List.fold_left((x, y) => f(y, x), e, List.rev(l));
};

/* Utilities */
module Util: {
  let from: (int, int) => list(int);
  let take: (list('a), int) => list('a);
  let drop: (list('a), int) => list('a);
} = {
  let from = (s, e) =>
    if (e < s) {
      raise(Invalid_argument("from"));
    } else {
      let n = ref([]);
      for (x in s to e) {
        n := [x, ...n^];
      };
      List.rev(n^);
    };

  let take = (l, n) =>
    if (n < 0) {
      raise(Invalid_argument("take"));
    } else {
      let rec take_inner = (r, l, n) =>
        if (n == 0) {
          List.rev(r);
        } else {
          switch (l) {
          | [] => raise(Invalid_argument("take"))
          | [h, ...t] => take_inner([h, ...r], t, n - 1)
          };
        };

      take_inner([], l, n);
    };

  let drop = (l, n) => {
    let rec drop_inner = (n, l) =>
      switch (l) {
      | [] => raise(Invalid_argument("drop"))
      | [_, ...t] =>
        if (n == 1) {
          t;
        } else {
          drop_inner(n - 1, t);
        }
      };

    if (n < 0) {
      raise(Invalid_argument("drop"));
    } else if (n == 0) {
      l;
    } else {
      drop_inner(n, l);
    };
  };
};
