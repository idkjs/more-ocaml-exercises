let rec interleave = (e, seen, l) =>
  switch (l) {
  | [] => [seen @ [e]]
  | [x, ...xs] => [seen @ [e, x, ...xs], ...interleave(e, seen @ [x], xs)]
  };

let combine = (x, ps) => List.concat(List.map(interleave(x, []), ps));

let rec perms = p =>
  switch (p) {
  | [] => [[]]
  | [h, ...t] => combine(h, perms(t))
  };

/* Tail recursive version */
let rec interleave = (acc, e, seen, l) =>
  switch (l) {
  | [] => [seen @ [e], ...acc]
  | [x, ...xs] =>
    interleave([seen @ [e, x, ...xs], ...acc], e, seen @ [x], xs)
  };

let combine = (x, ps) => List.concat(List.map(interleave([], x, []), ps));

let rec perms = p =>
  switch (p) {
  | [] => [[]]
  | [h, ...t] => combine(h, perms(t))
  };

/* Another method. pick each element out, and use it as the first element. Easy with sets: */
let rec without = (x, l) =>
  switch (l) {
  | [] => []
  | [h, ...t] when h == x => t
  | [h, ...t] => [h, ...without(x, t)]
  };

let rec perms = l =>
  switch (l) {
  | [] => [[]]
  | l =>
    List.concat(
      List.map(x => List.map(l => [x, ...l], perms(without(x, l))), l),
    )
  };

/* Version which can give the next permutation -- lexicographic order */
let first = arr => {
  let f = ref(Array.length(arr) - 1);
  for (x in 0 to Array.length(arr) - 2) {
    if (arr[x] < arr[x + 1]) {
      f := x;
    };
  };
  f^;
};

let last = (arr, f) => {
  let c = ref(-1);
  for (x in Array.length(arr) - 1 downto f + 1) {
    if (arr[x] > arr[f] && (c^ == (-1) || arr[x] < arr[c^])) {
      c := x;
    };
  };
  c^;
};

let swap = (arr, a, b) => {
  let t = arr[a];
  arr[a] = arr[b];
  arr[b] = t;
};

let sort_subarray = (arr, o, l) => {
  let sub = Array.sub(arr, o, l);
  Array.sort(compare, sub);
  Array.blit(sub, 0, arr, o, l);
};

let next_permutation = arr_in => {
  let arr = Array.copy(arr_in);
  let f = first(arr);
  let c = last(arr, f);
  swap(arr, f, c);
  sort_subarray(arr, f + 1, Array.length(arr) - 1 - f);
  arr;
};

let non_increasing = arr =>
  Array.length(arr) <= 1
  || {
    let r = ref(true);
    for (x in 0 to Array.length(arr) - 2) {
      if (arr[x + 1] > arr[x]) {
        r := false;
      };
    };
    r^;
  };

let all_permutations = arr => {
  let copy = Array.copy(arr);
  Array.sort(compare, copy);
  let perm = ref(copy);
  let perms = ref([copy]);
  while (!non_increasing(perm^)) {
    perm := next_permutation(perm^);
    perms := [perm^, ...perms^];
  };
  Array.of_list(List.rev(perms^));
};

/* Lazy method from next perm one. */
type lazylist('a) =
  | Cons('a, unit => lazylist('a));

let rec perms = x =>
  [@implicit_arity]
  Cons(
    x,
    () =>
      if (non_increasing(x)) {
        let c = Array.copy(x);
        Array.sort(compare, c);
        perms(c);
      } else {
        perms(next_permutation(x));
      },
  );
