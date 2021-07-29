open More;

/* 1. Generate all the combinations. */
let rec combinations = l =>
  switch (l) {
  | [] => [[]]
  | [h, ...t] =>
    let cs = combinations(t);
    List.map(x => [h, ...x], cs) @ cs;
  };

/* 2. Generate all the permi-combinations. */

/* Basic method, from the chapter. */
let rec interleave = (h, l, ls) =>
  switch (ls) {
  | [] => [l @ [h]]
  | [x, ...xs] => [l @ [h, x, ...xs], ...interleave(h, l @ [x], xs)]
  };

let combine = (x, ps) => List.concat(List.map(interleave(x, []), ps));

let rec perms = p =>
  switch (p) {
  | [] => [[]]
  | [h, ...t] => combine(h, perms(t))
  };

/* Now, our function: */
let permicombinations = l => List.concat(List.map(perms, combinations(l)));

/* 3. All possible lists of length n of true and false. */
let rec bool_lists = n =>
  switch (n) {
  | 0 => [[]]
  | _ =>
    let ls = bool_lists(n - 1);
    List.map(l => [true, ...l], ls) @ List.map(l => [false, ...l], ls);
  };

/* 4. Reverse the subarray a..b in array arr. */
let swap = (arr, a, b) => {
  let t = arr[a];
  arr[a] = arr[b];
  arr[b] = t;
};

let array_rev = (a, o, l) =>
  for (x in 0 to l / 2 - 1) {
    swap(a, o + x, o + l - x - 1);
  };

/* 5. Imperative algorithm in terms of lists */

/* Return (before, first, after) of a non-null list */
let rec first_inner = (before, l) =>
  switch (l) {
  | [] => raise(Invalid_argument("first_inner"))
  | [x] => (List.rev(before), x, [])
  | [a, b, ...t] =>
    if (b < a) {
      (List.rev(t), b, [a, ...before]);
    } else {
      first_inner([a, ...before], [b, ...t]);
    }
  };

let first = l => first_inner([], List.rev(l));

/* Return (before, ceiling, rest) of a non-null list */
let rec split_at_inner = (before, n, l) =>
  switch (l) {
  | [] => (List.rev(before), [])
  | [h, ...t] =>
    if (h == n) {
      (List.rev(before), t);
    } else {
      split_at_inner([h, ...before], n, t);
    }
  };

let split_at = (n, l) => split_at_inner([], n, l);

let last = (f, l) =>
  switch (List.filter(x => x > f, List.sort(compare, l))) {
  | [] => raise(Invalid_argument("last"))
  | [h, ...t] =>
    let (before, after) = split_at(h, l);
    (before, h, after);
  };

/* Find the next permutation */
let next_permutation = l => {
  let (before_f, f, after_f) = first(l);
  let (before_c, c, after_c) = last(f, after_f);
  before_f @ [c] @ List.rev(before_c @ [f] @ after_c);
};

/* Predicate on non-increasingness */
let rec non_increasing = l =>
  switch (l) {
  | []
  | [_] => true
  | [a, b, ...t] => a >= b && non_increasing([b, ...t])
  };

/* The main function */
let rec all_permutations_inner = (a, l) =>
  if (non_increasing(l)) {
    List.rev(a);
  } else {
    let next = next_permutation(l);
    all_permutations_inner([next, ...a], next);
  };

let all_permutations = l => [l, ...all_permutations_inner([], l)];
