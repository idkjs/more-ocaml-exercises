open More;
open Input;
open Bits;

let white_terminating_codes = [|
  [0, 0, 1, 1, 0, 1, 0, 1],
  [0, 0, 0, 1, 1, 1],
  [0, 1, 1, 1],
  [1, 0, 0, 0],
  [1, 0, 1, 1],
  [1, 1, 0, 0],
  [1, 1, 1, 0],
  [1, 1, 1, 1],
  [1, 0, 0, 1, 1],
  [1, 0, 1, 0, 0],
  [0, 0, 1, 1, 1],
  [0, 1, 0, 0, 0],
  [0, 0, 1, 0, 0, 0],
  [0, 0, 0, 0, 1, 1],
  [1, 1, 0, 1, 0, 0],
  [1, 1, 0, 1, 0, 1],
  [1, 0, 1, 0, 1, 0],
  [1, 0, 1, 0, 1, 1],
  [0, 1, 0, 0, 1, 1, 1],
  [0, 0, 0, 1, 1, 0, 0],
  [0, 0, 0, 1, 0, 0, 0],
  [0, 0, 1, 0, 1, 1, 1],
  [0, 0, 0, 0, 0, 1, 1],
  [0, 0, 0, 0, 1, 0, 0],
  [0, 1, 0, 1, 0, 0, 0],
  [0, 1, 0, 1, 0, 1, 1],
  [0, 0, 1, 0, 0, 1, 1],
  [0, 1, 0, 0, 1, 0, 0],
  [0, 0, 1, 1, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 1, 0],
  [0, 0, 0, 0, 0, 0, 1, 1],
  [0, 0, 0, 1, 1, 0, 1, 0],
  [0, 0, 0, 1, 1, 0, 1, 1],
  [0, 0, 0, 1, 0, 0, 1, 0],
  [0, 0, 0, 1, 0, 0, 1, 1],
  [0, 0, 0, 1, 0, 1, 0, 0],
  [0, 0, 0, 1, 0, 1, 0, 1],
  [0, 0, 0, 1, 0, 1, 1, 0],
  [0, 0, 0, 1, 0, 1, 1, 1],
  [0, 0, 1, 0, 1, 0, 0, 0],
  [0, 0, 1, 0, 1, 0, 0, 1],
  [0, 0, 1, 0, 1, 0, 1, 0],
  [0, 0, 1, 0, 1, 0, 1, 1],
  [0, 0, 1, 0, 1, 1, 0, 0],
  [0, 0, 1, 0, 1, 1, 0, 1],
  [0, 0, 0, 0, 0, 1, 0, 0],
  [0, 0, 0, 0, 0, 1, 0, 1],
  [0, 0, 0, 0, 1, 0, 1, 0],
  [0, 0, 0, 0, 1, 0, 1, 1],
  [0, 1, 0, 1, 0, 0, 1, 0],
  [0, 1, 0, 1, 0, 0, 1, 1],
  [0, 1, 0, 1, 0, 1, 0, 0],
  [0, 1, 0, 1, 0, 1, 0, 1],
  [0, 0, 1, 0, 0, 1, 0, 0],
  [0, 0, 1, 0, 0, 1, 0, 1],
  [0, 1, 0, 1, 1, 0, 0, 0],
  [0, 1, 0, 1, 1, 0, 0, 1],
  [0, 1, 0, 1, 1, 0, 1, 0],
  [0, 1, 0, 1, 1, 0, 1, 1],
  [0, 1, 0, 0, 1, 0, 1, 0],
  [0, 1, 0, 0, 1, 0, 1, 1],
  [0, 0, 1, 1, 0, 0, 1, 0],
  [0, 0, 1, 1, 0, 0, 1, 1],
  [0, 0, 1, 1, 0, 1, 0, 0],
|];

/* QUESTIONS */

/* 1. Byte-by-byte on lists. */

/* Compression, not tail-recursive */

/* Type for runs */
type run =
  | Same(int, int)
  | Diff(list(int));

/* Return list of same characters from a non-null list, and rest */
let rec get_same = (x, n, l) =>
  switch (l) {
  | [h, ...t] when h == x => get_same(x, n + 1, t)
  | _ => (n, l)
  };

/* Return list of different characters from a non-null list, until two are the
 * same as one another */
let rec get_different = (a, l) =>
  switch (l) {
  | [] => (List.rev(a), [])
  | [h, ...t] =>
    if (a == []) {
      get_different([h], t);
    } else if (h != List.hd(a)) {
      get_different([h, ...a], t);
    } else {
      (List.rev(List.tl(a)), [List.hd(a), ...l]);
    }
  };

/* Get a single run */
let getrun = l =>
  switch (l) {
  | [] => raise(Invalid_argument("getrun"))
  | [h, ..._] =>
    switch (get_same(h, 0, l)) {
    | (1, _) =>
      let (diff, rest) = get_different([], l);
      (Diff(diff), rest);
    | (n, rest) => ([@implicit_arity] Same(n, h), rest)
    }
  };

/* Build the next chars from a run */
let chars_of_run = r =>
  switch (r) {
  | [@implicit_arity] Same(length, c) => [257 - length, c]
  | Diff(chars) => [List.length(chars) - 1, ...chars]
  };

/* Compression, tail-recursive */
let rec compress_inner = (a, l) =>
  switch (l) {
  | [] => List.concat(List.map(chars_of_run, List.rev(a)))
  | _ =>
    let (run, rest) = getrun(l);
    compress_inner([run, ...a], rest);
  };

let compress = l => compress_inner([], l) @ [128];

/* Decompression, tail-recursive */
let rec decompress_inner = (a, l) =>
  switch (l) {
  | [128] => List.concat(List.rev(a))
  | []
  | [_] => raise(Invalid_argument("decompress_inner"))
  | [h, t, ...t'] =>
    if (h < 127) {
      let bytes = Util.take([t, ...t'], h + 1);
      let rest = Util.drop([t, ...t'], h + 1);
      decompress_inner([bytes, ...a], rest);
    } else if (h > 128) {
      decompress_inner([Array.to_list(Array.make(257 - h, t)), ...a], t');
    } else {
      decompress_inner(a, []);
    }
  };

let decompress = l => decompress_inner([], l);

/* 2. Tree from codes. */
type tree =
  | Lf
  | Code(int)
  | Br(tree, tree);

let rec add_elt = (tr, (l, n)) =>
  switch (l) {
  | [0, ...m] =>
    switch (tr) {
    | Lf => [@implicit_arity] Br(add_elt(Lf, (m, n)), Lf)
    | [@implicit_arity] Br(left, right) =>
      [@implicit_arity] Br(add_elt(left, (m, n)), right)
    | Code(x) => raise(Failure("collision"))
    }
  | [1, ...m] =>
    switch (tr) {
    | Lf => [@implicit_arity] Br(Lf, add_elt(Lf, (m, n)))
    | [@implicit_arity] Br(left, right) =>
      [@implicit_arity] Br(left, add_elt(right, (m, n)))
    | Code(x) => raise(Failure("collision"))
    }
  | [] => Code(n)
  | _ => raise(Failure("bad code"))
  };

let make_tree = (arr, numbers) =>
  List.fold_left(add_elt, Lf, List.combine(Array.to_list(arr), numbers));

let white_terminating_tree =
  make_tree(
    white_terminating_codes,
    Util.from(0, Array.length(white_terminating_codes) - 1),
  );

/* 4. Making a histogram of black and white frequencies. This is a pair of int
 * arrays of length 1792 for run lengths. */
let getbitint = b =>
  if (getbit(b)) {
    1;
  } else {
    0;
  };

let peekbit = b =>
  if (b.bit == 0) {
    let byte = int_of_char(b.input.input_char());
    rewind(b.input);
    byte land 128 > 0;
  } else {
    b.byte land b.bit > 0;
  };

let rec read_up_to = (v, i, n, w) =>
  if (n >= w) {
    (n, v);
  } else {
    switch (peekbit(i)) {
    | x when x == v =>
      ignore(getbit(i));
      read_up_to(v, i, n + 1, w);
    | x => (n, v)
    };
  };

let build_histogram = (a_white, a_black, i, w, h) => {
  let toread = ref(w * h);
  let wleft = ref(w);
  while (toread^ > 0) {
    let (n, v) = read_up_to(peekbit(i), i, 0, wleft^);
    let a = if (v) {a_black} else {a_white};
    a[n] = a[n] + 1;
    toread := toread^ - n;
    wleft := wleft^ - n;
    if (wleft^ == 0) {
      wleft := w;
    };
  };
};

let histogram_of_input = (i, w, h) => {
  let white = Array.make(1792, 0);
  let black = Array.make(1792, 0);
  build_histogram(white, black, input_bits_of_input(i), w, h);
  (white, black);
};

let print_histogram =
  Array.iteri((x, n) =>
    if (n > 0) {
      Printf.printf("%i runs of length %i\n", n, x);
    }
  );
