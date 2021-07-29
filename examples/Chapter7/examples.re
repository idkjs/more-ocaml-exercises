open More;

let fill = (a, s, l, v) =>
  for (x in s to s + l - 1) {
    a[x] = v;
  };

let filled = () => {
  let a = Array.make(100, "x");
  fill(a, 20, 40, "y");
  a;
};

let fill = (a, ~start as s, ~length as l, v) =>
  for (x in s to s + l - 1) {
    a[x] = v;
  };

let filled = () => {
  let a = Array.make(100, "x");
  fill(a, ~start=20, ~length=40, "y");
  a;
};

let filled = () => {
  let a = Array.make(100, "x");
  let st = 20;
  let ln = 40;
  fill(a, ~start=st, ~length=ln, "y");
  a;
};

let filled = () => {
  let a = Array.make(100, "x");
  fill(a, "y", ~length=20, ~start=40);
  a;
};

let fill = (a, ~start, ~length, v) =>
  for (x in start to start + length - 1) {
    a[x] = v;
  };

let filled = () => {
  let a = Array.make(100, "x");
  let start = 20;
  let length = 40;
  fill(a, ~start, ~length, "y");
  a;
};

/* Labels with partial application. */
let divide = (x, y) => x / y;

let f = divide(10000);

let _ = [f(100), f(50), f(20)];

let divide = (~x, ~y) => x / y;

let f = divide(~x=10000);

let _ = [f(100), f(50), f(20)];

let f = divide(~y=10000);

let _ = [f(100000), f(10000), f(1000)];

/* Optional arguments. */
let rec split = l =>
  switch (l) {
  | [] => []
  | [h, ...t] => [[h], ...split(t)]
  };

let rec split = (~chunksize, l) =>
  try([
    Util.take(l, chunksize),
    ...split(~chunksize, Util.drop(l, chunksize)),
  ]) {
  | _ =>
    switch (l) {
    | [] => []
    | _ => [l]
    }
  };

let rec split = (~chunksize=1, l) =>
  try([
    Util.take(l, chunksize),
    ...split(~chunksize, Util.drop(l, chunksize)),
  ]) {
  | _ =>
    switch (l) {
    | [] => []
    | _ => [l]
    }
  };

let rec split = (~chunksize=?, l) => {
  let ch =
    switch (chunksize) {
    | None => 1
    | Some(x) => x
    };

  try([Util.take(l, ch), ...split(~chunksize=ch, Util.drop(l, ch))]) {
  | _ =>
    switch (l) {
    | [] => []
    | _ => [l]
    }
  };
};
