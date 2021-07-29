/* 1 */
let make = (~len, ~elt) => Array.make(len, elt);

/* 2 */
type start =
  | Start(int);

type length =
  | Length(int);

let fill = (a, Start(s), Length(l), v) =>
  for (x in s to s + l - 1) {
    a[x] = v;
  };

let filled = () => {
  let a = Array.make(100, "x");
  fill(a, Start(20), Length(40), "y");
  a;
};

/* 3 */
let sub = (b, ~off, ~len) => Buffer.sub(b, off, len);

let blit = (src, ~srcoff, dst, ~dstoff, ~len) =>
  Buffer.blit(src, srcoff, dst, dstoff, len);

let add_substring = (b, s, ~ofs, ~len) =>
  Buffer.add_substring(b, s, ofs, len);

/* 5 */
let rec map = (~a=[], f, l) =>
  switch (l) {
  | [] => List.rev(a)
  | [h, ...t] => map(~a=[f(h), ...a], f, t)
  };
