type point('a) = {
  x: float,
  y: float,
  label: string,
  content: 'a,
};

let p = {x: 4.5, y: 6.0, label: "P", content: [1, 3, 1]};

let make_point = (x, y, l, c) => {x, y, label: l, content: c};

let make_point = (x, y, label, content) => {x, y, label, content};

let string_of_point = p =>
  p.label
  ++ " = ("
  ++ string_of_float(p.x)
  ++ ", "
  ++ string_of_float(p.y)
  ++ ")";

let string_of_point = ({label: l, x, y}) =>
  l ++ " = (" ++ string_of_float(x) ++ ", " ++ string_of_float(y) ++ ")";

let string_of_point = ({label: l, x, y, _}) =>
  l ++ " = (" ++ string_of_float(x) ++ ", " ++ string_of_float(y) ++ ")";

let string_of_point = ({label, x, y, _}) =>
  label ++ " = (" ++ string_of_float(x) ++ ", " ++ string_of_float(y) ++ ")";

let relabel = (p, l) => {...p, label: l};

let relabel = (p, label) => {...p, label};

let mirror = p => {...p, x: p.y, y: p.x};
