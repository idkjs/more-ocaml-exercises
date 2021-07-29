open More;

type turn =
  | O
  | X
  | E;

let won = ([a, b, c, d, e, f, g, h, i]) =>
  a
  && b
  && c
  || d
  && e
  && f
  || g
  && h
  && i
  || a
  && d
  && g
  || b
  && e
  && h
  || c
  && f
  && i
  || a
  && e
  && i
  || c
  && e
  && g;

let empty = b =>
  List.map(
    snd,
    List.filter(
      ((t, _)) => t == E,
      List.combine(b, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    ),
  );

let replace = (turn, board, p) =>
  Util.take(board, p - 1) @ [turn] @ Util.drop(board, p);

let flip_turn = t =>
  switch (t) {
  | O => X
  | X => O
  };

type tree =
  | Move(list(turn), list(tree));

let rec next_moves = (turn, board) => {
  let next =
    if (won(List.map(t => t == O, board))
        || won(List.map(t => t == X, board))) {
      [];
    } else {
      List.map(
        next_moves(flip_turn(turn)),
        List.map(replace(turn, board), empty(board)),
      );
    };

  [@implicit_arity] Move(board, next);
};

let game_tree = next_moves(O, [E, E, E, E, E, E, E, E, E]);

let rec num_wins = (turn, [@implicit_arity] Move(b, bs)) =>
  (
    if (won(List.map(t => t == turn, b))) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(num_wins(turn), bs));
