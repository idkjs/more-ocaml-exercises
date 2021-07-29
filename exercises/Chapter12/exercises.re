open More;

/* 1 */

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

let replace = (turn, board, p) =>
  Util.take(board, p - 1) @ [turn] @ Util.drop(board, p);

let empty = b =>
  List.map(
    snd,
    List.filter(
      ((t, _)) => t == E,
      List.combine(b, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    ),
  );

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

/* num_wins X game_tree = 77904 */
/* num_wins O game_tree = 131184 */

let rec drawn = ([@implicit_arity] Move(b, bs)) =>
  (
    if (empty(b) == []
        && !won(List.map((==)(O), b))
        && !won(List.map((==)(X), b))) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(drawn, bs));

let rec terminals = ([@implicit_arity] Move(b, bs)) =>
  (
    if (bs == []) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(terminals, bs));

/* drawn game_tree = 46080 */
/* so total games = 46080 + 77904 + 131184 = 255168 */

/* 2 */

/* The changes are simple */

/* We call this tree2 since no two types in a single file may have the same name */

type tree2 =
  | Move2(list(turn), unit => list(tree2));

let rec next_moves = (turn, board) => {
  let next = () =>
    if (won(List.map(t => t == O, board))
        || won(List.map(t => t == X, board))) {
      [];
    } else {
      List.map(
        next_moves(flip_turn(turn)),
        List.map(replace(turn, board), empty(board)),
      );
    };

  [@implicit_arity] Move2(board, next);
};

let game_tree2 = next_moves(O, [E, E, E, E, E, E, E, E, E]);

/* We wish to force evaluation once, take only the case where the centre is
 * chosen, and then force all of that case. */
let select_case = (board, [@implicit_arity] Move2(_, f)) =>
  switch (List.filter(([@implicit_arity] Move2(b, _)) => b == board, f())) {
  | [[@implicit_arity] Move2(b, g)] => g()
  | _ => raise(Failure("select_case"))
  };

let rec num_wins = (turn, [@implicit_arity] Move2(b, bs)) =>
  (
    if (won(List.map(t => t == turn, b))) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(num_wins(turn), bs()));

let pos_wins = (turn, pos) =>
  List.fold_left(
    (+),
    0,
    List.map(num_wins(turn), select_case(pos, game_tree2)),
  );

let rec drawn = ([@implicit_arity] Move2(b, bs)) =>
  (
    if (empty(b) == []
        && !won(List.map(t => t == O, b))
        && !won(List.map(t => t == X, b))) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(drawn, bs()));

let draws = pos =>
  List.fold_left((+), 0, List.map(drawn, select_case(pos, game_tree2)));

let centre = [E, E, E, E, O, E, E, E, E];

let side = [E, O, E, E, E, E, E, E, E];

let corner = [O, E, E, E, E, E, E, E, E];

let centre_x_wins = pos_wins(X, centre);

let centre_o_wins = pos_wins(O, centre);

let centre_drawn = draws(centre);

let side_x_wins = pos_wins(X, side) * 4;

let side_o_wins = pos_wins(O, side) * 4;

let side_drawn = draws(side) * 4;

let corner_x_wins = pos_wins(X, corner) * 4;

let corner_o_wins = pos_wins(O, corner) * 4;

let corner_drawn = draws(side) * 4;

/*val centre_x_wins : int = 5616
  val centre_o_wins : int = 15648
  val centre_drawn : int = 4608
  val side_x_wins : int = 40704
  val side_o_wins : int = 56928
  val side_drawn : int = 20736
  val corner_x_wins : int = 31584
  val corner_o_wins : int = 58608
  val corner_drawn : int = 20736*/

/* total is 255168, of course. */

/* 3 */
let rec combinations = l =>
  switch (l) {
  | [] => [[]]
  | [h, ...t] =>
    let cs = combinations(t);
    List.map(x => [h, ...x], cs) @ cs;
  };

/* We name this tree3 since no two types in a single file may share a name */
type tree3 =
  | Move(list(int), list(int), list(tree3));

let sum = l => List.fold_left((+), 0, l) == 15;

let threes = l => List.filter(l => List.length(l) == 3, combinations(l));

let won = l => List.mem(true, List.map(sum, threes(l)));

let drawn = (l, l') => List.length(l) + List.length(l') == 9;

let possibles = all =>
  List.filter(x => !List.mem(x, all), [1, 2, 3, 4, 5, 6, 7, 8, 9]);

let rec next_moves = (xs, os, o_is_playing) => {
  let next =
    if (won(xs) || won(os) || drawn(xs, os)) {
      [];
    } else if (o_is_playing) {
      List.map(
        new_os => next_moves(xs, new_os, !o_is_playing),
        List.map(q => [q, ...os], possibles(xs @ os)),
      );
    } else {
      List.map(
        new_xs => next_moves(new_xs, os, !o_is_playing),
        List.map(q => [q, ...xs], possibles(xs @ os)),
      );
    };

  [@implicit_arity] Move(xs, os, next);
};

let game_tree3 = next_moves([], [], true);

let rec xwins = ([@implicit_arity] Move(xs, os, cs)) =>
  (
    if (won(xs)) {
      1;
    } else {
      0;
    }
  )
  + List.fold_left((+), 0, List.map(xwins, cs));
