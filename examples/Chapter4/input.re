type input = {
  pos_in: unit => int,
  seek_in: int => unit,
  input_char: unit => char,
  in_channel_length: int,
};

let input_of_channel = ch => {
  pos_in: () => pos_in(ch),
  seek_in: seek_in(ch),
  input_char: () => input_char(ch),
  in_channel_length: in_channel_length(ch),
};

let input_of_string = s => {
  let pos = ref(0);
  {
    pos_in: () => pos^,
    seek_in: p => {
      if (p < 0) {
        raise(Invalid_argument("seek before beginning"));
      };
      pos := p;
    },
    input_char: () =>
      if (pos^ > String.length(s) - 1) {
        raise(End_of_file);
      } else {
        let c = s.[pos^];
        pos := pos^ + 1;
        c;
      },
    in_channel_length: String.length(s),
  };
};

/* Example : reading words */
let example = "There were four of them; more than before.";

let rewind = i => i.seek_in(i.pos_in() - 1);

let is_non_letter = x =>
  switch (x) {
  | ' '
  | '!'
  | '('
  | ')'
  | '.'
  | ','
  | ';'
  | ':' => true
  | _ => false
  };

let rec skip_characters = i =>
  if (is_non_letter(i.input_char())) {
    skip_characters(i);
  } else {
    rewind(i);
  };

let rec collect_characters = (b, i) =>
  switch (
    try(Some(i.input_char())) {
    | End_of_file => None
    }
  ) {
  | None => Buffer.contents(b)
  | Some(c) =>
    if (is_non_letter(c)) {
      Buffer.contents(b);
    } else {
      Buffer.add_char(b, c);
      collect_characters(b, i);
    }
  };

let read_word = i =>
  try(
    {
      skip_characters(i);
      Some(collect_characters(Buffer.create(20), i));
    }
  ) {
  | End_of_file => None
  };

let rec read_words_inner = (i, a) =>
  switch (read_word(i)) {
  | None => List.rev(List.map(String.lowercase, a))
  | Some(w) => read_words_inner(i, [w, ...a])
  };

let read_words = i => read_words_inner(i, []);

let input_example = () => {
  print_endline("Words read:");
  List.iter(print_endline, read_words(input_of_string(example)));
};

/* A type for outputs */

type output = {
  output_char: char => unit,
  out_channel_length: unit => int,
};

let output_of_channel = ch => {
  output_char: c => output_byte(ch, int_of_char(c)),
  out_channel_length: () => out_channel_length(ch),
};

let output_of_bytes = b => {
  let pos = ref(0);
  {
    output_char: c =>
      if (pos^ < Bytes.length(b)) {
        Bytes.set(b, pos^, c);
        pos := pos^ + 1;
      } else {
        raise(End_of_file);
      },
    out_channel_length: () => Bytes.length(b),
  };
};

let output_int_list = (o, ls) => {
  o.output_char('[');
  List.iter(
    n => {
      String.iter(o.output_char, string_of_int(n));
      o.output_char(';');
      o.output_char(' ');
    },
    ls,
  );
  o.output_char(']');
};

let output_example = () => {
  print_endline("Using output_int_list:");
  output_int_list(output_of_channel(stdout), [1, 2, 3, 4, 5]);
  print_endline("");
};
