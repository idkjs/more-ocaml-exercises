type input = {
  pos_in: unit => int,
  seek_in: int => unit,
  input_char: unit => char,
  in_channel_length: int,
};

type input_bits = {
  input,
  mutable byte: int,
  mutable bit: int,
};

/* 1 */
let input_of_array = a => {
  let pos = ref(0);
  {
    pos_in: () => pos^,
    seek_in: p => {
      if (p < 0) {
        raise(Invalid_argument("seek < 0"));
      };
      pos := p;
    },
    input_char: () =>
      if (pos^ > Array.length(a) - 1) {
        raise(End_of_file);
      } else {
        let c = a[pos^];
        pos := pos^ + 1;
        c;
      },
    in_channel_length: Array.length(a),
  };
};

/* 2 */
let input_string = (i, n) => {
  let b = Buffer.create(100);
  try(
    {
      for (x in 0 to n - 1) {
        Buffer.add_char(b, i.input_char());
      };
      Buffer.contents(b);
    }
  ) {
  | End_of_file => Buffer.contents(b)
  };
};

/* 3 */

/* We have named it input2 here, since one cannot have two types of the same
 * name in a single file. */
type input2 = {
  pos_in: unit => int,
  seek_in: int => unit,
  input_char: unit => char,
  input_char_opt: unit => option(char),
  in_channel_length: int,
};

let input_of_channel = ch => {
  pos_in: () => pos_in(ch),
  seek_in: seek_in(ch),
  input_char: () => input_char(ch),
  input_char_opt: () =>
    try(Some(input_char(ch))) {
    | End_of_file => None
    },
  in_channel_length: in_channel_length(ch),
};

let input_of_string = s => {
  let pos = ref(0);
  {
    pos_in: () => pos^,
    seek_in: p => {
      if (p < 0) {
        raise(Invalid_argument("seek < 0"));
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
    input_char_opt: () =>
      if (pos^ > String.length(s) - 1) {
        None;
      } else {
        let c = s.[pos^];
        pos := pos^ + 1;
        Some(c);
      },
    in_channel_length: String.length(s),
  };
};

/* 4 */

/* We have named it input3 here, since one cannot have two types of the same
 * name in a single file. */
type input3 = {
  pos_in: unit => int,
  seek_in: int => unit,
  input_char: unit => char,
  input_byte: unit => int,
  in_channel_length: int,
};

let no_more = (-1);

let input_of_channel = ch => {
  pos_in: () => pos_in(ch),
  seek_in: seek_in(ch),
  input_char: () => input_char(ch),
  input_byte: () =>
    try(int_of_char(input_char(ch))) {
    | End_of_file => no_more
    },
  in_channel_length: in_channel_length(ch),
};

let input_of_string = s => {
  let pos = ref(0);
  {
    pos_in: () => pos^,
    seek_in: p => {
      if (p < 0) {
        raise(Invalid_argument("seek < 0"));
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
    input_byte: () =>
      if (pos^ > String.length(s) - 1) {
        no_more;
      } else {
        let c = s.[pos^];
        pos := pos^ + 1;
        int_of_char(c);
      },
    in_channel_length: String.length(s),
  };
};

/* 5 */
let single_line_input_of_channel = ch => {
  pos_in: () => pos_in(ch),
  seek_in: seek_in(ch),
  input_char: () =>
    switch (input_char(ch)) {
    | '\n' => raise(End_of_file)
    | c => c
    },
  in_channel_length: in_channel_length(ch),
};

let input_a_string = () =>
  input_string(single_line_input_of_channel(stdin), max_int);

/* 6 */
type output = {
  output_char: char => unit,
  out_channel_length: unit => int,
};

let output_of_buffer = b => {
  output_char: Buffer.add_char(b),
  out_channel_length: () => Buffer.length(b),
};

let build_buffer = () => {
  let b = Buffer.create(20);
  let o = output_of_buffer(b);
  o.output_char('A');
  o.output_char('B');
  o.output_char('C');
  Buffer.contents(b);
};
