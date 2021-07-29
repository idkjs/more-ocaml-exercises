/* Answers for chapter 5 */
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

let rec getbit = b =>
  if (b.bit == 0) {
    b.byte = int_of_char(b.input.input_char());
    b.bit = 128;
    getbit(b);
  } else {
    let r = b.byte land b.bit > 0;
    b.bit = b.bit / 2;
    r;
  };

let align = b => b.bit = 0;

let getval = (b, n) =>
  if (n <= 0 || n > 31) {
    raise(Invalid_argument("getval"));
  } else {
    let r = ref(0);
    for (x in n - 1 downto 0) {
      r :=
        r^
        lor (
          if (getbit(b)) {
            1;
          } else {
            0;
          }
        )
        lsl x;
    };
    r^;
  };

/* 1 */
let getval_fast = (b, n) =>
  if (n == 8 && b.bit == 0) {
    int_of_char(b.input.input_char());
  } else {
    getval(b, n);
  };

/* 2 */
let getval_32 = (b, n) =>
  if (n < 0) {
    raise(Invalid_argument("getval_32"));
  } else if (n == 0) {
    0l;
  } else {
    let r = ref(Int32.zero);
    for (x in n - 1 downto 0) {
      let num =
        Int32.of_int(
          if (getbit(b)) {
            1;
          } else {
            0;
          },
        );
      r := Int32.logor(r^, Int32.shift_left(num, x));
    };
    r^;
  };

/* 3 */
type output = {
  output_char: char => unit,
  out_channel_length: unit => int,
};

type output_bits = {
  output, /* underlying output */
  mutable obyte: int, /* the byte we're building up */
  mutable obit: int,
};

let flush = o => {
  if (o.obit < 7) {
    o.output.output_char(char_of_int(o.obyte));
  };
  o.obyte = 0;
  o.obit = 7;
};

let rec putbit = (o, b) =>
  if (o.obit == (-1)) {
    flush(o);
    putbit(o, b);
  } else {
    if (b != 0) {
      o.obyte = o.obyte lor 1 lsl o.obit;
    };
    o.obit = o.obit - 1;
  };

let putval = (o, v, l) =>
  for (x in l - 1 downto 0) {
    putbit(o, v land 1 lsl x);
  };

let putval_fast = (o, v, l) =>
  if (l == 8 && o.obit == 7) {
    o.output.output_char(char_of_int(v));
  } else {
    putval(o, v, l);
  };

/* 4 */
let putval_32 = (o, v, l) =>
  for (x in l - 1 downto 0) {
    putbit(o, Int32.to_int(Int32.logand(v, Int32.shift_left(1l, x))));
  };

/* 5 */

/* We name this output2 since one cannot have two types with the same name in a
 * single file. */
type output2 = {
  output_char: char => unit,
  rewind: unit => unit,
  out_channel_length: unit => int,
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
    rewind: () =>
      if (pos^ > 0) {
        pos := pos^ - 1;
      } else {
        raise(Failure("rewind"));
      },
    out_channel_length: () => Bytes.length(b),
  };
};

/* We name this output_bits2 since one cannot have two types with the same name
 * in a single file. */
type output_bits2 = {
  output: output2,
  mutable obyte: int,
  mutable obit: int,
};

let output_bits_of_output = output => {output, obyte: 0, obit: 7};

let rec putbit = (o, b) =>
  if (o.obit == (-1)) {
    o.obyte = 0;
    o.obit = 7;
    putbit(o, b);
  } else {
    if (b != 0) {
      o.obyte = o.obyte lor 1 lsl o.obit;
    };
    o.output.output_char(char_of_int(o.obyte));
    o.output.rewind();
    o.obit = o.obit - 1;
  };
