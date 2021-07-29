/* Open the 'input' module from Chapter 4 */
open Input;

type input_bits = {
  input,
  mutable byte: int,
  mutable bit: int,
};

let input_bits_of_input = i => {input: i, byte: 0, bit: 0};

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

/* This function is one of the exercises. Do not peek! */
let getval_32 = (b, n) =>
  if (n < 0) {
    raise(Invalid_argument("getval_32"));
  } else if (n == 0) {
    0l;
  } else {
    let r = ref(Int32.zero);
    for (x in n - 1 downto 0) {
      r :=
        Int32.logor(
          r^,
          Int32.shift_left(
            Int32.of_int(
              if (getbit(b)) {
                1;
              } else {
                0;
              },
            ),
            x,
          ),
        );
    };
    r^;
  };

/* Deconstruction of TCP datagram header */
let print_header = filename => {
  let ch = open_in_bin(filename);
  let i = input_of_channel(ch);
  let b = input_bits_of_input(i);
  let src_port = getval(b, 16);
  let dest_port = getval(b, 16);
  let seq_number = getval_32(b, 32);
  let ack_number = getval_32(b, 32);
  let data_offset = getval(b, 4);
  let reserved = getval(b, 6);
  let urgent = getbit(b);
  let ack = getbit(b);
  let push = getbit(b);
  let reset = getbit(b);
  let syn = getbit(b);
  let fin = getbit(b);
  let receive = getval(b, 16);
  let checksum = getval(b, 16);
  let urgent_pointer = getval(b, 16);
  print_string("Source port = ");
  print_int(src_port);
  print_string("\nDestination = ");
  print_int(dest_port);
  print_string("\nSequence = ");
  print_string(Int32.to_string(seq_number));
  print_string("\nAcknowledgement Number = ");
  print_string(Int32.to_string(ack_number));
  print_string("\ndata offset = ");
  print_int(data_offset);
  print_string("\nreserved = ");
  print_int(reserved);
  let print_bool = b => print_string(string_of_bool(b));
  print_string("\nFlags:\n Urgent = ");
  print_bool(urgent);
  print_string("\n Ack = ");
  print_bool(ack);
  print_string("\n Push = ");
  print_bool(push);
  print_string("\n Reset = ");
  print_bool(reset);
  print_string("\n Syn = ");
  print_bool(syn);
  print_string("\n Fin = ");
  print_bool(fin);
  print_string("\nReceive window size = ");
  print_int(receive);
  print_string("\nChecksum = ");
  print_int(checksum);
  print_string("\nUrgent pointer = ");
  print_int(urgent_pointer);
  print_string("\n");
  close_in(ch);
};

let input_example = () => print_header("packet.bin");

/* Output bit streams */
type output_bits = {
  output,
  mutable obyte: int,
  mutable obit: int,
};

let output_bits_of_output = o => {output: o, obyte: 0, obit: 7};

/* Flush a byte to the underlying output, padding with zeroes. If output byte
 * has not been touched, don't output. */
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

/* This function is one of the exercises. Do not peek! */
let putval_32 = (o, v, l) =>
  for (x in l - 1 downto 0) {
    putbit(o, Int32.to_int(Int32.logand(v, Int32.shift_left(1l, x))));
  };

/* Output bit streams example. */
let output_header = filename => {
  let ch = open_out_bin(filename);
  let o = output_of_channel(ch);
  let bits = output_bits_of_output(o);
  putval(bits, 38, 16);
  putval(bits, 47892, 16);
  putval_32(bits, 1656212531l, 32);
  putval_32(bits, 1481973485l, 32);
  putval(bits, 0, 4);
  putval(bits, 32, 6);
  putbit(bits, 0);
  putbit(bits, 0);
  putbit(bits, 0);
  putbit(bits, 0);
  putbit(bits, 0);
  putbit(bits, 0);
  putval(bits, 17664, 16);
  putval(bits, 888, 16);
  putval(bits, 63404, 16);
  flush(bits);
  close_out(ch);
};

let output_example = () => output_header("packet_out.bin");
