/* Representing PDF Documents */
type pdfobject =
  | Boolean(bool)
  | Integer(int)
  | Float(float)
  | String(string)
  | Name(string)
  | Array(list(pdfobject))
  | Dictionary(list((string, pdfobject)))
  | Stream(pdfobject, string)
  | Indirect(int);

type t = {
  version: (int, int),
  objects: list((int, pdfobject)),
  trailer: pdfobject,
};
