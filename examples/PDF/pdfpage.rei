/* Build PDF page, with ops */
type t =
  | Move(float, float)
  | Line(float, float)
  | Close
  | Stroke
  | Fill
  | FillColour(float)
  | StrokeColour(float)
  | SetClip
  | StrokeColourRGB(float, float, float)
  | FillColourRGB(float, float, float)
  | LineWidth(float)
  | BeginText
  | EndText
  | SetTextPosition(float, float)
  | SetFontAndSize(string, float)
  | ShowText(string)
  | SetCharacterSpacing(float);

let string_of_op: t => string;

let string_of_ops: list(t) => string;
