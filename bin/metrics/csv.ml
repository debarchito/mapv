type t = { oc : out_channel }

let escape s =
  let needs_quote =
    String.exists (fun c -> c = ',' || c = '"' || c = '\n' || c = '\r') s
  in
  if not needs_quote then s
  else
    let b = Buffer.create (String.length s + 8) in
    Buffer.add_char b '"';
    String.iter
      (fun c -> if c = '"' then Buffer.add_string b "\"\"" else Buffer.add_char b c)
      s;
    Buffer.add_char b '"';
    Buffer.contents b

let create path header =
  let oc = open_out_bin path in
  output_string oc (String.concat "," (List.map escape header));
  output_char oc '\n';
  { oc }

let write t values =
  output_string t.oc (String.concat "," (List.map escape values));
  output_char t.oc '\n'

let flush t = flush t.oc
let close t = close_out t.oc

let float v =
  if Float.is_nan v then "nan"
  else if Float.is_infinite v then if v > 0.0 then "inf" else "-inf"
  else Printf.sprintf "%.15g" v
