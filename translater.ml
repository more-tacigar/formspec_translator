exception Not_number
exception Already_registered

let is_int f =
  let cf = classify_float (fst (modf f)) in
  cf == FP_zero

class translater = object (self)
  val buffer = Buffer.create 10
  val symtable = Hashtbl.create 10

  method retrieve fields name =
    let exp = List.assoc name fields in
    self#string_of_expression exp

  method retrieve_optional fields name =
    try Some(self#retrieve fields name)
    with Not_found -> None

  method retrieve_position fields =
    let xexp = List.assoc "X" fields in
    let yexp = List.assoc "Y" fields in
    (self#string_of_expression xexp, self#string_of_expression yexp)

  method retrieve_size fields =
    let wexp = List.assoc "W" fields in
    let hexp = List.assoc "H" fields in
    (self#string_of_expression wexp, self#string_of_expression hexp)

  method retrieve_position_and_size fields =
    let x, y = self#retrieve_position fields in
    let w, h = self#retrieve_size fields in
    (x, y, w, h)

  method output strs =
    List.iter (fun str -> Buffer.add_string buffer str) strs;
    Buffer.add_char buffer '\n'

  method translate_formspec external_definitions =
    List.iter (fun external_definition ->
      match external_definition with
      | Ast.Variable_definition (v, exp) ->
        begin
          if Hashtbl.mem symtable v then
            raise Already_registered
          else
            Hashtbl.add symtable v exp
        end
      | Ast.Element_definition (e, fields) ->
        begin
          match e with
          | "size"               -> self#translate_size fields
          | "list"               -> self#translate_list fields
          | "image"              -> self#translate_image fields
          | "field"              -> self#translate_field fields
          | "pwdfield"           -> self#translate_pwdfield fields
          | "textarea"           -> self#translate_textarea fields
          | "label"              -> self#translate_label fields
          | "vertlabel"          -> self#translate_vertlabel fields
          | "button"             -> self#translate_button fields
          | "image_button"       -> self#translate_image_button fields
          | "item_image_button"  -> self#translate_item_image_button fields
          | "button_exit"        -> self#translate_button_exit fields
          | "image_button_exit"  -> self#translate_image_button_exit fields
          | "bgcolor"            -> self#translate_bgcolor fields
          | "background"         -> self#translate_background fields
          | "textlist"           -> self#translate_textlist fields
          | "dropdown"           -> self#translate_dropdown fields
          | _                    -> assert false
        end
    ) external_definitions;
    buffer

  method float_of_expression expression =
    match expression with
    | Ast.Variable_expression v ->
      self#float_of_expression (Hashtbl.find symtable v)
    | Ast.Number_expression n -> n
    | Ast.Binary_operation_expression (lhs, op, rhs) ->
      let lhs_number = self#float_of_expression lhs in
      let rhs_number = self#float_of_expression rhs in
      begin
        match op with
        | Ast.Plus   -> lhs_number +. rhs_number
        | Ast.Minus  -> lhs_number -. rhs_number
        | Ast.Mult   -> lhs_number *. rhs_number
        | Ast.Div    -> lhs_number /. rhs_number
      end
    | Ast.Paren_expression e ->
      self#float_of_expression e
    | _ -> raise Not_number (* Identifier_expression / Array_expression *)

  method string_of_expression expression =
    match expression with
    | Ast.Variable_expression v ->
      self#string_of_expression (Hashtbl.find symtable v)
    | Ast.Number_expression n ->
      if is_int n then
        string_of_int (int_of_float n)
      else
        string_of_float n
    | Ast.Identifier_expression id ->
      id
    | Ast.Binary_operation_expression _ ->
      let result = self#float_of_expression expression in
      if is_int result then
        string_of_int (int_of_float result)
      else
        string_of_float result
    | Ast.Paren_expression e ->
      self#string_of_expression e
    | Ast.Array_expression elems ->
      let tmpbuf = Buffer.create 20 in
      for i = 0 to (List.length elems) - 1 do
        Buffer.add_string tmpbuf (self#string_of_expression (List.nth elems i));
        if i != (List.length elems) - 1 then
          Buffer.add_char tmpbuf ','
      done;
      Buffer.contents tmpbuf

  method translate_size fields =
    let w, h = self#retrieve_size fields in
    self#output [
      "size["; w; ","; h; "]";
    ]

  method translate_list fields =
    let inventory_location = self#retrieve fields "inventory_location" in
    let list_name = self#retrieve fields "list_name" in
    let x, y, w, h = self#retrieve_position_and_size fields in
    let starting_item_index = self#retrieve_optional fields "starting_item_index" in
    match starting_item_index with
    | Some sii ->
      self#output [
        "list["; inventory_location; ";"; list_name; ";";
        x; ","; y; ";"; w; ","; h; ";"; sii; "]"
      ]
    | None ->
      self#output [
        "list["; inventory_location; ";"; list_name; ";";
        x; ","; y; ";"; w; ","; h; ";"; "]"
      ]

  method translate_image fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let texture_name = self#retrieve fields "texture_name" in
    self#output [
      "image["; x; ","; y; ";"; w; ","; h; ";"; texture_name; "]"
    ]

  method translate_field fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    let default = self#retrieve fields "default" in
    self#output [
      "field["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; label; ";"; default; "]"
    ]

  method translate_pwdfield fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "pwdfield["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; label; "]"
    ]

  method translate_textarea fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    let default = self#retrieve fields "default" in
    self#output [
      "textarea["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; label; ";"; default; "]"
    ]

  method translate_label fields =
    let x, y = self#retrieve_position fields in
    let label = self#retrieve fields "label" in
    self#output [
      "label["; x; ","; y; ";"; label; "]"
    ]

  method translate_vertlabel fields =
    let x, y = self#retrieve_position fields in
    let label = self#retrieve fields "label" in
    self#output [
      "vertlabel["; x; ","; y; ";"; label; "]"
    ]

  method translate_button fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "button["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; label; "]"
    ]

  method translate_image_button fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let image = self#retrieve fields "image" in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "image_button["; x; ","; y; ";"; w; ","; h; ";"; image; ";"; name; ";"; label; "]"
    ]

  method translate_item_image_button fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let item_name = self#retrieve fields "item_name" in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "item_image_button["; x; ","; y; ";"; w; ","; h; ";"; item_name; ";"; name; ";"; label; "]"
    ]

  method translate_button_exit fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "button_exit["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; label; "]"
    ]

  method translate_image_button_exit fields =
    let x, y, w, h = self#retrieve_position_and_size fields in
    let image = self#retrieve fields "image" in
    let name = self#retrieve fields "name" in
    let label = self#retrieve fields "label" in
    self#output [
      "image_button_exit["; x; ","; y; ";"; w; ","; h; ";"; image; ";"; name; ";"; label; "]"
    ]

  method translate_bgcolor fields =
    let color = self#retrieve fields "color" in
    let fullscreen = self#retrieve fields "fullscreen" in
    self#output [
      "bgcolor["; color; ";"; fullscreen; ";]"
    ]

  method translate_background fields =
    let x, y, h, w = self#retrieve_position_and_size fields in
    let texture_name = self#retrieve fields "texture_name" in
    let auto_clip = self#retrieve_optional fields "auto_clip" in
    match auto_clip with
    | Some ac ->
      self#output [
        "background["; x; ","; y; ";"; w; ","; h; ";"; texture_name; ";"; ac; "]"
      ]
    | None ->
      self#output [
        "background["; x; ","; y; ";"; w; ","; h; ";"; texture_name; "]"
      ]

  method translate_textlist fields =
    let x, y, h, w = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let listelems = self#retrieve fields "listelems" in
    let selected_idx = self#retrieve_optional fields "selected_idx" in
    match selected_idx with
    | Some si ->
      let transparent = self#retrieve fields "transparent" in
      self#output [
        "textlist["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; listelems; ";";
        si; ";"; transparent; "]"
      ]
    | None ->
      self#output [
        "textlist["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; listelems; "]"
      ]

  method translate_dropdown fields =
    let x, y, h, w = self#retrieve_position_and_size fields in
    let name = self#retrieve fields "name" in
    let items = self#retrieve fields "items" in
    let selected_idx = self#retrieve fields "selected_idx" in
    self#output [
      "dropdown["; x; ","; y; ";"; w; ","; h; ";"; name; ";"; items; ";"; selected_idx; "]"
    ]
end
