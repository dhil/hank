open Lexing

type t = {
    loc_start: position;
    loc_end: position;
    loc_ghost: bool;
  }

let in_file name =
  let loc = {
      pos_fname = name;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = -1;
    }
  in
  { loc_start = loc;
    loc_end = loc;
    loc_ghost = true }

let none = in_file "_none_"

let make pos_start pos_end =
  { loc_start = pos_start;
    loc_end = pos_end;
    loc_ghost = false }

module Located = struct
  type nonrec 'a t = {
      loc_item: 'a;
      loc_pos: t;
    }

  let lift elem pos_start pos_end =
    { loc_item = elem;
      loc_pos = make pos_start pos_end }

  let item lc = lc.loc_item

  let lift_dummy elem =
    { loc_item = elem;
      loc_pos = none }
end
