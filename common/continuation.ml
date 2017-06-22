module type K = sig
  val apply : ('a, 'b) continuation -> 'a -> 'b
  val raise : ('a, 'b) continuation -> exn -> 'b
end

module Multishot : K = struct
  let apply k x =
    let k' = Obj.clone_continuation k in
    continue k' x

  let raise = discontinue
end

module Singleshot : K = struct
  let apply k x = continue k x
  let raise = discontinue
end
