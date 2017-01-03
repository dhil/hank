module type K = sig
    val apply : ('a, 'b) continuation -> 'a -> 'b
end

module Multishot : K = struct
  let apply k x =
    let k' = Obj.clone k in
    continue k' x
end

module Singleshot : K = struct
    let apply k x = continue k x
end
