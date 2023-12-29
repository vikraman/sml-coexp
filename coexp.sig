(* coexponentials *)
signature COEXP =
sig
    type 'a dual
    val colam : ('a dual -> 'b) -> ('a, 'b) Either.either
    val coapp : ('a, 'b) Either.either -> 'a dual -> 'b
end
