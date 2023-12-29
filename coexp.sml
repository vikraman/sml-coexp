(* coexponentials using native continuations *)
structure Coexp: COEXP =
struct
    open Either
    open SMLofNJ . Cont

    type 'a dual = 'a cont

    fun colam (f : 'a dual -> 'b) : ('a, 'b) either =
      callcc (fn (k : ('a, 'b) either dual) =>
        let val a = callcc (fn (ka : 'a dual) => throw k (INR (f ka)))
        in throw k (INL a)
        end
      )
    fun coapp (e : ('a, 'b) either) (k : 'a dual) : 'b =
      case e of
        INL a => throw k a
      | INR b => b
end
