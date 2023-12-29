(* classical logic and control operators *)
structure Classical =
struct
	open Coexp
    open Either

    (* callcc/throw from colam/coapp *)
    fun codiag (e : ('a, 'a) either) : 'a =
        case e of
          INL a => a
        | INR a => a
    fun callcc (f : 'a dual -> 'a) : 'a =
        codiag (colam f)
    fun throw (a : 'a) (k : 'a dual) : 'b =
        coapp (INL a) k

    fun tnd () : ('a, 'a dual) either =
        colam (fn k => k)
    fun dni (a : 'a) : ('a dual) dual =
        case tnd () of
            INL ka => throw a ka
          | INR kka => kka
    fun dne (kka : ('a dual) dual) : 'a =
        case tnd () of
            INL a => a
          | INR ka => throw ka kka

    fun swap (INL a) = INR a
      | swap (INR b) = INL b
    fun adj (f : 'a dual -> 'b) : 'b dual -> 'a =
        coapp (swap (colam f))
    fun contramap (f : 'a -> 'b) : 'b dual -> 'a dual =
        adj (fn kka => f (dne kka))

    (* Peirce's law *)
    fun callCC (f : ('a -> 'b) -> 'a) : 'a =
        codiag (colam (fn k => f (fn a => throw a k)))

    (* deMorgan's laws *)
    fun deMorgan1 (k : ('a, 'b) either dual) : ('a dual * 'b dual) =
        (contramap INL k, contramap INR k)
    fun deMorgan2 ((ka , kb) : 'a dual * 'b dual) : ('a, 'b) either dual =
        contramap (fn e => coapp e ka) kb
    fun deMorgan3 (kp : ('a * 'b) dual) : ('a dual, 'b dual) either =
        adj (fn ks => let val (kka, kkb) = deMorgan1 ks
                      in (dne kka, dne kkb) end)
            kp
    fun deMorgan4 (ks : ('a dual, 'b dual) either) : ('a * 'b) dual =
        case tnd () of
            INL (a, b) => (case ks of
                               INL ka => throw a ka
                             | INR kb => throw b kb)
          | INR kp => kp

    (* subtraction is dual to functions *)
    type ('a, 'b) sub = ('a * 'b dual)
    fun ftoc (f : 'a -> 'b) : ('a, 'b) sub dual =
        case tnd () of
            INL (a, kb) => throw (f a) kb
          | INR kp => kp
    fun ctof (k : ('a, 'b) sub dual) : 'a -> 'b =
        fn a => case deMorgan3 k of
                    INL ka => throw a ka
                  | INR kkb => dne kkb

    (* Lawvere's boundary operator *)
    type 'a del = ('a, 'a) sub
    fun leibniz1 (p : ('a * 'b) del) : (('a del * 'b), ('a * 'b del)) either =
        let val ((a, b), kp) = p in
            case deMorgan3 kp of
                INL ka => INL((a, ka), b)
              | INR kb => INR(a, (b, kb))
        end
    fun leibniz2 (s : (('a del * 'b), ('a * 'b del)) either) : ('a * 'b) del =
        case s of
            INL ((a, ka), b) => ((a, b), deMorgan4 (INL ka))
          | INR (a, (b, kb)) => ((a, b), deMorgan4 (INR kb))
end
