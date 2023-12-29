(* examples from the paper *)
structure Examples =
struct
    open Coexp
    open Classical

    (* example 1 *)
    fun ex1 (f : int -> string) (g : (int, string) either) : int -> (int, string) either =
        fn (x : int) =>
           colam (fn (k : int dual) =>
                     if x = 0 then coapp g k else f x)
    val ex1_1 = ex1 Int.toString (INR "0") 0 (* INR "0" from g *)
    val ex1_2 = ex1 Int.toString (INL 1) 1   (* INR "1" from f *)

    (* example 2 *)
    (* debugging multiplication operator *)
    fun op ** (x, y) =
        (print (Int.toString x ^ " * " ^ Int.toString y ^ " = " ^ Int.toString (x * y)); x * y)
    infix 1 **
    fun trace (s : string) (x : 'a) : 'a =
        (print (s ^ "\n"); x)
    fun listToString (h : int) (t : int list) : string =
        "[" ^ Int.toString h ^ ", " ^ String.concatWith ", " (List.map Int.toString t) ^ "]"

    (* naive implementation *)
    fun mult (l : int list) : int =
        case l of
            [] => 1
          | h :: t => h ** mult t

    (* first version *)
    fun mult1 (l : int list) : int =
        let fun loop [] = 1
              | loop (0 :: t) = trace (" at " ^ listToString 0 t) 0
              | loop (h :: t) = trace (" at " ^ listToString h t) (h ** loop t)
        in loop l
        end

    (* second version, with sums *)
    fun mult2 (l : int list) : (int, int) either =
        let fun loop [] = INR 1
              | loop (0 :: t) = INL 0
              | loop (h :: t) = trace (" at " ^ listToString h t) (mapRight (fn n => h ** n) (loop t))
        in loop l
        end
    fun run_mult2 (l : int list) = codiag (mult2 l)

    (* third version, with coexponential sums *)
    fun mult3 (l : int list) : (int, int) either =
        colam (fn (k : int dual) =>
                  let fun loop [] = 1
                        | loop (0 :: t) = coapp (INL 0) k
                        | loop (h :: t) = trace (" at " ^ listToString h t) (h ** loop t)
                  in loop l
                  end
              )
    fun run_mult3 (l : int list) = codiag (mult3 l)

    (* fourth version, passing around the dual *)
    fun mult4 (l : int list) : int =
        let val ch = colam (fn (k : int dual) => (l, k))
        in let fun loop [] k = 1
                 | loop (0 :: t) k = coapp (INL 0) k
                 | loop (h :: t) k = trace (" at " ^ listToString h t) (h ** loop t k)
           in case ch of
                  INL n => n
                | INR (l, k) => loop l k
           end
        end

    (* fifth version, with throw *)
    fun mult5 (l : int list) : int =
        let val ch = colam (fn (k : int dual) => (l, k))
        in let fun loop [] k = 1
                 | loop (0 :: t) k = throw 0 k
                 | loop (h :: t) k = trace (" at " ^ listToString h t) (h ** loop t k)
           in case ch of
                  INL n => n
                | INR (l, k) => loop l k
           end
        end
end
