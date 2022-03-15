let smoothing = 2

exception OutOfRange

let rec sublist start stop lst =
  match lst with
  | [] -> raise OutOfRange
  | h :: t ->
      let tail =
        if stop = 0 then [] else sublist (start - 1) (stop - 1) t
      in
      if start > 0 then tail else h :: tail

let avg lst =
  let sum = List.fold_right (fun total x -> total +. x) lst 0. in
  sum /. float_of_int (List.length lst)

let rec sma (lst : float list) (n : int) : float list =
  match lst with
  | [] -> []
  | h :: t ->
      if List.length lst < n then []
      else avg (sublist 0 (n - 1) lst) :: sma t n

let rec ema_recurse (lst : float list) (n : int) (yesterday : float) :
    float list =
  match lst with
  | [] -> []
  | h :: t ->
      let alpha = float_of_int smoothing /. float_of_int (n + 1) in
      let ema_today = (alpha *. h) +. ((1. -. alpha) *. yesterday) in
      ema_today :: ema_recurse t n ema_today

let ema lst n =
  match lst with
  | [] -> []
  | _ ->
      let first = avg (sublist 0 (n - 1) lst) in
      first :: ema_recurse (sublist n (List.length lst - 1) lst) n first

let rec diff lst =
  match lst with
  | [] -> []
  | [ h ] -> []
  | h :: i :: t -> (i -. h) :: diff (i :: t)

let rec gain_loss lst (gain, loss) =
  match lst with
  | [] -> (gain, loss)
  | h :: t ->
      if h >= 0. then gain_loss t (h +. gain, loss)
      else gain_loss t (gain, h +. loss)
