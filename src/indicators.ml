type day = {
  date : int * int * int;
  open_price : float;
  high : float;
  low : float;
  close : float; 
  volume : int;
}

let smoothing = 2

let sma lst = 
  let sum = List.fold_right (fun total x -> total +. x) lst 0. in sum /. float_of_int (List.length lst)