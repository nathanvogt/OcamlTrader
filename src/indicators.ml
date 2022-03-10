type day = {
  date : int * int * int;
  open_price : float;
  high : float;
  low : float;
  close : float;
  volume : int;
}

let smoothing = 2
