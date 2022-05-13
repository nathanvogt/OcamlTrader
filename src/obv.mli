(** [obv] is the implementation of the technical indicator on-balance
    volume, which provides a running total of an asset's trading volume
    and indicates whether this volume is flowing in or out of a given
    security or currency pair. The OBV is a cumulative total of volume
    (positive and negative). There are three rules implemented when
    calculating the OBV. They are:

    1. If today's closing price is higher than yesterday's closing
    price, then: Current OBV = Previous OBV + today's volume

    2. If today's closing price is lower than yesterday's closing price,
    then: Current OBV = Previous OBV - today's volume

    3. If today's closing price equals yesterday's closing price, then:
    Current OBV = Previous OBV; Source:
    https://www.investopedia.com/terms/o/onbalancevolume.asp *)

val update_val : int -> float -> int -> float -> string -> int * float
(** [update_val prev_obv prev_close vol close coin] takes in the
    previous OBV [prev_obv] as an [int], the previous close [prev_close]
    as a [float], today's volume [vol] as an [int], today's close price
    as a [float], and the coin name as a [string]; returns a tuple of
    today's close price [close] and today's OBV value [today_obv] in the
    form [(today_obv, close)] *)
