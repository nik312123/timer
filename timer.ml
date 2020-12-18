(**
    [Timer] is a function timing framework that is used to time the execution time of expressions and functions
    
    Copyright (C) 2020 Nikunj Chawla
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

(**
    [time_exp_res] takes in an unevaluated expression using a thunk, times how long it takes for the expression to run,
    and returns the evaluated expression and the time it took to run in seconds
    @param exp_thunk The expression to time upon execution of the thunk
    @return A tuple containing the result of the expression and the time it took the expression to run in seconds
*)
let time_exp_res (exp_thunk: unit -> 'a): 'a * float =
    let t_i = Sys.time () in
    let res = exp_thunk () in
    let t_f = Sys.time () in
    (res, t_f -. t_i)

(**
    [time_exp] takes in an unevaluated expression using a thunk, times how long it takes for the expression to run, and
    returns the time it took to run in seconds
    @param exp_thunk The expression to time upon execution of the thunk
    @return The time it took the expression to run in seconds
*)
let time_exp (exp_thunk: unit -> 'a): float = time_exp_res exp_thunk |> snd

(**
    [time_exp_n] takes in an unevaluated expression using a thunk, times how long it takes for the expression to run
    [n] times, and returns the time it took to run in seconds
    @param n         The number of times to test the expression
    @param exp_thunk The expression to time upon execution of the thunk
    @return The time it took the expression to run [n] times in seconds
*)
let time_exp_n (n: int) (exp_thunk: unit -> 'a): float =
    let rec time_exp_n_aux (i: int) (acc: float): float =
        if i <= 0 then acc
        else time_exp_n_aux (i - 1) (acc +. (time_exp exp_thunk))
    in time_exp_n_aux n 0.

(**
    [func_timer_res] takes in an unevaluated, one-argument function using a thunk and its unevaluated input using a
    thunk, times how long it takes for the function to run with the given input, and returns the function result and the
    time it took to run in seconds; it may be useful to only provide the first argument so that you can time the same
    function with various different inputs
    @param fun_thunk   The function thunk to time with the given input upon execution of the thunk
    @param input_thunk The input to the function to time that will be evaluated upon execution of the thunk
    @return A tuple containing the output of the function and the time it took the function to run in seconds with the
    given input
*)
let func_timer_res (fun_thunk: unit -> ('a -> 'b)) (input_thunk: unit -> 'a): 'b * float =
    let t_i = Sys.time () in
    let res = input_thunk () |> fun_thunk () in
    let t_f = Sys.time () in
    (res, t_f -. t_i)

(**
    [func_timer] takes in an unevaluated, one-argument function using a thunk and its unevaluated input using a thunk,
    times how long it takes for the function to run with the given input, and returns the time it took to run in
    seconds; it may be useful to only provide the first argument so that you can time the same function with various
    different inputs
    @param fun_thunk   The function thunk to time with the given input upon execution of the thunk
    @param input_thunk The input to the function to time that will be evaluated upon execution of the thunk
    @return The time it took the function to run in seconds with the given input
*)
let func_timer (fun_thunk: unit -> ('a -> 'b)) (input_thunk: unit -> 'a): float =
    func_timer_res fun_thunk input_thunk |> snd

(**
    [func_timer] takes in an unevaluated, one-argument function using a thunk and its unevaluated input using a thunk,
    times how long  it takes for the function to run with the given input [n] times, and returns the time it took to run
    in seconds; it may be useful to only provide the first argument so that you can time the same function with various
    different inputs
    @param n           The number of times to test the function with the given input
    @param fun_thunk   The function thunk to time with the given input upon execution of the thunk
    @param input_thunk The input to the function to time that will be evaluated upon execution of the thunk
    @return The time it took the function to run [n] times in seconds with the given input
*)
let func_timer_n (n: int) (fun_thunk: unit -> ('a -> 'b)) (input_thunk: unit -> 'a): float =
    let rec func_timer_n_aux (i: int) (acc: float): float =
        if i <= 0 then acc
        else func_timer_n_aux (i - 1) (acc +. (func_timer fun_thunk input_thunk))
    in func_timer_n_aux n 0.
