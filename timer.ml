(**
    [Timer] is a function timing framework that is used to time the execution time of expressions or functions
*)

(**
    [time_exp_res] takes in a lazy expression, times how long it takes for the expression to run, and returns the
    evaluated expression and the time it took to run in seconds
*)
let time_exp_res (exp_lazy: 'a lazy_t): 'a * float =
    let t_i = Sys.time () in
    let res = Lazy.force exp_lazy in
    let t_f = Sys.time () in
    (res, t_f -. t_i)

(**
    [time_exp] takes in a lazy expression, times how long it takes for the expression to run, and returns the time it
    took to run in seconds
*)
let time_exp (exp_lazy: 'a lazy_t): float = time_exp_res exp_lazy |> snd

(**
    [func_timer_res] takes in a lazy function and its input, times how long it takes for the function to run with the
    given input, and returns the function result and the time it took to run in seconds; it may be useful to only
    provide the first argument so that you can time the same function with various different inputs
*)
let func_timer_res (fun_lazy: ('a -> 'b) lazy_t) (input: 'a): 'b * float =
    let t_i = Sys.time () in
    let res = input |> Lazy.force fun_lazy in
    let t_f = Sys.time () in
    (res, t_f -. t_i)

(**
    [func_timer] takes in a lazy function and its input, times how long it takes for the function to run with the given
    input, and returns the time it took to run in seconds; it may be useful to only provide the first argument so that
    you can time the same function with various different inputs
*)
let func_timer (fun_lazy: ('a -> 'b) lazy_t) (input: 'a): float = func_timer_res fun_lazy input |> snd
