open Kal

let () = Sys.getenv "HOME" ^ "/txt/diet" |> Diet.load |> Diet.dump
