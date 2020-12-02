let day = 1


(* Prebiranje datotek - pobrano iz vzorcne datoteke project_windows.ms*)
let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l
  
  let lines = String.split_on_char '\n'
end


(*reševanje nalog*)

let rec find_the_other first list =
  match list with
  | [] -> 0
  | x :: xs -> if x + first = 2020 then x else find_the_other first xs

let rec find_and_multiply_the_pair list =
  match list with
  | [] -> 0
  | x :: xs -> (if find_the_other x list = 0 then (find_and_multiply_the_pair xs)
   else (find_the_other x list) * x)

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.int_list
  |> find_and_multiply_the_pair 
  |> string_of_int  
    

(* Poženemo zadevo - pobrano iz vzorcne datoteke project_windows.ms *)
let main () =
  let input_data = preberi_datoteko ("data/day_1.in") in
  let part1 = naloga1 input_data in
  print_endline part1;
  izpisi_datoteko ("out/day_" ^ (string_of_int day) ^ "_1.out") part1;
  ()

let _ = main ()