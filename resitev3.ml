let day = 3


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

let rec get_element list number =
    let sez = [list] in
    match sez with
    | [] -> ""
    | x :: xs -> if number = 0 then x else get_element xs (number - 1)

let rec count_trees list element =
    match list with
    | [] -> 0
    | one_list :: others -> 
        if get_element one_list element = "#" then 1 + count_trees others (element + 3)
        else count_trees others (element + 3)

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.int_list



(* Poženemo zadevo - pobrano iz vzorcne datoteke project_windows.ms *)
let main () =
  let input_data = preberi_datoteko ("data/day_3.in") in
  let part1 = naloga1 input_data in
  print_endline part1;
  izpisi_datoteko ("out/day_" ^ (string_of_int day) ^ "_1.out") part1;
  ()

let _ = main ()