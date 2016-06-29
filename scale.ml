module Letter = struct
  type t = A | B | C | D | E | F | G

  let char_of = function A -> 'A' | B -> 'B' | C -> 'C' | D -> 'D' | E -> 'E' | F -> 'F' | G -> 'G'

  let next = function A -> B | B -> C | C -> D | D -> E | E -> F | F -> G | G -> A

  let semitones_between t t' =
    let semitones_to_next = function B | E -> 1 | _ -> 2 in
    let rec aux acc t =
      if t = t'
      then acc
      else aux (semitones_to_next t + acc) (next t)
    in
    aux 0 t
end

module Augmentation = struct
  type kind = Flat | Sharp

  let string_of_kind = function Flat -> "♭" | Sharp -> "♯"

  type t = kind list

  let string_of t = List.map string_of_kind t |> String.concat ""

  let of_semitones n =
    Array.(make (abs n) (if n < 0 then Flat else Sharp) |> to_list)

  let semitones_of = List.length
end

module Note = struct
  type t = {letter: Letter.t;  aug: Augmentation.t}

  let string_of t = Printf.sprintf "%c%s"
    (Letter.char_of t.letter)
    (Augmentation.string_of t.aug)

  let next_of_semitones t n =
    let letter = Letter.next t.letter in
    let letter_gap = Letter.semitones_between t.letter letter in
    let aug = Augmentation.(n - (letter_gap - semitones_of t.aug) |> of_semitones) in
    {letter; aug}
end

type interval = int (* number of semitones *)

type scale = Note.t * interval list

let rotate_by_one = function [] -> [] | hd::tl -> tl @ [hd]

let notes_of_scale scale =
  let rec aux acc = function
  | _, [] -> List.rev acc
  | note, x::xs -> aux (note::acc) ((Note.next_of_semitones note x), xs)
  in
  aux [] scale

(* Actual scale definitions *)
let ionian = [2; 2; 1; 2; 2; 2; 1]
let dorian = rotate_by_one ionian
let phrygian = rotate_by_one dorian
let lydian = rotate_by_one phrygian
let mixolydian = rotate_by_one lydian
let aeolian = rotate_by_one mixolydian
let locrian = rotate_by_one aeolian

let _ =
  let all_letters = Letter.([A; B; C; D; E; F; G]) in
  let all_modes = [ionian; dorian; phrygian; lydian; mixolydian; aeolian; locrian] in
  List.iter (fun letter ->
    let note = {Note.letter; aug=[]} in
    List.iter (fun mode ->
      let scale = (note, mode) in
      notes_of_scale scale |> List.map Note.string_of |> String.concat " "
      |> print_endline
    ) all_modes
  ) all_letters
