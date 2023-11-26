open Base
open Stdio
open Stdint
open Tsdl

module Uint8 = struct
  include Uint8

  let sexp_of_t u8 = Sexp.Atom (Uint8.to_int u8 |> Printf.sprintf "0x%02X")

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> of_string s
    | _ -> failwith "invalid sexp"
  ;;
end

module Uint16 = struct
  include Uint16

  let sexp_of_t u16 = Sexp.Atom (Uint16.to_int u16 |> Printf.sprintf "0x%02X")

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> of_string s
    | _ -> failwith "invalid sexp"
  ;;
end

(* loosely following this guide https://tobiasvl.github.io/blog/write-a-chip-8-emulator/ *)
module Font = struct
  let digits =
    [| [| 0xF0; 0x90; 0x90; 0x90; 0xF0 |] (* 0 *)
     ; [| 0x20; 0x60; 0x20; 0x20; 0x70 |] (* 1 *)
     ; [| 0xF0; 0x10; 0xF0; 0x80; 0xF0 |] (* 2 *)
     ; [| 0xF0; 0x10; 0xF0; 0x10; 0xF0 |] (* 3 *)
     ; [| 0x90; 0x90; 0xF0; 0x10; 0x10 |] (* 4 *)
     ; [| 0xF0; 0x80; 0xF0; 0x10; 0xF0 |] (* 5 *)
     ; [| 0xF0; 0x80; 0xF0; 0x90; 0xF0 |] (* 6 *)
     ; [| 0xF0; 0x10; 0x20; 0x40; 0x40 |] (* 7 *)
     ; [| 0xF0; 0x90; 0xF0; 0x90; 0xF0 |] (* 8 *)
     ; [| 0xF0; 0x90; 0xF0; 0x10; 0xF0 |] (* 9 *)
     ; [| 0xF0; 0x90; 0xF0; 0x90; 0x90 |] (* A *)
     ; [| 0xE0; 0x90; 0xE0; 0x90; 0xE0 |] (* B *)
     ; [| 0xF0; 0x80; 0x80; 0x80; 0xF0 |] (* C *)
     ; [| 0xE0; 0x90; 0x90; 0x90; 0xE0 |] (* D *)
     ; [| 0xF0; 0x80; 0xF0; 0x80; 0xF0 |] (* E *)
     ; [| 0xF0; 0x80; 0xF0; 0x80; 0x80 |] (* F *)
    |]
    |> Array.map ~f:(Array.map ~f:Uint8.of_int)
  ;;
  (* converts to uint8 *)
end

module Memory = struct
  type t = Uint8.t array [@@deriving sexp]

  let size = 4 * 1024
  let rom_start = 0x0200

  let init =
    let memory = Array.init size ~f:(Fn.const Uint8.zero) in
    let digits_flatten =
      Array.fold Font.digits ~init:[||] ~f:(fun acc digit -> Array.append acc digit)
    in
    Array.blit
      ~src:digits_flatten
      ~src_pos:0
      ~dst:memory
      ~dst_pos:0
      ~len:(Array.length digits_flatten);
    memory
  ;;

  let load ~rom ~memory =
    Array.blit ~src:rom ~src_pos:0 ~dst:memory ~dst_pos:rom_start ~len:(Array.length rom);
    memory
  ;;

  let sexp_of_t memory : Sexp.t =
    Array.fold memory ~init:[] ~f:(fun acc cell ->
      Sexp.Atom (Printf.sprintf "0x%02X" cell) :: acc)
    |> List.rev
    |> Sexp.List
  ;;
end

module Register = struct
  type t = Uint8.t array [@@deriving sexp]

  let init = Array.create ~len:16 Uint8.zero
end

module Display = struct
  type t = bool array array [@@deriving sexp]

  let width = 64
  let height = 32
  let init = Array.make_matrix ~dimx:width ~dimy:height false

  let show display =
    let pixel_of_bool b = if b then "@" else " " in
    let row_to_string r = Array.fold r ~init:"" ~f:(fun acc b -> acc ^ pixel_of_bool b) in
    let display_to_string d =
      Array.map d ~f:(fun r -> row_to_string r)
      |> Array.to_list
      |> String.concat ~sep:"\n"
    in
    print_endline (display_to_string display)
  ;;
end

module Cpu = struct
  type t =
    { pc : Uint16.t
    ; index : Uint16.t
    ; stack : Uint16.t Stack.t
    ; registers : Register.t
    ; memory : Uint8.t array (* idk why it's not happy with type Memory.t *)
    ; display : Display.t
    (* ; delayTimer : Uint8.t *)
    (* ; soundTimer : Uint8.t *)
    }
  [@@deriving fields, sexp]

  let init ~rom =
    { pc = Uint16.of_int Memory.rom_start
    ; index = Uint16.zero
    ; stack = Stack.create ()
    ; registers = Register.init
    ; memory = Memory.load ~rom ~memory:Memory.init
    ; display = Display.init
    }
  ;;

  let step t = failwith "todo"
end

(* module Timer = struct end *)
(* module Keypad = struct end *)

let () =
  print_endline "Hello, World!";
  let rom = [| 0x12; 0x34; 0xFF; 0xFF |] |> Array.map ~f:Uint8.of_int in
  let memory = Memory.load ~rom ~memory:Memory.init in
  print_s [%sexp (memory : Uint8.t array)] (* same here, it's not happy with type Memory.t *)
;;
