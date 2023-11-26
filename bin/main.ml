open Base
open Stdio
open Stdint
open Tsdl

module Uint8 = struct
  include Uint8

  let sexp_of_t u8 = Sexp.Atom (Uint8.to_int u8 |> Printf.sprintf "0x%02x")

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> of_string s
    | _ -> failwith "invalid sexp"
  ;;
end

module Uint16 = struct
  include Uint16

  let two = 2 |> Uint16.of_int
  let sexp_of_t u16 = Sexp.Atom (Uint16.to_int u16 |> Printf.sprintf "0%04x")

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> of_string s
    | _ -> failwith "invalid sexp"
  ;;
end

(* loosely following this guide https://tobiasvl.github.io/blog/write-a-chip-8-emulator/ *)
module Font = struct
  let digits =
    [| [| 0xf0; 0x90; 0x90; 0x90; 0xf0 |] (* 0 *)
     ; [| 0x20; 0x60; 0x20; 0x20; 0x70 |] (* 1 *)
     ; [| 0xf0; 0x10; 0xf0; 0x80; 0xf0 |] (* 2 *)
     ; [| 0xf0; 0x10; 0xf0; 0x10; 0xf0 |] (* 3 *)
     ; [| 0x90; 0x90; 0xf0; 0x10; 0x10 |] (* 4 *)
     ; [| 0xf0; 0x80; 0xf0; 0x10; 0xf0 |] (* 5 *)
     ; [| 0xf0; 0x80; 0xf0; 0x90; 0xf0 |] (* 6 *)
     ; [| 0xf0; 0x10; 0x20; 0x40; 0x40 |] (* 7 *)
     ; [| 0xf0; 0x90; 0xf0; 0x90; 0xf0 |] (* 8 *)
     ; [| 0xf0; 0x90; 0xf0; 0x10; 0xf0 |] (* 9 *)
     ; [| 0xf0; 0x90; 0xf0; 0x90; 0x90 |] (* a *)
     ; [| 0xe0; 0x90; 0xe0; 0x90; 0xe0 |] (* b *)
     ; [| 0xf0; 0x80; 0x80; 0x80; 0xf0 |] (* c *)
     ; [| 0xe0; 0x90; 0x90; 0x90; 0xe0 |] (* d *)
     ; [| 0xf0; 0x80; 0xf0; 0x80; 0xf0 |] (* e *)
     ; [| 0xf0; 0x80; 0xf0; 0x80; 0x80 |] (* f *)
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
    (* splice digits into memory at 0x00 *)
    Array.blit
      ~src:digits_flatten
      ~src_pos:0
      ~dst:memory
      ~dst_pos:0
      ~len:(Array.length digits_flatten);
    memory
  ;;

  let load ~rom ~memory =
    (* splice rom into memory at 0x0200 *)
    Array.blit ~src:rom ~src_pos:0 ~dst:memory ~dst_pos:rom_start ~len:(Array.length rom);
    memory
  ;;

  let sexp_of_t memory : Sexp.t =
    Array.fold memory ~init:[] ~f:(fun acc cell ->
      Sexp.Atom (Printf.sprintf "0x%02x" cell) :: acc)
    |> List.rev
    |> Sexp.List
  ;;

  (* read byte *)
  let read_uint8 ~memory ~index = memory.(index)

  let read_uint16 ~memory ~index =
    let fst_u8 = read_uint8 ~memory ~index in
    let snd_u8 = read_uint8 ~memory ~index:(index + 1) in
    (Uint8.to_int fst_u8 lsl 8) + Uint8.to_int snd_u8 |> Uint16.of_int
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
    { mutable pc : Uint16.t
    ; mutable index : Uint16.t
    ; mutable stack : Uint16.t Stack.t
    ; mutable registers : Register.t
    ; mutable memory : Uint8.t array (* idk why it's not happy with type Memory.t *)
    ; mutable display : Display.t
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

  (* utils *)
  let nibbles_of_uint8 u8 =
    let fst_nibble = Uint8.(shift_right (logand u8 (of_int 0xf0)) 4) in
    let snd_nibble = Uint8.(logand u8 (of_int 0x0f)) in
    [| fst_nibble; snd_nibble |]
  ;;

  let nibbles_of_uint16 u16 : Uint8.t array =
    let fst_u8 = Uint16.(shift_right u16 8 |> to_uint8) in
    let snd_u8 = Uint16.(logand u16 (of_int 0x00ff) |> to_uint8) in
    let fst_snd_nibbles = nibbles_of_uint8 fst_u8 in
    let thrd_frth_nibbles = nibbles_of_uint8 snd_u8 in
    [| fst_snd_nibbles.(0); fst_snd_nibbles.(1); thrd_frth_nibbles.(0); thrd_frth_nibbles.(1) |]
  ;;

  let step (state : t) =
    let instr =
      Memory.read_uint16 ~memory:state.memory ~index:(Uint16.to_int state.pc)
    in
    let old_pc = state.pc in
    let err () =
      failwith
        (Printf.sprintf
           "unimplemented: 0x%04x (pc=0x%04x)"
           (Uint16.to_int instr)
           (Uint16.to_int old_pc))
    in
    let _ = state.pc <- Uint16.(state.pc + two) in
    let addr = Uint16.(logand instr (of_int 0x0FFF)) in
    let nibbles = nibbles_of_uint16 instr in
    let op = nibbles.(0) in
    let x = nibbles.(1) in
    let y = nibbles.(2) in
    let n = nibbles.(3) in
    match op with
    | Uint8.of_int 0x0 -> ()
    | Uint8.of_int 0x1 -> ()
    | Uint8.of_int 0x2 -> ()
    | Uint8.of_int 0x3 -> ()
    | Uint8.of_int 0x4 -> ()
    | Uint8.of_int 0x5 -> ()
    | Uint8.of_int 0x6 -> ()
    | Uint8.of_int 0x7 -> ()
    | Uint8.of_int 0x8 -> ()
    | Uint8.of_int 0x9 -> ()
    | Uint8.of_int 0xa -> ()
    | Uint8.of_int 0xb -> ()
    | Uint8.of_int 0xc -> ()
    | Uint8.of_int 0xd -> ()
    | Uint8.of_int 0xe -> ()
    | Uint8.of_int 0xf -> ()
    | _ -> err()
  ;;
end

(* module Timer = struct end *)
(* module Keypad = struct end *)

let () =
  print_endline "Hello, World!";
  let rom = [| 0x12; 0x34; 0xFF; 0xFF |] |> Array.map ~f:Uint8.of_int in
  let memory = Memory.load ~rom ~memory:Memory.init in
  print_s [%sexp (memory : Uint8.t array)]
;;
(* same here, it's not happy with type Memory.t *)
