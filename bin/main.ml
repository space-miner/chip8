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

module Registers = struct
  type t = Uint8.t array [@@deriving sexp]

  let init = Array.create ~len:16 Uint8.zero
  let value t ~register = t.(register)
  let update t ~register ~value = t.(register) <- value
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
    ; mutable registers : Registers.t
    ; mutable memory : Uint8.t array (* idk why it's not happy with type Memory.t *)
    ; mutable display : Display.t
    }
  [@@deriving fields, sexp]

  let init ~rom =
    { pc = Uint16.of_int Memory.rom_start
    ; index = Uint16.zero
    ; stack = Stack.create ()
    ; registers = Registers.init
    ; memory = Memory.load ~rom ~memory:Memory.init
    ; display = Display.init
    }
  ;;

  (* utils *)
  let nibbles_of_uint8 u8 =
    let fst_nibble = Uint8.(shift_right (logand u8 (of_int 0xf0)) 4) in
    let snd_nibble = Uint8.(logand u8 (of_int 0x0f)) in
    fst_nibble, snd_nibble
  ;;

  let nibbles_of_uint16 u16 =
    let fst_u8 = Uint16.(shift_right u16 8 |> to_uint8) in
    let snd_u8 = Uint16.(logand u16 (of_int 0x00ff) |> to_uint8) in
    let fst_nibble, snd_nibble = nibbles_of_uint8 fst_u8 in
    let thrd_nibble, frth_nibble = nibbles_of_uint8 snd_u8 in
    fst_nibble, snd_nibble, thrd_nibble, frth_nibble
  ;;

  let step (state : t) : t =
    let pc_u16 = state.pc in
    let pc_int = Uint16.to_int pc_u16 in
    let fst_u8 = Memory.read_uint8 ~memory:state.memory ~index:pc_int in
    let snd_u8 = Memory.read_uint8 ~memory:state.memory ~index:(pc_int + 1) in
    let instr_u16 = Memory.read_uint16 ~memory:state.memory ~index:pc_int in
    let addr_u16 = Uint16.(logand instr_u16 (of_int 0x0FFF)) in
    let err () =
      failwith
        (Printf.sprintf
           "unimplemented: 0x%04x (pc=0x%04x)"
           (Uint16.to_int instr_u16)
           (Uint16.to_int pc_u16))
    in
    (* update pc to next instruction *)
    state.pc <- Uint16.(state.pc + two);
    let op_u8, x_u8, y_u8, n_u8 = nibbles_of_uint16 instr_u16 in
    let fst = Uint8.to_int fst_u8 in
    let snd = Uint8.to_int snd_u8 in
    let instr = Uint16.to_int instr_u16 in
    let addr = Uint16.to_int addr_u16 in
    let op = Uint8.to_int op_u8 in
    let x = Uint8.to_int x_u8 in
    let y = Uint8.to_int y_u8 in
    let n = Uint8.to_int n_u8 in
    (* reference http://devernay.free.fr/hacks/chip8/C8TECH10.HTM *)
    match op with
    | 0x0 ->
      (match instr with
       | 0x00e0 ->
         state.display <- Display.init;
         state
       | 0x00ee ->
         state.pc <- Stack.pop_exn state.stack;
         state
       | _ -> err ())
    | 0x1 ->
      state.pc <- addr_u16;
      state
    | 0x2 ->
      Stack.push state.stack state.pc;
      state.pc <- addr_u16;
      state
    | 0x3 ->
      let reg_x_u8 = Registers.value state.registers ~register:x in
      if Uint8.compare reg_x_u8 snd_u8 = 0
      then (
        state.pc <- Uint16.(state.pc + two);
        state)
      else state
    | 0x4 ->
      let reg_x_u8 = Registers.value state.registers ~register:x in
      if Uint8.compare reg_x_u8 snd_u8 <> 0
      then (
        state.pc <- Uint16.(state.pc + two);
        state)
      else state
    | 0x5 ->
      let reg_x_u8 = Registers.value state.registers ~register:x in
      let reg_y_u8 = Registers.value state.registers ~register:y in
      if Uint8.compare reg_x_u8 reg_y_u8 = 0
      then (
        state.pc <- Uint16.(state.pc + two);
        state)
      else state
    | 0x6 ->
      Registers.update state.registers ~register:x ~value:snd_u8;
      state
    | 0x7 ->
      let reg_x_u8 = Registers.value state.registers ~register:x in
      Registers.update state.registers ~register:x ~value:Uint8.(reg_x_u8 + snd_u8);
      state
    | 0x8 ->
      (match n with
       | 0x0 ->
         let reg_y_u8 = Registers.value state.registers ~register:y in
         Registers.update state.registers ~register:x ~value:reg_y_u8;
         state
       | 0x1 ->
         let reg_x_u8 = Registers.value state.registers ~register:x in
         let reg_y_u8 = Registers.value state.registers ~register:y in
         Registers.update
           state.registers
           ~register:x
           ~value:Uint8.(logor reg_x_u8 reg_y_u8);
         Registers.update state.registers ~register:0xf ~value:Uint8.zero;
         state
       | 0x2 ->
         let reg_x_u8 = Registers.value state.registers ~register:x in
         let reg_y_u8 = Registers.value state.registers ~register:y in
         Registers.update
           state.registers
           ~register:x
           ~value:Uint8.(logand reg_x_u8 reg_y_u8);
         Registers.update state.registers ~register:0xf ~value:Uint8.zero;
         state
       | 0x3 ->
         let reg_x_u8 = Registers.value state.registers ~register:x in
         let reg_y_u8 = Registers.value state.registers ~register:y in
         Registers.update
           state.registers
           ~register:x
           ~value:Uint8.(logxor reg_x_u8 reg_y_u8);
         Registers.update state.registers ~register:0xf ~value:Uint8.zero;
         state
       | 0x4 -> err ()
       | 0x5 -> err ()
       | 0x6 -> err ()
       | 0x7 -> err ()
       | 0xe -> err ()
       | _ -> err ())
    | 0x9 ->
      let reg_x_u8 = Registers.value state.registers ~register:x in
      let reg_y_u8 = Registers.value state.registers ~register:y in
      if Uint8.compare reg_x_u8 reg_y_u8 <> 0
      then (
        state.pc <- Uint16.(state.pc + two);
        state)
      else state
    | 0xa ->
      state.index <- addr_u16;
      state
    | 0xb ->
      let reg_0_u8 = Registers.value state.registers ~register:0 in
      state.pc <- Uint16.(addr_u16 + of_uint8 reg_0_u8);
      state
    | 0xc -> err ()
    | 0xd -> err ()
    | 0xe -> err ()
    | 0xf -> err ()
    | _ -> err ()
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
