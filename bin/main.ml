open Base
open Stdio

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
  ;;
end

module Memory = struct
  type t = int array [@@deriving sexp]

  let size = 4 * 1024
  let rom_start = 0x0200

  let init =
    let memory = Array.init size ~f:(Fn.const 0) in
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

  let load rom memory =
    Array.blit ~src:rom ~src_pos:0 ~dst:memory ~dst_pos:rom_start ~len:(Array.length rom);
    memory
  ;;

  let sexp_of_t memory =
    Array.fold memory ~init:[] ~f:(fun acc cell ->
      Sexp.Atom (Printf.sprintf "0x%02X" cell) :: acc)
    |> List.rev
    |> Sexp.List
  ;;
end

module Display = struct
  type t = bool array array

  let width = 64
  let height = 32
  let init = Array.make_matrix ~dimx:width ~dimy:height false
end

let () =
  print_endline "Hello, World!";
  let rom = [| 0x12; 0x34; 0xFF; 0xFF |] in
  let memory = Memory.load rom Memory.init in
  print_s [%sexp (memory : Memory.t)]
;;
