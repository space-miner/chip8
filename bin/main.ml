open Base
open Stdio
open Tsdl

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
  ;;
end

module Memory = struct
  (* memory_value are uint8 *)
  type memory_value = int [@@deriving sexp]
  type t = memory_value array [@@deriving sexp]

  let size = 4 * 1024
  let rom_start = 0x0200

  let init () =
    let memory = Array.init size ~f:(Fn.const 0) in
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

  (* splice rom into memory at 0x0200 *)
  let load ~rom ~memory : t =
    Array.blit ~src:rom ~src_pos:0 ~dst:memory ~dst_pos:rom_start ~len:(Array.length rom);
    memory
  ;;

  let read_byte ~memory ~index = memory.(index)

  let read_instruction ~memory ~index =
    let first_byte = read_byte ~memory ~index in
    let second_byte = read_byte ~memory ~index:(index + 1) in
    (first_byte lsl 8) + second_byte
  ;;

  let sexp_of_t memory : Sexp.t =
    Array.fold memory ~init:[] ~f:(fun acc cell ->
      Sexp.Atom (Printf.sprintf "0x%02x" cell) :: acc)
    |> List.rev
    |> Sexp.List
  ;;

  let get t ~index = t.(index)
  let set t ~index ~value = t.(index) <- value
end

module Registers = struct
  (* register_value are uint8 *)
  type register_value = int [@@deriving sexp]
  type t = register_value array [@@deriving sexp]

  let init () = Array.create ~len:16 0
  let get t ~register = t.(register)
  let set t ~register ~value = t.(register) <- value
end

module Display = struct
  type pixel = bool [@@deriving sexp]
  type t = pixel array array [@@deriving sexp]

  let width = 64
  let height = 32
  let init () = Array.make_matrix ~dimx:height ~dimy:width false

  let show display =
    let pixel_to_string p = if p then "@" else " " in
    let row_to_string r =
      Array.fold r ~init:"" ~f:(fun acc p -> acc ^ pixel_to_string p)
    in
    let display_to_string d =
      Array.map d ~f:(fun r -> row_to_string r)
      |> Array.to_list
      |> String.concat ~sep:"\n"
    in
    print_endline (display_to_string display)
  ;;

  let get t ~row ~col = t.(row).(col)

  let set t ~row ~col ~value =
    if 0 <= row && row < height && 0 <= col && col < width then t.(row).(col) <- value
  ;;
end

module Chip8 = struct
  type t =
    { mutable pc : int (* u16 *)
    ; mutable index : int (* u16 *)
    ; mutable stack : int Stack.t (* u16 stack *)
    ; mutable registers : Registers.t
    ; mutable memory : Memory.t (* u8 array *)
    ; mutable display : Display.t (* bool array array *)
    ; mutable delay_timer : int (* u8 *)
    ; mutable sound_timer : int (* u8 *)
    ; mutable key : int option (* u8 option) *)
    }
  [@@deriving fields, sexp]

  let init ~rom : t =
    { pc = Memory.rom_start
    ; index = 0
    ; stack = Stack.create ()
    ; registers = Registers.init ()
    ; memory = Memory.load ~rom ~memory:(Memory.init ())
    ; display = Display.init ()
    ; delay_timer = 0
    ; sound_timer = 0
    ; key = None
    }
  ;;

  let nibbles_of_byte byte =
    let first_nibble = (byte land 0xf0) lsr 4 in
    let second_nibble = byte land 0x0f in
    first_nibble, second_nibble
  ;;

  let nibbles_of_instruction instruction =
    let first_byte = instruction lsr 8 in
    let second_byte = instruction land 0x00ff in
    let first_nibble, second_nibble = nibbles_of_byte first_byte in
    let third_nibble, fourth_nibble = nibbles_of_byte second_byte in
    first_nibble, second_nibble, third_nibble, fourth_nibble
  ;;

  let step state =
    let pc = state.pc in
    let instr = Memory.read_instruction ~memory:state.memory ~index:pc in
    let addr = instr land 0x0fff in
    let err () = failwith (Printf.sprintf "unimplemented: 0x%04x (pc=0x%04x)" instr pc) in
    state.pc <- state.pc + 2;
    let op, x, y, n = nibbles_of_instruction instr in
    let kk = Memory.read_byte ~memory:state.memory ~index:(pc + 1) in
    let reg_x = Registers.get state.registers ~register:x in
    let reg_y = Registers.get state.registers ~register:y in
    print_endline (Printf.sprintf "Processing: 0x%04x (pc=0x%04x)" instr pc);
    match op with
    | 0x0 ->
      (match instr with
       | 0x00e0 -> state.display <- Display.init ()
       | 0x00ee -> state.pc <- Stack.pop_exn state.stack
       | _ -> err ())
    | 0x1 -> state.pc <- addr
    | 0x2 ->
      Stack.push state.stack state.pc;
      state.pc <- addr
    | 0x3 -> if reg_x = kk then state.pc <- state.pc + 2
    | 0x4 -> if reg_x <> kk then state.pc <- state.pc + 2
    | 0x5 -> if reg_x = reg_y then state.pc <- state.pc + 2
    | 0x6 -> Registers.set state.registers ~register:x ~value:kk
    | 0x7 -> Registers.set state.registers ~register:x ~value:((reg_x + kk) % 0x100)
    | 0x8 ->
      (match n with
       | 0x0 -> Registers.set state.registers ~register:x ~value:reg_y
       | 0x1 ->
         Registers.set state.registers ~register:x ~value:(reg_x lor reg_y);
         Registers.set state.registers ~register:0xf ~value:0
       | 0x2 ->
         Registers.set state.registers ~register:x ~value:(reg_x land reg_y);
         Registers.set state.registers ~register:0xf ~value:0
       | 0x3 ->
         Registers.set state.registers ~register:x ~value:(reg_x lxor reg_y);
         Registers.set state.registers ~register:0xf ~value:0
       | 0x4 ->
         let x_plus_y = reg_x + reg_y in
         let carry = x_plus_y / 0xff in
         Registers.set state.registers ~register:x ~value:(x_plus_y % 0x100);
         Registers.set state.registers ~register:0xf ~value:carry
       | 0x5 ->
         let x_minus_y = reg_x - reg_y in
         let borrow = if reg_x >= reg_y then 1 else 0 in
         Registers.set state.registers ~register:x ~value:((x_minus_y + 0x100) % 0x100);
         Registers.set state.registers ~register:0xf ~value:borrow
       | 0x6 ->
         let lsb = reg_x land 0x001 in
         Registers.set state.registers ~register:x ~value:(reg_x lsr 1);
         Registers.set state.registers ~register:0xf ~value:lsb
       | 0x7 ->
         let y_minus_x = reg_y - reg_x in
         let borrow = if reg_y >= reg_x then 1 else 0 in
         Registers.set state.registers ~register:x ~value:((y_minus_x + 0x100) % 0x100);
         Registers.set state.registers ~register:0xf ~value:borrow
       | 0xe ->
         let msb = (reg_x land 0x80) lsr 7 in
         Registers.set state.registers ~register:x ~value:((reg_x lsl 1) % 0x100);
         Registers.set state.registers ~register:0xf ~value:msb
       | _ -> err ())
    | 0x9 -> if reg_x <> reg_y then state.pc <- state.pc + 2
    | 0xa -> state.index <- addr
    | 0xb ->
      let reg_0 = Registers.get state.registers ~register:0 in
      state.pc <- (addr + reg_0) % 0xffff
    | 0xc ->
      let rand = Random.int 256 in
      Registers.set state.registers ~register:x ~value:(rand land kk)
    | 0xd ->
      let y = reg_y % Display.height in
      let x = reg_x % Display.width in
      let collision = ref false in
      for dy = 0 to n - 1 do
        let byte = state.memory.(state.index + dy) in
        for dx = 0 to 7 do
          let row = y + dy in
          let col = x + dx in
          if 0 <= row && row < Display.height && 0 <= col && col < Display.width
          then (
            let sprite_pixel = byte land (0x80 lsr dx) <> 0 in
            let screen_pixel = Display.get state.display ~row ~col in
            let collide = Bool.(sprite_pixel = screen_pixel) in
            if collide then collision := true;
            Display.set state.display ~row ~col ~value:Bool.(sprite_pixel <> screen_pixel))
        done
      done;
      Registers.set state.registers ~register:0xf ~value:(if !collision then 1 else 0)
    | 0xe ->
      (match kk with
       | 0x9e -> if Option.is_some state.key then state.pc <- state.pc + 2
       | 0xa1 -> if Option.is_none state.key then state.pc <- state.pc + 2
       | _ -> err ())
    | 0xf ->
      (match kk with
       | 0x07 -> Registers.set state.registers ~register:x ~value:state.delay_timer
       | 0x0a ->
         (match state.key with
          | Some k -> ()
          | None -> state.pc <- state.pc - 2)
       | 0x15 -> state.delay_timer <- reg_x
       | 0x18 -> state.sound_timer <- reg_x
       | 0x1e -> state.index <- (state.index + reg_x) % 0x10000
       | 0x29 -> state.index <- reg_x * 5
       | 0x33 ->
         let ones = reg_x % 10 in
         let tens = reg_x / 10 % 10 in
         let hundreds = reg_x / 100 in
         let bcd_array = [| hundreds; tens; ones |] in
         Array.blit
           ~src:bcd_array
           ~src_pos:0
           ~dst:state.memory
           ~dst_pos:state.index
           ~len:3
       | 0x55 ->
         for i = 0 to x do
           let reg_i = Registers.get state.registers ~register:i in
           Memory.set state.memory ~index:(state.index + i) ~value:reg_i
         done
       | 0x65 ->
         for i = 0 to x do
           let memory_i = Memory.get state.memory ~index:(state.index + i) in
           Registers.set state.registers ~register:i ~value:memory_i
         done
       | _ -> err ())
    | _ -> err ()
  ;;

  let or_exit = function
    | Error (`Msg e) ->
      Sdl.log "%s" e;
      Caml.exit 1
    | Ok x -> x
  ;;

  let init_graphics () =
    Sdl.init Sdl.Init.video |> or_exit;
    let window = Sdl.create_window ~w:640 ~h:320 "Caml8" Sdl.Window.opengl |> or_exit in
    let renderer = Sdl.create_renderer window ~index:(-1) |> or_exit in
    Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff |> or_exit;
    renderer
  ;;

  let clear_graphics renderer =
    Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xff |> or_exit;
    Sdl.render_clear renderer |> or_exit
  ;;

  let draw_graphics state renderer =
    Sdl.set_render_draw_color renderer 0xff 0xff 0xff 0xff |> or_exit;
    Array.iteri state.display ~f:(fun i row ->
      Array.iteri row ~f:(fun j pixel ->
        if pixel
        then (
          let rect = Sdl.Rect.create ~x:(10 * j) ~y:(10 * i) ~w:10 ~h:10 in
          Sdl.render_fill_rect renderer (Some rect) |> or_exit)));
    Sdl.render_present renderer
  ;;

  let handle_input state event =
    if Sdl.poll_event (Some event)
    then (
      match Sdl.Event.(get event typ |> enum) with
      | `Key_down ->
        (match Sdl.Event.(get event keyboard_scancode) |> Sdl.Scancode.enum with
         | `K1 -> state.key <- Some 0x1
         | `K2 -> state.key <- Some 0x2
         | `K3 -> state.key <- Some 0x3
         | `K4 -> state.key <- Some 0xc
         | `Q -> state.key <- Some 0x4
         | `W -> state.key <- Some 0x5
         | `E -> state.key <- Some 0x6
         | `R -> state.key <- Some 0xd
         | `A -> state.key <- Some 0x7
         | `S -> state.key <- Some 0x8
         | `D -> state.key <- Some 0x9
         | `F -> state.key <- Some 0xe
         | `Z -> state.key <- Some 0xa
         | `X -> state.key <- Some 0x0
         | `C -> state.key <- Some 0xb
         | `V -> state.key <- Some 0xf
         | `Escape -> Caml.exit 0
         | _ -> ())
      | `Key_up -> state.key <- None
      | `Quit -> Caml.exit 0
      | _ -> ())
  ;;

  let rec run state event renderer =
    step state;
    Display.show state.display;
    if Option.is_some state.key
    then print_endline (Printf.sprintf "Pressing key: %d" (Option.value_exn state.key));
    clear_graphics renderer;
    draw_graphics state renderer;
    handle_input state event
  ;;
end

let read_rom () : int array =
  let bytes = In_channel.input_all In_channel.stdin in
  String.to_array bytes |> Array.map ~f:Char.to_int
;;

let () =
  let last_tick = ref 0. in
  let rom = read_rom () in
  let state = Chip8.init ~rom in
  let event = Sdl.Event.create () in
  let renderer = Chip8.init_graphics () in
  while true do
    if Float.(Unix.gettimeofday () -. !last_tick >= 1. /. 360.)
    then (
      Chip8.run state event renderer;
      last_tick := Unix.gettimeofday ())
  done
;;
