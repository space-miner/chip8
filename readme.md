# chip8

chip8 is a fantasy console created by joe weisbecker in the 70s. originally developed for the cosmac vip, it has gained popularity as a simple system that can run a variety of classic games and applications. the chip8 system has 4kb of addressable memory, 16 general-purpose registers (v0-vf), and supports monochrome graphics and basic input.

![image is from the test suite (i'll add a better one once it can handle real games and has keypad support)](https://img.imgdd.com/f210f3.fd3ba59a-fde9-4ace-8c2f-052e4df6a117.png)

### specifications
* memory: chip-8 has direct access to up to 4 kilobytes of ram
* display: 64 x 32 pixels monochrome -- i.e. black or white
* program counter: pointing at the current instruction in memory
* index register (16-bit): used to point at locations in memory
* stack: for 16-bit addresses used to call subroutines/functions and return from them
* delay timer (8-bit): that decrements at a rate of 60 hz (60 times per second) until it reaches 0
* sound timer (8-bit): that beeps as long as it’s not 0
* v registers (8-bit): 16 general-purpose registers, numbered 0 through f hexadecimal -- usually referred to as (v0-vf)

### features
* chip8 emulation: emulates the chip8 system, allowing you to run chip8 roms.
* graphics: display emulation for chip8 graphics.
* input: support for handling keyboard input.

### todo
- [ ] handle keypad inputs and test roms #5 #6
- [x] refactor to not depend on stdint 
- [ ] consider moving opcode functions out of step
- [ ] move some modules into lib
- [ ] test on other chip8 roms -- e.g. pong
- [ ] add beeps and test on rom #7

