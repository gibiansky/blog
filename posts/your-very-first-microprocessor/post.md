This is the last post in Computing with Transistors, a series of blog posts
describing how computers work from the ground up. The first post and namesake
of the series is available as Computing with Transistors. In the previous post,
we talked about Circuits and Arithmetic. 

Computer processors are, for programmers, almost magical devices that do their
commanded bidding. However, delving a bit deeper and figuring out what is
really going on inside the processor can be an incredibly rewarding experience,
and can help programmers write well-performing code as well as understand how
the code they write actually gets executed. In this code, I'll go over how a
program goes from human-readable form (i.e. assembly language) into a processor
and how the processor executes a program. Most of the things I say are sweeping
generalizations and possibly inaccurate if you scrutinize, but the ideas are
correct and should help you understand how your Intel processor actually works.

Executable Code
---------------

In order for our processor to execute a program we write, we must convert the
program into some form that a digital processor can understand. This is
traditionally done by an assembler - a small program that can convert between
an assembly language and machine code. Assembly language  is usually incredibly
simple, and describes almost exactly what is going on in the processor. For
example, we can have the following assembly command:


    move %eax, 0


In this hypothetical assembly language, this might mean to move the value zero
into a register named "eax". A register is a small bit of memory that exists on
a processor, often made of flip-flops or some other low-latency high-speed
high-area memory. A typical processor will have a few registers, with designs
having as few as ten to as many as a hundred. For instance, if we have three
registers named "eax", "ebx", and "ecx", we could write a few lines of assembly
code to swap values in these registers:


    move %ecx, %eax
    move %eax, %ebx
    move %ebx, %ecx


This code swaps the values in %eax and %ebx, using %ecx as a temporary
placeholder register. Assembly languages will also have commands to do
arithmetic. For instance, a command to add the values in two registers and
store the result in a third register might look like this:


    add %eax, %ebx, %ecx


This would add the values stored in %eax and %ebx and put the result in %ecx.
Using dozens of these small commands to move values around registers, do
arithmetic operations, and others, complete computer programs can be built.

For a processor to execute these commands, we first encode them into machine
code. Machine code is simply a digital representation of assembly language. In
order to encode the command "move %eax, %ebx" into a number, we need to store
three things: the command type (move), the source (%ebx), and the destination
(%eax). Any given assembly language will only have a few dozen commands, so we
can just number them, starting from zero. Similarly, we can number all the
registers from zero onwards. Then, we can agree upon a machine code "protocol",
where in order to represent a command, we first list the command type, then the
source register, and then the destination register, and use an agreed-upon
number of bits for each number. Suppose we have less than eight registers and
less than five commands: then, we can use three bits to represent each register
and two bits for each command. If "move" is the third command in our assembly
language, and %eax and %ebx are the 2nd and 3rd registers, respectively, then
we can encode "move %eax, %ebx" as 3|2|3. In binary, the command becomes "11",
while the registers become "010" and "011", for a complete command of
"11010011". 

Let's make a small processor to read and execute these machine code
instructions. We're going to break our task down, and do three separate things
for each instruction: fetch, execute, and store.

Fetch
-----

The first thing our processor needs to do is read the newest command from
memory. However, we first need to make some memory to store our commands! Let's
create a bit of RAM. In our hypothetical piece of memory, we want to be able to
read a byte of memory every clock cycle. We can make this memory in Verilog. As
input, we take a clock and the address we want to read from. As output, we give
the value at the desired address. In order to read from the memory, we first
have to set the address to our desired location, and then pulse the clock up
and down in order to have the memory update itself. This Verilog module might
look a bit like this:

```verilog
module ram(input clock, input [7:0] address, output reg [7:0] data);

// Storage medium, using Verilog syntax for arrays
reg [7:0] memory[255:0];

always @(posedge clock)
    data <= memory[address];

endmodule
```

Let's create our processor module. The only thing the processor module will
take as input will be a clock; in a real processor, of course, this would have
connections to things such as input devices or LED displays or a multitude of
other things. But we're sticking to simple things.

```verilog
module processor(input clock);
endmodule
```

Let's create a counter. This counter will give us the address from which we
want to read our next instruction. Upon initialization, we'll start with
address zero, and every time we want the next instruction, we'll increment our
address by one.

```verilog
module processor(input clock);

// Instantiate memory, using a counter for the address.
// Start reading at the first byte, address zero.
reg [7:0] address_counter = 8'b0;
wire [7:0] mem_out;
ram memory(clock, address_counter, mem_out);

// Increment the counter when we need to, as defined
// by the flag value increment_counter.
reg increment_counter;
always @(posedge clock)
    if (increment_counter == 1)
        address_counter <= address_counter + 1;

endmodule
```

Note that we don't want to increment the address every cycle! These
instructions will take several cycles to execute, so we have a flag register
telling us whether we want to increment to the next address.

Next, we want to decode the instruction. We need to break it into its component
pieces: the command, the source register, and the destination register. We know
where each of these pieces is because of the way we designed our machine code:
the first two bits are the command, the next three bits are the destination
register, and the last three bits are the source register.

```verilog
wire [1:0] command = mem_out[7:6];
wire [2:0] destination_addr = mem_out[6:3];
wire [2:0] source_addr = mem_out[2:0];
```

Our processor can now fetch the instructions from memory, using a counter to
decide which byte of memory to read, and decode the machine code into its
constituent parts.

Execute
-------

After fetching and decoding our instructions, we need to process them to
actually decide what to do. In our case, we have just one instruction to care
about: move. The move instruction reads a value from one register and then
writes it to another register. The first part of executing this instruction,
then, will be reading the source register.

In order to read the source register, we first need a register file to read it
from! A register file is a small block of memory with only a small number of
available slots. In Verilog, a register file looks a lot like the RAM we
designed earlier. However, in this case we're also going to want to write to
it.

We're going to create a single-ported register file. This means that the
register file just has one input/output port, which is used for both reading
from the file and writing to the file. As before, we're going to use a clock
and an address (three bits, indicating which register we want). We're also
going to use an enable signal to determine whether the register file should be
reading or writing. Finally, we're going to have a two-way input and output
port which can either read data or write data.

```verilog
module regfile(input clock, input [2:0] address, input en_write, inout [7:0] data);

// Register file storage
reg [7:0] registers[7:0];

// Read and write from register file
always @(posedge clock)
    if (en_write)
        registers[address] <= data;
    else
        out_val <= registers[address];

// Output data if not writing. If we are writing,
// do not drive output pins. This is denoted
// by assigning 'z' to the output pins.
reg [7:0] out_val;
assign data = en_write ? 8'bz : out_val;

endmodule
```

In order to use this register file, we have to remember that the data port must
not be used when the write-enable signal is off (and the register file is
reading). In order to tell Verilog that we're not using a data port, we simply
write a special constant to it, denoted by 'z'.

Now that we have a register file, we can read from it by setting the address to
the source register (if the write-enable signal is off).

```verilog
// Flag to indicate whether we are reading or writing
reg reg_write = 0;

// If we are writing to the register file, use the destination register.
// If we are reading from it, use the source register address.
wire [2:0] reg_addr = reg_write ? destination_addr : source_addr;

// If we are reading, don't drive the pins.
wire [7:0] reg_data = reg_write ? write_data : 8'bz;

// Instantiate register file
regfile registers(clock, reg_addr, reg_write, reg_data);

// If we are reading, store the value
reg reading = 1;
reg [7:0] write_data;
always @(posedge clock)
    if (reading)
        write_data <= reg_data;   
```

Control
-------

We now have all the necessary pieces to completely implement the 'move'
command. However, we're still missing the logic! Every clock cycle, we need to
decide what to do based on what we were doing the previous clock cycle. (This
is a simple state machine.)

We start out reading from the main memory (RAM). Once we've read from memory,
we want to update the counter so that on the next read we don't read from the
same place. We can implement this by remembering what state the processor is
currently in, and changing the state every clock cycle based on current state:

```verilog
parameter READING_RAM = 4'd1;
parameter UPDATING_COUNTER = 4'd2;

reg [3:0] state;

always @(posedge clock)
    if (state == READING_RAM)
        state <= UPDATING_COUNTER;
```

Once we've updated the counter, we want to read from the register file.

```verilog
parameter READING_RAM = 4'd1;
parameter UPDATING_COUNTER = 4'd2;
parameter READING_REGISTER = 4'd3;

reg [3:0] state;

always @(posedge clock)
    if (state == READING_RAM)
        state <= UPDATING_COUNTER;
    else if (state == UPDATING_COUNTER)
        state <= READING_REGISTER;
```

Finally, after we've read from the register file, we want to write to the
register file. After we've written to the register file, we start over, once
more reading from memory.

```verilog
parameter READING_RAM = 4'd1;
parameter UPDATING_COUNTER = 4'd2;
parameter READING_REGISTER = 4'd3;
parameter WRITING_REGISTER = 4'd4;

reg [3:0] state;

always @(posedge clock)
    if (state == READING_RAM)
        state <= UPDATING_COUNTER;
    else if (state == UPDATING_COUNTER)
        state <= READING_REGISTER;
    else if (state == READING_REGISTER)
        state <= WRITING_REGISTER;
    else if (state == WRITING_REGISTER)
        state <= READING_RAM;
```

We now know exactly what stages our processor will go through as it executes
each command. However, we're still missing one thing: the logic that controls
the rest of the hardware based on the state. We'd like to change the inputs to
the RAM and register file based on the state that the processor is in.

```verilog
// The @(*) notation means that the signals
// are always updated. This creates a block of combinational logic.
always @(*)
    if (state == UPDATE_COUNTER) begin
        increment_counter <= 1;
        reg_write <= 0;
        reading <= 0;
    end else if (state == WRITING_REGISTER) begin
        increment_counter <= 0;
        reg_write <= 1;
        reading <= 0;
    end else if (state == READING_REGISTER) begin
        increment_counter <= 0;
        reg_write <= 0;
        reading <= 1;
    end else begin
        increment_counter <= 0;
        reg_write <= 0;
        reading <= 0;
    end
```

Once we've built this logic, as above, we have a fully functioning (albeit very
simple) processor! Note that since we only have one command (move), we never
actually use the first two bits. However, in a real processor, we have
additional logic to look at that command and go into different states based on
what that command is. Our processor reads a byte from memory, decodes the byte
to determine what to do and which registers to use, and goes through several
states as it reads and writes the registers indicated by our command.
Congratulations, you've built your first mini-mini-CPU.

Conclusion
----------

This mini-processor is obviously not a good representation of how a real-world
processor works, but it is a starting point. If you're interested in learning
more, I suggest that you design yourself a machine code/assembly language and
create a fully-functioning processor for it. Include commands for movement,
arithmetic, jumping around the instructions, and so on. If you want to test
this on real hardware, you can buy an FPGA, which is a piece of programmable
hardware on which you can imprint the designs from your Verilog code. (The
Altera Cyclone II Dev Starter Kit is a good place to start.) Once you
understand the basics of processors, Coursera has an [outstanding course](https://class.coursera.org/comparch-2012-001) on
modern processor design and computer architecture. Good luck! 
