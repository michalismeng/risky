`timescale 1ns/1ns

module main();

    // Toggle the reset line
    reg reset_reg;
    initial begin
        reset_reg = 1;
        reset_reg = #1 0;
        // reset_reg = #2 1;            // ! RST is ACTIVE HIGH !!!
    end
    wire reset = reset_reg;
    
    // Clock line
    reg theClock = 0;
    assign clk = theClock;
    always begin
        #1;
        theClock = !theClock;
    end

    wire [31:0] reg1;
    wire [31:0] reg2;
    wire [31:0] reg3;
    wire [31:0] reg4;
    wire [31:0] reg5;
    wire [31:0] reg6;
    wire [31:0] reg7;
    wire [31:0] reg8;
    wire [31:0] reg9;
    wire [31:0] reg10;
    reg [31:0] counter = 0;

    System_topEntity cpu(clk, reset, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, reg10);

    initial
    begin
        $dumpfile("verilog/dump.vcd");
        $dumpvars(0,main);
    end
    
    always@(posedge clk) begin
            $display("cycle: %d", counter);
            $display("reg1: 0x%h\nreg2: 0x%h\nreg3: 0x%h\nreg4: 0x%h\nreg5: 0x%h\nreg6: 0x%h\nreg7: 0x%h\nreg8: 0x%h\nreg9: 0x%h\n\n\n", reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9);
        counter = counter + 1;
    end

    always@(posedge clk) begin
        if (counter == 200) $finish;
    end

endmodule