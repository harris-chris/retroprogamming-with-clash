/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.9.0. DO NOT MODIFY.
*/
`default_nettype none
`timescale 100fs/100fs
module topEntity
    ( // Inputs
      input wire  clk // clock


      // Outputs
    , output wire  result
    );
  wire  c$bindCsr;
  // src/JustBlinksNotTypeLevel.hs:28:1-14
  wire signed [63:0] curr1;
  // src/JustBlinksNotTypeLevel.hs:28:1-14
  wire signed [63:0] max1;
  wire [128:0] c$case_alt;
  // src/JustBlinksNotTypeLevel.hs:28:1-14
  wire signed [63:0] curr1_0;
  // src/JustBlinksNotTypeLevel.hs:28:1-14
  wire signed [63:0] max1_0;
  wire [128:0] c$case_alt_0;
  wire [128:0] result_1;
  wire [0:0] c$app_arg;
  // src/JustBlinksNotTypeLevel.hs:51:15-17
  reg [128:0] r = {1'b1,64'sd0,64'sd5};

  // resetGen begin
  // pragma translate_off
  reg  rst;
  localparam reset_period = 1000000 - 10 + (1 * 100000);
  `ifndef VERILATOR
  initial begin
    #1 rst =  1 ;
    #reset_period rst =  0 ;
  end
  `else
  always begin
    // The redundant (rst | ~ rst) is needed to ensure that this is
    // calculated in every cycle by verilator. Without it, the reset will stop
    // being updated and will be stuck as asserted forever.
    rst = $c("this->reset_gen(",reset_period,",true)") & (rst | ~ rst);
  end
  `systemc_interface
  CData reset_gen(vluint32_t reset_period, bool active_high) {
    static vluint32_t to_wait = reset_period;
    static CData reset = active_high ? 1 : 0;
    static bool finished = false;

    if(!finished) {
      if(to_wait == 0) {
        reset = reset == 0 ? 1 : 0;
        finished = true;
      }
      else {
        to_wait = to_wait - 1;
      }
    }

    return reset;
  }
  `verilog
  `endif
  assign c$bindCsr = rst;
  // pragma translate_on
  // resetGen end

  assign curr1 = $signed(r[127:64]);

  assign max1 = $signed(r[63:0]);

  assign c$case_alt = (curr1 == max1) ? {1'b0,64'sd0,max1} : {1'b1,curr1 + 64'sd1,max1};

  assign curr1_0 = $signed(r[127:64]);

  assign max1_0 = $signed(r[63:0]);

  assign c$case_alt_0 = (curr1_0 == max1_0) ? {1'b1,64'sd0,max1_0} : {1'b0,curr1_0 + 64'sd1,max1_0};

  assign result_1 = r[128:128] ? c$case_alt : c$case_alt_0;

  assign c$app_arg = r[128:128] ? 1'b0 : 1'b1;

  assign result = (c$app_arg);

  // register begin
  always @(posedge clk or  posedge  c$bindCsr) begin : r_register
    if ( c$bindCsr) begin
      r <= {1'b1,64'sd0,64'sd5};
    end else begin
      r <= result_1;
    end
  end
  // register end


endmodule

