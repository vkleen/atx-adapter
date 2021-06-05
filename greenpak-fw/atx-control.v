`define RESET 2'b00
`define POWER_ON 2'b01
`define ICE_CONF 2'b10
`define ICE_GOOD 2'b11

typedef enum bit[1:0] { RESET = `RESET, POWER_ON = `POWER_ON, ICE_CONF = `ICE_CONF, ICE_GOOD_FORCE = `ICE_GOOD } COMMAND;

module command_detector ( input logic sdi,
                          input logic sclk,
                          output logic shift,
                          output logic done
                        );
  typedef enum bit[2:0] { IDLE = 0, S0, S01, S010, S0101, S01011, CMD0, CMD1 } STATE;

  STATE cur_state, next_state;

  always_ff @(posedge sclk) begin
    cur_state = next_state;
  end

  always_comb begin
    casez({cur_state, sdi})
      {IDLE,     1'b0}: begin
        next_state = S0;      shift = 0; done = 0;
      end
      {S0,       1'b1}: begin
        next_state = S01;     shift = 0; done = 0;
      end
      {S01,      1'b0}: begin
        next_state = S010;    shift = 0; done = 0;
      end
      {S010,     1'b1}: begin
        next_state = S0101;   shift = 0; done = 0;
      end
      {S0101,    1'b1}: begin
        next_state = S01011;  shift = 0; done = 0;
      end
      {S01011,   1'b0}: begin
        next_state = CMD0;    shift = 0; done = 0;
      end
      {CMD0,     1'b?}: begin
        next_state = CMD1;    shift = 1; done = 0;
      end
      {CMD1,     1'b?}: begin
        next_state = IDLE;    shift = 1; done = 1;
      end
      default: begin
        next_state = IDLE; shift = 0; done = 0;
      end
    endcase
  end
endmodule

module atx_control ( input n_atx_on,
                     input cs_pgood,
                     input wdog_timeout,
                     input ice_cdone,
                     input sdi,
                     input sclk,

                     output reg n_ice_reset,
                     output reg ice_power,
                     output reg main_en,
                     output reg n_cs_en,
                     output reg wdog_inhibit,
                     output reg sdo_enable
                   );
  always_comb begin
    main_en = cs_pgood & ~n_cs_en;
    n_cs_en = n_atx_on;
  end

  reg spi_in_gate;
  logic gated_sdi, gated_sclk;

  always_comb begin
    gated_sclk = spi_in_gate ? sclk : 1'b0;
    gated_sdi = spi_in_gate ? sdi : 1'b0;
  end

  logic shift;
  logic done;
  logic[1:0] state;
  logic[1:0] cmd;
  command_detector detector( .sdi(gated_sdi), .sclk(gated_sclk), .shift(shift), .done(done));

`ifndef SYNTHESIS
  function string cmd_name(bit[1:0] i);
    case(i)
      `RESET: return "RESET";
      `POWER_ON: return "POWER_ON";
      `ICE_CONF: return "ICE_CONF";
      `ICE_GOOD: return "ICE_GOOD_FORCE";
    endcase
  endfunction

  initial begin
    state = `RESET;
  end
`endif

  always @(posedge sclk) begin
    if(shift) begin
      cmd = {cmd[0], sdi};
    end
    if (done) begin
      state = cmd;
      `ifndef SYNTHESIS
        $display("Command received: %s", cmd_name(cmd));
      `endif
    end
    else if (ice_cdone && state === ICE_CONF)
      state = `ICE_GOOD;
  end

`ifndef SYNTHESIS
  always @(posedge wdog_timeout) begin
    state = `RESET;
  end
`endif

  always_comb begin
    case(state)
      `RESET: begin
        n_ice_reset = 0;
        ice_power = 0;
        wdog_inhibit = 1;
        spi_in_gate = 1;
        sdo_enable = 1;
      end
      `POWER_ON: begin
        n_ice_reset = 0;
        ice_power = 1;
        wdog_inhibit = 1;
        spi_in_gate = 1;
        sdo_enable = 1;
      end
      `ICE_CONF: begin
        n_ice_reset = 1;
        ice_power = 1;
        wdog_inhibit = 1;
        spi_in_gate = 0;
        sdo_enable = 1;
      end
      `ICE_GOOD: begin
        n_ice_reset = 1;
        ice_power = 1;
        wdog_inhibit = 0;
        spi_in_gate = 0;
        sdo_enable = 0;
      end
    endcase
  end
endmodule

`ifndef SYNTHESIS
module test;

  bit n_atx_on, cs_pgood, wdog_timeout, ice_cdone, sdi, sclk;
  reg bit n_ice_reset, main_en, n_cs_en, wdog_inhibit, sdo, ice_power;
  atx_control dut( .n_atx_on(n_atx_on),
                   .cs_pgood(cs_pgood),
                   .wdog_timeout(wdog_timeout),
                   .ice_cdone(ice_cdone),
                   .sdi(sdi),
                   .sclk(sclk),

                   .n_ice_reset(n_ice_reset),
                   .main_en(main_en),
                   .n_cs_en(n_cs_en),
                   .wdog_inhibit(wdog_inhibit),
                   .sdo_enable(sdo),
                   .ice_power(ice_power)
                 );

  function bit[0:7] command(COMMAND cmd);
    return {6'b010110, 2'(cmd)};
  endfunction

  bit[0:7] c;
  task send_command(COMMAND cmd);
    send_ones(8);

    c = command(cmd);
    for (int i = 0; i < $size(c); ++i) begin
      #50 sclk = ~sclk;
      sdi = c[i];
      #50 sclk = ~sclk;
    end

    send_ones(8);
  endtask

  task send_junk(int n);
    for (int i = 0; i < n; ++i) begin
      #50 sclk = ~sclk;
      sdi = $random;
      #50 sclk = ~sclk;
    end
    sclk = 1; sdi = 1;
  endtask

  task send_ones(int n);
    for (int i = 0; i < n; ++i) begin
      #50 sclk = ~sclk;
      sdi = 1;
      #50 sclk = ~sclk;
    end
    sclk = 1; sdi = 1;
  endtask

  initial begin
    n_atx_on = 1;
    cs_pgood = 0;
    wdog_timeout = 0;
    ice_cdone = 0;
    sdi = 1;
    sclk = 1;

    $dumpfile("atx-control.vcd");
    $dumpvars(0, test);

    #50 n_atx_on = 0;

    send_command(RESET);
    send_command(POWER_ON);
    send_junk(32);
    send_command(RESET);
    send_command(POWER_ON);
    send_command(ICE_CONF);
    send_junk(32);
    send_command(RESET);
    #100 wdog_timeout = 1;
    #100 wdog_timeout = 0;

    send_command(POWER_ON);
    send_command(ICE_CONF);
    send_junk(32);
    #100 wdog_timeout = 1;
    #100 wdog_timeout = 0;

    send_command(RESET);
    send_command(ICE_GOOD_FORCE);

    #10 n_atx_on = 1;
    #1000 $finish;
  end

  always begin
    @(negedge n_cs_en)
      #100 cs_pgood = 1;
    @(posedge n_cs_en)
      cs_pgood = 0;
  end

  always @(posedge n_ice_reset) begin
    $display("Configuring iCE");
    #400 ice_cdone = 1;
  end

  always @(posedge wdog_timeout) begin
    $display("Watchdog timed out");
    ice_cdone = 0;
  end

endmodule
`endif
