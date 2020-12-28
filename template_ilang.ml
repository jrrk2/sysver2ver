open Printf

let template_ilang fd nam wirlst cellst proclst asgnlst =
	fprintf fd "module \\%s\n" nam;
	List.iter (fun (w,kind,nam) -> fprintf fd "  wire %s %d \\%s\n" kind w nam) wirlst;
	List.iter (fun (kind,inst,parms,conlst) -> 
	  fprintf fd "  cell $%s $%s\n" kind inst;
	  List.iter (fun (nam,siz) ->
	    fprintf fd "    parameter \\%s %d\n" nam siz) parms;
	  List.iter (fun (lft,rght) ->
	    fprintf fd "    connect \\%s \\%s\n" lft rght) conlst) cellst;
	fprintf fd "  end\n";
	List.iter (fun (procnam, clk, rst) ->
	  fprintf fd "  process $%s\n" procnam;
	  fprintf fd "    assign \\nxt_count \\count\n";
	  fprintf fd "    switch \\reset\n";
	  fprintf fd "      case 1'1\n";
	  fprintf fd "        assign \\nxt_count 5'01111\n";
	  fprintf fd "      case \n";
	  fprintf fd "        assign \\nxt_count \\sub_tmp [4:0]\n";
	  fprintf fd "    end\n";
	  fprintf fd "    sync posedge \\%s\n" clk;
	  fprintf fd "      update \\count \\nxt_count\n";
	  fprintf fd "    sync posedge \\%s\n" rst;
	  fprintf fd "      update \\count \\nxt_count\n";
	  fprintf fd "  end\n") proclst;
	List.iter (fun (lft, rght) ->
	  fprintf fd "  connect \\%s \\%s\n" lft rght) asgnlst;
	fprintf fd "end\n";
	fprintf fd "\n"

