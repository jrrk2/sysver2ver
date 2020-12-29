open Printf
open Vxml

let f k = Hashtbl.find Vxml.modules k;;
let p k = Hashtbl.find Vxml.packages k;;
let d f = Vxml.debug f (Hashtbl.find Vxml.modules f)
let h k = Hashtbl.find Vxml.hierarchy k;;
let dumps s = "\""^s^"\""
let dumpstrlst lst = "["^String.concat ";\n\t" (List.map dumps lst)^"]"

let rec dumpdir = function
| Dinput -> "input"
| Doutput -> "output"
| Dinout -> "inout"
| Dvif s -> "Dvif "^dumps !s
| Dinam str -> "Dinam "^dumps str
| Dport(str1, int1, dirop, str2, str_lst) ->
     "Dport("^dumps str1 ^", "^ string_of_int int1 ^", "^ dumpdir dirop ^", "^ dumps str2 ^", "^ dumpstrlst str_lst^")"
| Dunknown -> "Dunknown"

let err = ref "";;
let cond' = ref UNKNOWN;;
let then' = ref UNKNOWN;;
let then'' = ref UNKNOWN;;
let then''' = ref UNKNOWN;;
let then'''' = ref UNKNOWN;;
let then''''' = ref UNKNOWN;;
let else' = ref UNKNOWN;;
let lft' = ref UNKNOWN;;
let rght' = ref UNKNOWN;;
let entry = ref ("", UNKNOWN, [], []);;

let errlst = ref [];;

let flt' oth = String.concat " " (List.map (tokencnv (ref 0)) oth);;
let rng = function
| BIT :: [] -> 1
| ARNG(hi,lo) :: [] -> hi-lo+1
| oth -> err := flt' (comment oth); failwith "rng"

let if_always fd rst then_stmt else_stmt =
    fprintf fd "    switch \\%s\n" rst;
    fprintf fd "      case 1'1\n";
    (match then_stmt with
      | BGN(None, lst) -> List.iter (function
        | ASGN(_, _, lft :: rght :: []) -> let w, n = match lft with
	  | CNST (w, HEX n) -> w, bin2str w n
	  | oth -> lft' := oth; failwith "lft'" in let dst = match rght with
	    | VRF (id, typ, []) -> id
	    | oth -> rght' := oth; failwith "rght'" in
	  fprintf fd "        assign \\nxt_%s %d'%s\n" dst w n;
	| oth -> then' := oth; failwith "then'") lst
      | oth -> then'' := oth; failwith "then''");
    fprintf fd "      case \n";
    (match else_stmt with
      | BGN(None, lst) -> List.iter (function
        | ASGN(_, _, lft :: rght :: []) -> let rhs = match lft with
	  | VRF (id, typ, []) -> "\\"^id
	  | oth -> then''' := oth; failwith "then'''" in let dst = match rght with
	    | VRF (id, typ, []) -> id
	    | oth -> rght' := oth; failwith "rght'" in
            fprintf fd "        assign \\nxt_%s %s\n" dst rhs;
	| oth -> then'''' := oth; failwith "then''''") lst
      | oth -> else' := oth; failwith "else");
    fprintf fd "    end\n"

let stmt_always fd ix = function 
  | (_, POSPOS (clk, rst), lst, targets) ->
	  fprintf fd "  process $proc_%d\n" ix;
 (match lst with
  | IF(_, cnd :: then_stmt :: else_stmt :: []) :: [] ->  cond' := cnd; if_always fd rst then_stmt else_stmt
  | oth -> failwith "if");
 fprintf fd "    sync posedge \\%s\n" clk;
 List.iter (fun itm -> fprintf fd "      update \\%s \\nxt_%s\n" itm itm) targets;
 fprintf fd "    sync posedge \\%s\n" rst;
 List.iter (fun itm -> fprintf fd "      update \\%s \\nxt_%s\n" itm itm) targets;
 fprintf fd "  end\n"
  | (_, POSEDGE (clk), lst, targets) ->
	  fprintf fd "  process $proc_%d\n" ix;
 (match lst with
  | IF(_, cnd :: then_stmt :: else_stmt :: []) :: [] -> if_always fd "reset" then_stmt else_stmt
  | oth -> failwith "if");
 fprintf fd "    sync posedge \\%s\n" clk;
 List.iter (fun itm -> fprintf fd "      update \\%s \\nxt_%s\n" itm itm) targets;
| oth -> entry := oth; failwith "sentry"

let iter fd xmlf =
  let _ = Vxml.translate errlst xmlf in
  Hashtbl.iter (fun nam (_,modul) -> 
	fprintf fd "module \\%s\n" nam;
List.iteri (fun ix (io, (origin, typ', dir, kind', lst)) -> 
  let t,_,_ = findmembers' typ' in
      fprintf fd "  wire width %d %s %d \\%s\n" (rng t) (dumpdir dir) (ix+1) io;
    ) !(modul.io);
List.iter (fun (id, (origin, typ', kind', n)) ->
  let t,_,_ = findmembers' typ' in
fprintf fd "  wire width %d \\%s\n" (rng t) id;
) (List.sort compare (!(modul.v)));
List.iteri (stmt_always fd) !(modul.alwys);
fprintf fd "  end\n";
List.iter (fun (kind,inst,parms,conlst) -> 
  fprintf fd "  cell $%s $%s\n" kind inst;
  List.iter (fun (nam,siz) ->
    fprintf fd "    parameter \\%s %d\n" nam siz) parms;
  List.iter (fun (lft,rght) ->
    fprintf fd "    connect \\%s %s\n" lft rght) conlst;
  fprintf fd "  end\n") !(modul.arith);
let asgnlst' = List.map (function
| (_, VRF(dst, typ, []), e) -> dst,flt'(expr modul e)
| oth -> failwith "assign") !(modul.ca) in
	List.iter (fun (lft, rght) ->
	  fprintf fd "  connect \\%s \\%s\n" lft rght) asgnlst';
	fprintf fd "end\n";
	fprintf fd "\n") Vxml.modules;;

