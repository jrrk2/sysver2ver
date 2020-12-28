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
let else' = ref UNKNOWN;;
let lft' = ref UNKNOWN;;
let arg1' = ref UNKNOWN;;
let rght' = ref UNKNOWN;;
let entry = ref ("", UNKNOWN, []);;
let op' = ref Aunknown;;

let errlst = ref [];;

let flt' oth = String.concat " " (List.map (tokencnv (ref 0)) oth);;
let rec bin2str w n = (if w > 1 then bin2str (w-1) (n/2) else "")^(if n mod 2 == 1 then "1" else "0")
let rng = function
| BIT :: [] -> 1
| ARNG(hi,lo) :: [] -> hi-lo+1
| oth -> err := flt' (comment oth); failwith "rng"

let dyadic op cellst' lft' rght' =
let arg1 = match lft' with
  | VRF(arg, typ, []) -> "\\"^arg
  | CNST (w, HEX n) -> string_of_int n 
  | oth -> arg1' := oth; failwith "arg1" in
let arg2 = match rght' with CNST (w, HEX n) -> string_of_int n | _ -> "" in
let params = ("A_SIGNED", 0) :: ("A_WIDTH", 5) ::
 ("B_SIGNED", 0) :: ("B_WIDTH", 32) ::
("Y_WIDTH", 5) :: [] in
let arg3 = "\\nxt2_count" in
let connects = ("A", arg1) :: ("B", arg2) :: ("Y", arg3) :: [] in
 cellst' := (op,op^string_of_int (List.length !cellst'), params, connects) :: !cellst';
arg3

let extract cellst' lft' rght' = function
	    | Asub -> dyadic "sub" cellst' lft' rght'
	    | Aadd -> dyadic "add" cellst' lft' rght'
	    | oth -> op' := oth; failwith "op"

let if_always fd cellst' rst then_stmt else_stmt =
    fprintf fd "    switch \\%s\n" rst;
    fprintf fd "      case 1'1\n";
    (match then_stmt with
      | BGN(None, lst) -> (match lst with
        | ASGN(_, _, lft :: rght :: []) :: [] -> let w, n = match lft with
	  | CNST (w, HEX n) -> w, bin2str w n
	  | oth -> lft' := oth; failwith "lft'" in let dst = match rght with
	    | VRF (id, typ, []) -> id
	    | oth -> rght' := oth; failwith "rght'" in
	  fprintf fd "        assign \\nxt_%s %d'%s\n" dst w n;
	| oth -> then' := List.hd oth; failwith "then");
      | oth -> then' := oth; failwith "then");
    fprintf fd "      case \n";
    (match else_stmt with
      | BGN(None, lst) -> (match lst with
        | ASGN(_, _, lft :: rght :: []) :: [] -> let rhs = match lft with
	  | ARITH(_, op, lft' :: rght' :: []) -> extract cellst' lft' rght' op
	  | oth -> then' := oth; failwith "lft" in let dst = match rght with
	    | VRF (id, typ, []) -> id
	    | oth -> rght' := oth; failwith "rght'" in
            fprintf fd "        assign \\nxt_%s %s\n" dst rhs;
	| oth -> then' := List.hd oth; failwith "else");
      | oth -> else' := oth; failwith "else");
    fprintf fd "    end\n"

let stmt_always fd ix cellst' = function 
  | (_, POSPOS (clk, rst), lst) ->
	  fprintf fd "  process $proc_%d\n" ix;
 (match lst with
  | IF(_, cnd :: then_stmt :: else_stmt :: []) :: [] ->  cond' := cnd; if_always fd cellst' rst then_stmt else_stmt
  | oth -> failwith "if");
 fprintf fd "    sync posedge \\%s\n" clk;
 fprintf fd "      update \\count \\nxt_count\n";
 fprintf fd "    sync posedge \\%s\n" rst;
 fprintf fd "      update \\count \\nxt_count\n";
 fprintf fd "  end\n"
  | (_, POSEDGE (clk), lst) ->
	  fprintf fd "  process $proc_%d\n" ix;
 (match lst with
  | IF(_, cnd :: then_stmt :: else_stmt :: []) :: [] -> if_always fd cellst' "reset" then_stmt else_stmt
  | oth -> failwith "if");
 fprintf fd "    sync posedge \\%s\n" clk;
 fprintf fd "      update \\count \\nxt_count\n";
| oth -> entry := oth; failwith "sentry"

let iter fd xmlf =
  let _ = Vxml.translate errlst xmlf in
  Hashtbl.iter (fun nam (_,modul) -> 
	fprintf fd "module \\%s\n" nam;
List.iter (fun (io, (origin, typ', dir, kind', lst)) -> 
  let t,_,_ = findmembers' typ' in
      fprintf fd "  wire %s %d \\%s\n" (dumpdir dir) (rng t) io;
    ) !(modul.io);
List.iter (fun (id, (origin, typ', kind', n)) ->
  let t,_,_ = findmembers' typ' in
fprintf fd "  wire width %d \\%s\n" (rng t) id;
fprintf fd "  wire width %d \\nxt_%s\n" (rng t) id;
fprintf fd "  wire width %d \\nxt2_%s\n" (rng t) id;
) !(modul.v);
let cellst' = ref [] in
List.iteri (fun ix -> stmt_always fd ix cellst') !(modul.alwys);
List.iter (fun (kind,inst,parms,conlst) -> 
  fprintf fd "  cell $%s $%s\n" kind inst;
  List.iter (fun (nam,siz) ->
    fprintf fd "    parameter \\%s %d\n" nam siz) parms;
  List.iter (fun (lft,rght) ->
    fprintf fd "    connect \\%s %s\n" lft rght) conlst) !cellst';
fprintf fd "  end\n";
let asgnlst' = List.map (function
| (_, VRF(dst, typ, []), e) -> dst,flt'(expr modul e)
| oth -> failwith "assign") !(modul.ca) in
	List.iter (fun (lft, rght) ->
	  fprintf fd "  connect \\%s \\%s\n" lft rght) asgnlst';
	fprintf fd "end\n";
	fprintf fd "\n") Vxml.modules;;

