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
let op' = ref Aunknown;;
let ops' = ref (UNKNOWN,UNKNOWN);;

let errlst = ref [];;

let flt' oth = String.concat ":" (List.map (tokencnv (ref 0)) oth);;

let iter fd xmlf =
  let _ = Vxml.translate errlst xmlf in
  Hashtbl.iter (fun nam (_,modul) -> 
	fprintf fd "module \\%s\n" nam;
let iolst = List.map (fun (io, (origin, typ', dir, kind', lst)) -> 
    (io,dumpdir dir,findmembers' typ')
    ) !(modul.io) in
let wlst = iolst @ List.map (fun (id, (origin, typ', kind', n)) ->
    (id,"width",findmembers' typ')) !(modul.v) in
let wlst' = List.map (fun (id, dir, (rng,_,_)) -> match rng with 
| BIT :: [] -> (1, dir, id)
| ARNG(hi,lo) :: [] -> (hi-lo+1, dir, id)
| oth -> err := flt' (comment oth); failwith "rng"
) wlst in
  	List.iter (fun (w,kind,nam) -> fprintf fd "  wire %s %d \\%s\n" kind w nam) wlst';
let cellst' = ref [] in
List.iteri (fun ix -> function 
  | (_, POSPOS (clk, rst), lst) ->
	  fprintf fd "  process $proc_%d\n" ix;
	  fprintf fd "    assign \\nxt_count \\count\n";
 (match lst with
  | IF(_, cnd :: then_stmt :: else_stmt :: []) :: [] -> cond' := cnd;
    fprintf fd "    switch \\reset\n";
    fprintf fd "      case 1'1\n";
    (match then_stmt with
      | BGN(None, lst) -> (match lst with
        | ASGN(_, _, lft :: rght :: []) :: [] ->
	  fprintf fd "        assign \\nxt_count 5'01111\n";
	| oth -> then' := List.hd oth; failwith "then");
      | oth -> then' := oth; failwith "then");
    fprintf fd "      case \n";
    (match else_stmt with
      | BGN(None, lst) -> (match lst with
        | ASGN(_, _, lft :: rght :: []) :: [] -> (match lft with
	  | ARITH(_, op, lft' :: rght' :: []) -> (match op with
	    | Asub -> ops' := (lft',rght');
let arg1 = match lft' with VRF(arg, (BASDTYP, "logic", TYPRNG (HEX 4, HEX 0), []), []) -> arg | _ -> failwith "arg1" in
let arg2 = match rght' with CNST (w, HEX n) -> string_of_int n | _ -> "" in
let params = ("A_SIGNED", 0) :: ("A_WIDTH", 5) ::
 ("B_SIGNED", 0) :: ("B_WIDTH", 32) ::
("Y_WIDTH", 32) :: [] in
let connects = ("A", arg1) :: ("B", arg2) :: ("Y", "$Y") :: [] in
 cellst' := ("sub","sub2", params, connects) :: !cellst';
   fprintf fd "        assign \\nxt_count \\sub_tmp [4:0]\n";
	    | oth -> op' := oth; failwith "op")
	  | oth -> then' := oth; failwith "lft")
	| oth -> then' := List.hd oth; failwith "else");
      | oth -> else' := oth; failwith "else");
    fprintf fd "    end\n";
  | oth -> failwith "if");
 fprintf fd "    sync posedge \\%s\n" clk;
 fprintf fd "      update \\count \\nxt_count\n";
 fprintf fd "    sync posedge \\%s\n" rst;
 fprintf fd "      update \\count \\nxt_count\n";
 fprintf fd "  end\n"
| oth -> failwith "sentry") !(modul.alwys);
	List.iter (fun (kind,inst,parms,conlst) -> 
	  fprintf fd "  cell $%s $%s\n" kind inst;
	  List.iter (fun (nam,siz) ->
	    fprintf fd "    parameter \\%s %d\n" nam siz) parms;
	  List.iter (fun (lft,rght) ->
	    fprintf fd "    connect \\%s \\%s\n" lft rght) conlst) !cellst';
	fprintf fd "  end\n";
let asgnlst' = List.map (function
| (_, VRF(dst, typ, []), e) -> dst,flt'(expr modul e)
| oth -> failwith "assign") !(modul.ca) in
	List.iter (fun (lft, rght) ->
	  fprintf fd "  connect \\%s \\%s\n" lft rght) asgnlst';
	fprintf fd "end\n";
	fprintf fd "\n") Vxml.modules;;

