open Vxml
open Template_ilang

let _ = Printexc.record_backtrace true;;
let ilang = open_out Sys.argv.(2) in
let _ = iter ilang Sys.argv.(1) in close_out ilang;;
List.iter (fun l -> Printf.printf "%d:" (List.length l)) [
!exprothlst;
!stmtothlst;
!portothlst;
!iothlst;
List.flatten (!csothlst);
!bgnothlst;
!itmothlst;
!catothlst;
!cellothlst;
List.flatten (!posneglst);
];
Printf.printf "%d\n" (List.length !errlst);
()

