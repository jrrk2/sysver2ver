(*
ocamlfind ocamlopt -o example_json example_json.ml -package yojson -linkpkg
*)

let (json: Yojson.Basic.t) =
`Assoc
  [("creator",
    `String "Yosys 0.9 (git sha1 UNKNOWN, clang 12.0.0 -fPIC -Os)");
   ("modules",
    `Assoc
      [("timer",
        `Assoc
          [("attributes",
            `Assoc
              [("cells_not_processed", `Int 1);
               ("src", `String "timer.v:22")]);
           ("ports",
            `Assoc
              [("rdy",
                `Assoc
                  [("direction", `String "output"); ("bits", `List [`Int 2])]);
               ("clk",
                `Assoc
                  [("direction", `String "input"); ("bits", `List [`Int 3])]);
               ("reset",
                `Assoc
                  [("direction", `String "input"); ("bits", `List [`Int 4])])]);
           ("cells",
            `Assoc
              [("$sub$timer.v:31$2",
                `Assoc
                  [("hide_name", `Int 1); ("type", `String "$sub");
                   ("parameters",
                    `Assoc
                      [("A_SIGNED", `Int 0); ("A_WIDTH", `Int 5);
                       ("B_SIGNED", `Int 0); ("B_WIDTH", `Int 32);
                       ("Y_WIDTH", `Int 32)]);
                   ("attributes", `Assoc [("src", `String "timer.v:31")]);
                   ("port_directions",
                    `Assoc
                      [("A", `String "input"); ("B", `String "input");
                       ("Y", `String "output")]);
                   ("connections",
                    `Assoc
                      [("A", `List [`Int 5; `Int 6; `Int 7; `Int 8; `Int 2]);
                       ("B",
                        `List
                          [`String "1"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"; `String "0";
                           `String "0"; `String "0"]);
                       ("Y",
                        `List
                          [`Int 9; `Int 10; `Int 11; `Int 12; `Int 13;
                           `Int 14; `Int 15; `Int 16; `Int 17; `Int 18;
                           `Int 19; `Int 20; `Int 21; `Int 22; `Int 23;
                           `Int 24; `Int 25; `Int 26; `Int 27; `Int 28;
                           `Int 29; `Int 30; `Int 31; `Int 32; `Int 33;
                           `Int 34; `Int 35; `Int 36; `Int 37; `Int 38;
                           `Int 39; `Int 40])])])]);
           ("netnames",
            `Assoc
              [("$0\\count[4:0]",
                `Assoc
                  [("hide_name", `Int 1);
                   ("bits",
                    `List [`Int 41; `Int 42; `Int 43; `Int 44; `Int 45]);
                   ("attributes", `Assoc [("src", `String "timer.v:27")])]);
               ("$sub$timer.v:31$2_Y",
                `Assoc
                  [("hide_name", `Int 1);
                   ("bits",
                    `List
                      [`Int 9; `Int 10; `Int 11; `Int 12; `Int 13; `Int 14;
                       `Int 15; `Int 16; `Int 17; `Int 18; `Int 19; `Int 20;
                       `Int 21; `Int 22; `Int 23; `Int 24; `Int 25; `Int 26;
                       `Int 27; `Int 28; `Int 29; `Int 30; `Int 31; `Int 32;
                       `Int 33; `Int 34; `Int 35; `Int 36; `Int 37; `Int 38;
                       `Int 39; `Int 40]);
                   ("attributes", `Assoc [("src", `String "timer.v:31")])]);
               ("clk",
                `Assoc
                  [("hide_name", `Int 0); ("bits", `List [`Int 3]);
                   ("attributes", `Assoc [("src", `String "timer.v:22")])]);
               ("count",
                `Assoc
                  [("hide_name", `Int 0);
                   ("bits", `List [`Int 5; `Int 6; `Int 7; `Int 8; `Int 2]);
                   ("attributes", `Assoc [("src", `String "timer.v:24")])]);
               ("rdy",
                `Assoc
                  [("hide_name", `Int 0); ("bits", `List [`Int 2]);
                   ("attributes", `Assoc [("src", `String "timer.v:22")])]);
               ("reset",
                `Assoc
                  [("hide_name", `Int 0); ("bits", `List [`Int 4]);
                   ("attributes", `Assoc [("src", `String "timer.v:22")])])])])])];;

let dump x = Yojson.Basic.to_file "example_json.json" x;;
