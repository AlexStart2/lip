open Recognizer  (* Make sure to open your module *)

let%test _ = belongsTo ['0' ; '1'] = [true; true; false; false; false]

let%test _ = belongsTo ['0'; '0'; '0'] = [true; false; true; false; false]

let%test _ = belongsTo ['0';'1';'1';'0'] = [true; false; true; true; false]

let%test _ = belongsTo ['1';'1';'1';'1'] = [true; true; false; false; true]

let%test _ = belongsTo ['1';'0';'1'] = [true; false; false; true; false]

let%test _ = belongsTo [] = [true; true; false; false; true]

let%test _ = belongsTo ['1';'1';'0';'1';'1';'0'] = [true; false; false; false; false]

