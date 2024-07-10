open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different
     kinds of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe
  let empty_gomoku = Game.empty Game.Game_kind.Omok

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let test_gomoku =
    let open Game in
    empty_gomoku
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 6; column = 6 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 6; column = 7 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 6; column = 4 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 5; column = 6 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 4; column = 8 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 11 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 12 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 8; column = 10 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 7 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    (*|> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0
      }*)
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let print_dashed_line len =
    match len with
    | 3 -> "---------"
    | _ -> "----------------------------------------------------------"
  ;;

  let print_game (game : Game.t) =
    let board_len =
      Game_strategies_common_lib.Game.Game_kind.board_length game.game_kind
    in
    let piece_map =
      List.init (board_len * board_len) ~f:(fun idx ->
        if Map.mem
             game.board
             { Game.Position.row = idx /% board_len
             ; column = idx % board_len
             }
        then
          Game.Piece.to_string
            (Map.find_exn
               game.board
               { Game.Position.row = idx /% board_len
               ; column = idx % board_len
               })
        else " ")
    in
    List.iteri piece_map ~f:(fun idx piece ->
      match (idx + 1) % board_len with
      | 1 -> printf "%s |" piece
      | 0 ->
        printf " %s\n" piece;
        if not (idx + 1 = List.length piece_map)
        then print_endline (print_dashed_line board_len)
      | _ -> printf " %s |" piece)
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_win_for_x_G" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_test_gomoku" =
    print_game test_gomoku;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  let board_positions (game : Game.t) =
    let board_len =
      Game_strategies_common_lib.Game.Game_kind.board_length game.game_kind
    in
    List.init (board_len * board_len) ~f:(fun idx ->
      { Game.Position.row = idx /% board_len; column = idx % board_len })
  ;;

  (* Exercise 1 *)
  let available_moves (game : Game.t) : Game.Position.t list =
    let all_positions = board_positions game in
    List.filter all_positions ~f:(fun position ->
      not (Map.mem game.board position))
  ;;

  let%expect_test "available_win_for_x" =
    let moves = available_moves win_for_x in
    List.iter moves ~f:(fun coord ->
      print_endline (Game.Position.to_string coord));
    [%expect {|
      
      |}];
    return ()
  ;;

  let%expect_test "available_non_win" =
    let moves = available_moves non_win in
    List.iter moves ~f:(fun coord ->
      print_endline (Game.Position.to_string coord));
    [%expect
      {|
      ((row 0) (column 1))
      ((row 0) (column 2)) 
      ((row 1) (column 1))
      ((row 1) (column 2)) 
      ((row 2) (column 1))
      |}];
    return ()
  ;;

  (* Exercise 2 *)
  let neighbors_2 (position : Game.Position.t) =
    let row, col = position.row, position.column in
    [ ( { position with row = row - 1 }
      , ({ position with row = row - 2 }, { position with row = row + 1 }) )
    ; ( { position with column = col + 1 }
      , ( { position with column = col + 2 }
        , { position with column = col - 1 } ) )
    ; ( { row = row - 1; column = col + 1 }
      , ( { row = row - 2; column = col + 2 }
        , { row = row + 1; column = col - 1 } ) )
    ; ( { row = row + 1; column = col + 1 }
      , ( { row = row + 2; column = col + 2 }
        , { row = row - 1; column = col - 1 } ) )
    ]
  ;;

  let neighbors_3 (position : Game.Position.t) =
    let row, col = position.row, position.column in
    [ [ position
      ; { position with row = row - 1 }
      ; { position with row = row + 1 }
      ]
    ; [ position
      ; { position with column = col - 1 }
      ; { position with column = col + 1 }
      ]
    ; [ position
      ; { row = row - 1; column = col - 1 }
      ; { row = row + 1; column = col + 1 }
      ]
    ; [ position
      ; { row = row - 1; column = col + 1 }
      ; { row = row + 1; column = col - 1 }
      ]
    ]
  ;;

  let neighbors_4 (position : Game.Position.t) =
    let row, col = position.row, position.column in
    [ ( [ { position with row = row - 2 }
        ; { position with row = row - 1 }
        ; { position with row = row + 1 }
        ]
      , ({ position with row = row - 3 }, { position with row = row + 2 }) )
    ; ( [ { position with column = col - 2 }
        ; { position with column = col - 1 }
        ; { position with column = col + 1 }
        ]
      , ( { position with column = col - 3 }
        , { position with column = col + 2 } ) )
    ; ( [ { row = row - 2; column = col - 2 }
        ; { row = row - 1; column = col - 1 }
        ; { row = row + 1; column = col + 1 }
        ]
      , ( { row = row - 3; column = col - 3 }
        , { row = row + 2; column = col + 2 } ) )
    ; ( [ { row = row - 2; column = col + 2 }
        ; { row = row - 1; column = col + 1 }
        ; { row = row + 1; column = col - 1 }
        ]
      , ( { row = row - 3; column = col + 3 }
        , { row = row + 2; column = col - 2 } ) )
    ]
  ;;

  let neighbors_5 (position : Game.Position.t) =
    let row, col = position.row, position.column in
    [ [ position
      ; { position with row = row - 1 }
      ; { position with row = row - 2 }
      ; { position with row = row + 1 }
      ; { position with row = row + 2 }
      ]
    ; [ position
      ; { position with column = col - 1 }
      ; { position with column = col - 2 }
      ; { position with column = col + 1 }
      ; { position with column = col + 2 }
      ]
    ; [ position
      ; { row = row - 1; column = col - 1 }
      ; { row = row - 2; column = col - 2 }
      ; { row = row + 1; column = col + 1 }
      ; { row = row + 2; column = col + 2 }
      ]
    ; [ position
      ; { row = row - 1; column = col + 1 }
      ; { row = row - 2; column = col + 2 }
      ; { row = row + 1; column = col - 1 }
      ; { row = row + 2; column = col - 2 }
      ]
    ]
  ;;

  let on_board (game : Game.t) (coord : Game.Position.t) =
    let board_len =
      Game_strategies_common_lib.Game.Game_kind.board_length game.game_kind
    in
    coord.row < board_len
    && coord.row > -1
    && coord.column < board_len
    && coord.column > -1
  ;;

  let complete_line line (game : Game.t) piece =
    List.for_all line ~f:(fun coord ->
      on_board game coord
      && Map.mem game.board coord
      && Game.Piece.equal piece (Map.find_exn game.board coord))
  ;;

  let check_invalid (game : Game.t) : bool =
    let placed_pieces = Map.to_alist game.board in
    let visited = Hash_set.create (module Game.Position) in
    let errors =
      List.filter placed_pieces ~f:(fun (pos, _) ->
        if on_board game pos && not (Hash_set.mem visited pos)
        then (
          Hash_set.add visited pos;
          false)
        else true)
    in
    List.length errors > 0
  ;;

  let evaluate (game : Game.t) : Game.Evaluation.t =
    match check_invalid game with
    | true -> Illegal_move
    | false ->
      let positions = board_positions game in
      let fun_type = ref neighbors_3 in
      if Game.Game_kind.equal game.game_kind Omok
      then fun_type := neighbors_5;
      let checks =
        List.concat_map positions ~f:(fun coord ->
          if Map.mem game.board coord then !fun_type coord else [])
      in
      let result =
        List.find checks ~f:(fun full_line ->
          match full_line with
          | coord :: rest ->
            let piece = Map.find_exn game.board coord in
            complete_line rest game piece
          | _ -> false)
      in
      (match result with
       | Some solution ->
         Game_over
           { winner = Some (Map.find_exn game.board (List.hd_exn solution)) }
       | None -> Game_continues)
  ;;

  let%expect_test "game_over_win_for_x" =
    let status = evaluate win_for_x in
    print_s [%message (status : Game.Evaluation.t)];
    [%expect {|
      (status (Game_over (winner (X))))
      |}];
    return ()
  ;;

  let%expect_test "available_non_win" =
    let status = evaluate non_win in
    print_s [%message (status : Game.Evaluation.t)];
    [%expect {|
      (status Game_continues)
      |}];
    return ()
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let moves = available_moves game in
    List.filter moves ~f:(fun position ->
      let temp_game = place_piece game ~piece:me ~position in
      match evaluate temp_game with
      | Game_over { winner } ->
        (match winner with
         | Some winner -> Game.Piece.equal winner me
         | _ -> false)
      | _ -> false)
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t)
    : Game.Position.t list
    =
    let moves = available_moves game in
    List.filter moves ~f:(fun position ->
      let temp_game = place_piece game ~piece:me ~position in
      match winning_moves ~me:(Game.Piece.flip me) temp_game with
      | _ :: _ -> true
      | _ -> false)
  ;;

  let available_moves_that_do_not_immediately_lose
    ~(me : Game.Piece.t)
    (game : Game.t)
    : Game.Position.t list
    =
    let moves = available_moves game in
    let losing_spots = losing_moves ~me game in
    List.filter moves ~f:(fun position ->
      if Map.mem game.board position
         || List.mem losing_spots position ~equal:Game.Position.equal
      then false
      else true)
  ;;

  let all_peice_positions (game : Game.t) (desired_piece : Game.Piece.t) =
    let positions = Map.to_alist game.board in
    List.filter_map positions ~f:(fun (pos, piece) ->
      match Game.Piece.equal piece desired_piece with
      | true -> Some pos
      | false -> None)
  ;;

  let centralization3 radius (positions : Game.Position.t list) : int =
    let total = ref 0 in
    List.iter positions ~f:(fun pos ->
      let r_val = Int.abs (pos.row - radius) in
      let c_val = Int.abs (pos.column - radius) in
      if r_val = 0 && c_val = 0
      then total := !total + 3
      else total := !total + ((radius - r_val + 1) * (radius - c_val + 1)));
    !total
  ;;

  let centralization5 radius (positions : Game.Position.t list) : int =
    let total = ref 0 in
    List.iter positions ~f:(fun pos ->
      let r_val = Int.abs (pos.row - radius) in
      let c_val = Int.abs (pos.column - radius) in
      total
      := !total
         + (Int.abs (radius - r_val + 1) * Int.abs (radius - c_val + 1)));
    !total
  ;;

  let connectivity5
    (positions : Game.Position.t list)
    (game : Game.t)
    (piece : Game.Piece.t)
    =
    let total = ref 0 in
    let available = available_moves game @ positions in
    List.iter positions ~f:(fun pos ->
      let neighbor2 = neighbors_2 pos in
      let neighbor3 = neighbors_3 pos in
      let neighbor4 = neighbors_4 pos in
      List.iter neighbor2 ~f:(fun (n2, (block1, block2)) ->
        if Map.mem game.board n2
           && Game.Piece.equal piece (Map.find_exn game.board n2)
        then
          if List.mem available block1 ~equal:Game.Position.equal
          then total := !total + 8;
        if List.mem available block2 ~equal:Game.Position.equal
        then total := !total + 8);
      List.iter neighbor3 ~f:(fun n3_pos ->
        if complete_line n3_pos game piece then total := !total + 81);
      List.iter neighbor4 ~f:(fun (n4_pos, (block1, block2)) ->
        if complete_line n4_pos game piece
        then (
          if List.mem available block1 ~equal:Game.Position.equal
          then total := !total + 128;
          if List.mem available block2 ~equal:Game.Position.equal
          then total := !total + 128)));
    !total
  ;;

  let _score _game_state _maximizing_piece = 0

  let score game_state maximizing_piece =
    let state = evaluate game_state in
    match state with
    | Game_over { winner } ->
      (match winner with
       | Some piece ->
         if Game.Piece.equal piece maximizing_piece
         then Int.max_value
         else Int.min_value
       | None -> 0)
    | Game_continues ->
      let max_pieces = all_peice_positions game_state maximizing_piece in
      let min_pieces =
        all_peice_positions game_state (Game.Piece.flip maximizing_piece)
      in
      (match game_state.game_kind with
       | Tic_tac_toe ->
         centralization3 1 max_pieces - centralization3 1 min_pieces
       | Omok ->
         centralization5 7 max_pieces
         - centralization5 7 min_pieces
         + connectivity5 max_pieces game_state maximizing_piece
         - connectivity5
             min_pieces
             game_state
             (Game.Piece.flip maximizing_piece))
    | _ -> 0
  ;;

  let eval game_state =
    let state = evaluate game_state in
    match state with Game_continues -> false | _ -> true
  ;;

  let rec minimax game_state depth maximizing_player piece
    : int * Game.Position.t
    =
    if depth = 0 || eval game_state
    then score game_state piece, { Game.Position.column = 1; row = 0 }
    else (
      let best_position = ref { Game.Position.row = -1; column = -1 } in
      if maximizing_player
      then (
        let value = ref Int.min_value in
        List.iter (available_moves game_state) ~f:(fun position ->
          let temp_game = place_piece game_state ~piece ~position in
          let depth_score, _ = minimax temp_game (depth - 1) false piece in
          if depth_score > !value
          then (
            value := depth_score;
            best_position := position));
        !value, !best_position)
      else (
        let value = ref Int.max_value in
        List.iter (available_moves game_state) ~f:(fun position ->
          let temp_game =
            place_piece game_state ~piece:(Game.Piece.flip piece) ~position
          in
          let depth_score, _ = minimax temp_game (depth - 1) true piece in
          if depth_score < !value
          then (
            value := depth_score;
            best_position := position));
        !value, !best_position))
  ;;

  let decide_move game_state piece =
    let _, position = minimax game_state 3 true piece in
    if Game.Position.equal position { Game.Position.row = -1; column = -1 }
    then (
      print_endline "FAIL";
      List.hd_exn (available_moves game_state))
    else position
  ;;

  let%expect_test "find_best_move" =
    let pos = decide_move test_gomoku Game.Piece.X in
    print_s [%message (pos : Game.Position.t)];
    let new_game =
      place_piece test_gomoku ~piece:Game.Piece.X ~position:pos
    in
    let _pos = decide_move new_game Game.Piece.O in
    [%expect {|
    (pos ((row 0) (column 2)))
    |}];
    return ()
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all
            |> List.map ~f:Game.Piece.to_string
            |> String.concat ~sep:", "))
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: Available nonlosing moves"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let available_moves =
           available_moves_that_do_not_immediately_lose ~me:piece non_win
         in
         print_s [%sexp (available_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:"Exercise 5: making best moves"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let pos = decide_move test_gomoku piece in
         print_s [%message (pos : Game.Position.t)];
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one", exercise_one
      ; "two", exercise_two
      ; "three", exercise_three
      ; "four", exercise_four
      ; "five", exercise_five
      ; "six", exercise_six
      ]
  ;;
end

let _game_rpc =
  Rpc.Rpc.create
    ~name:"ping"
    ~version:0
    ~bin_query:String.bin_t
    ~bin_response:String.bin_t
;;

let handle_take_turn (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  let response =
    { Rpcs.Take_turn.Response.piece = query.you_play
    ; Rpcs.Take_turn.Response.position =
        Exercises.decide_move query.game query.you_play
    }
  in
  return response
;;

let _handle_start_game (_client : unit) (_query : Rpcs.Start_game.Query.t) =
  return Rpcs.Start_game.Response.Game_started
;;

let _handle_game_over (_client : unit) (_query : Rpcs.Game_over.Query.t) =
  return ()
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle_take_turn ]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     (*and _controller = flag "-controller" (required host_and_port) ~doc:"_
       host_and_port of controller"*)
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
       (* We should start listing on the supplied [port], ready to handle
          incoming queries for [Take_turn] and [Game_over]. We should also
          connect to the controller and send a [Start_game] to initiate the
          game. *)
       let%bind server =
         Rpc.Connection.serve
           ~implementations
           ~initial_connection_state:(fun _client_identity _client_addr ->
             (* This constructs the "client" values which are passed to the
                implementation function above. We're just using unit for
                now. *)
             ())
           ~where_to_listen:(Tcp.Where_to_listen.of_port port)
           ()
       in
       Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
