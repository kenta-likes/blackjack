
let thresh = ref 0.4
let threshStep = ref 0.05
let runNums = 10000
let nMod = 100
let nWins = ref 0
let nDraws = ref 0

let rec playNGames n =
  if n > runNums then
    (print_string "Win rate: "; print_float ((float !nWins) /. (float runNums)); print_string "\n";
     print_string "Draw rate: "; print_float ((float !nDraws) /. (float runNums)); print_string "\n";
     print_string "Lose rate: "; print_float ((float (runNums - !nWins - !nDraws) /. (float runNums))); print_string "\n";
     !thresh)
  else
    let myGame = init_game !thresh in
    let res = play_game myGame in
    begin
    print_float !thresh;
    print_string "\n";
    match res with
    |PlayerBust -> print_string "Player Bust\n\n";
                   thresh := !thresh +. (!threshStep)
    |PlayerLow -> print_string "Player Low\n\n";
                  thresh := !thresh -. (!threshStep)
    |PlayerHigh -> print_string "Win\n\n"; nWins := !nWins + 1;
    |HouseBust -> print_string "Win\n\n"; nWins := !nWins + 1;
    |Draw -> print_string "Draw\n\n"; nDraws := !nDraws + 1;
    end;
    (if n mod nMod = 0 then threshStep := !threshStep /. 2.
    else ());
    playNGames (n + 1)