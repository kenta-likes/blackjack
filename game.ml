type card_suit = Cloves | Spades | Hearts | Diamonds

type person = Player | Dealer | End

type result = PlayerBust | HouseBust | PlayerHigh | PlayerLow | Draw

type card = {
  suit : card_suit;
  number : int
}

type card_dealer = {
  mutable d_hand : card list
}

type player = {
  mutable p_hand : card list;
  thresh : float
}

type game = {
  mutable dealer : card_dealer;
  mutable player : player;
  mutable visibleCards : card list;
  mutable deck : card list;
  mutable turn : person
}

let create_deck () =
  let rec make numCard decAccum =
    let rec makeSuit suitList (acc : card list) =
      match suitList with
      | [] -> acc
      | st::tl -> makeSuit tl ({suit = st; number = numCard}::acc) in
    if numCard > 0 then
      make (numCard - 1) (decAccum@(makeSuit [Cloves ; Spades ; Hearts ; Diamonds] []))
    else
      decAccum in
  make 13 []

let rec draw num cards acc =
  if num = 0 then (cards, acc)
  else
    let cardIndex = Random.int (List.length cards) in
    let card = List.nth cards cardIndex in
    let newDeck = List.filter (fun x -> x <> card) cards in
    draw (num - 1) newDeck (card::acc)

let init_game r : game =
  let deck = create_deck () in
  let pl : player = {p_hand = []; thresh = r} in
  let dl : card_dealer = {d_hand = []} in
  let drawPl = draw 2 deck [] in
  let newDeck = fst drawPl in
  pl.p_hand <- snd drawPl;
  let drawDl = draw 2 newDeck [] in
  let newDeck' = fst drawDl in
  dl.d_hand <- snd drawDl;
  {
    dealer = dl;
    player = pl;
    visibleCards = [List.hd (List.tl pl.p_hand); List.hd (List.tl dl.d_hand)];
    deck = newDeck';
    turn = Player
  }

let getValue card =
  if card.number < 10 then card.number
  else 10

let calcProb value visible =
  let deck = create_deck () in
  let deck' = List.filter (fun x -> not (List.mem x visible)) deck in
  let allowed = List.fold_left (fun acc elem -> if getValue elem <= value then acc + 1 else acc) 0 deck' in
  (float allowed) /. (float (List.length deck'))

let player_turn g =
  let score = List.fold_left (fun accum elem -> accum + (getValue elem)) 0 g.player.p_hand in
  let prob = calcProb (21 - score) ((List.hd g.player.p_hand)::g.visibleCards) in
  if prob > g.player.thresh then
  begin
    let (newDeck, newCard) = draw 1 g.deck [] in
    g.deck <- newDeck;
    g.player.p_hand <- g.player.p_hand@newCard;
    g
  end
  else
  begin
   g.turn <- Dealer;
   g
  end


let dealer_turn g = 
  let score = List.fold_left (fun accum elem -> accum + (getValue elem)) 0 g.dealer.d_hand in
  if score > 16 then
  begin
    g.turn <- End;
    g
  end
  else
  begin
    let (newDeck, newCard) = draw 1 g.deck [] in
    g.deck <- newDeck;
    g.dealer.d_hand <- g.dealer.d_hand@newCard;
    g
  end


let rec play_game currGame : result =
  let dlScore = List.fold_left (fun accum elem -> accum + (getValue elem)) 0 currGame.dealer.d_hand in
  let plScore = List.fold_left (fun accum elem -> accum + (getValue elem)) 0 currGame.player.p_hand in
  if plScore > 21 || dlScore > 21 then
    begin
      if plScore > 21 && dlScore > 21 then
        Draw
      else if plScore > 21 then
        PlayerBust
      else
        HouseBust
    end
  else
  match currGame.turn with
  | Player -> play_game (player_turn currGame)
  | Dealer -> play_game (dealer_turn currGame)
  | End -> if dlScore > plScore then PlayerLow
           else if dlScore < plScore then PlayerHigh
           else Draw





