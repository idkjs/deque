let () = {
  let steps = 1000;
  for (size in 0 to 500) {
    let lst = List.init(size, i => i);
    let steq = Deque.Steque.of_list(lst);
    let deck = Deque.Deck.of_list(lst);
    let deckrev = Deque.Deckrev.of_list(lst);
    Printf.printf("%i\t", size);

    let t0 = Unix.gettimeofday();
    for (_ in 0 to steps) {
      let _ = lst @ lst;
      ();
    };
    let t1 = Unix.gettimeofday();
    Printf.printf("%f\t", t1 -. t0);

    let t0 = Unix.gettimeofday();
    for (_ in 0 to steps) {
      let _ = List.rev_append(List.rev(lst), lst);
      ();
    };
    let t1 = Unix.gettimeofday();
    Printf.printf("%f\t", t1 -. t0);

    let t0 = Unix.gettimeofday();
    for (_ in 0 to steps) {
      let _ = Deque.Steque.(steq @ steq);
      ();
    };
    let t1 = Unix.gettimeofday();
    Printf.printf("%f\t", t1 -. t0);

    let t0 = Unix.gettimeofday();
    for (_ in 0 to steps) {
      let _ = Deque.Deck.(deck @ deck);
      ();
    };
    let t1 = Unix.gettimeofday();
    Printf.printf("%f\t", t1 -. t0);

    let t0 = Unix.gettimeofday();
    for (_ in 0 to steps) {
      let _ = Deque.Deckrev.(deckrev @ deckrev);
      ();
    };
    let t1 = Unix.gettimeofday();
    Printf.printf("%f\n%!", t1 -. t0);
  };
};
