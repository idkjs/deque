include Steque_internal;

let append = (a, b) => concat(a, b);

let fold_left: type a z. ((z, a) => z, z, t(a)) => z =
  (f, z, T(t)) => {
    let fold_prefix: type b c. ((z, b) => z, z, prefix(b, c)) => z =
      (f, z, p) =>
        switch (p) {
        | [@implicit_arity] P2(a, b) => f(f(z, a), b)
        | [@implicit_arity] P3(a, b, c) => f(f(f(z, a), b), c)
        | [@implicit_arity] P4(a, b, c, d, deq) =>
          let z = f(f(f(f(z, a), b), c), d);
          Dequeue.fold_left(f, z, deq);
        };

    let list_of_suffix = (f, z, s) => Dequeue.fold_left(f, z, s);

    let rec go:
      type b1 b2 c1 c2 k.
        ((z, b1) => z, z, steque(b1, b2, c1, k), kont(b2, c2)) => z =
      (f, z, steque, kont) =>
        switch (steque) {
        | KONT => go_kont(f, z, kont)
        | [@implicit_arity] Triple(p, c, s) =>
          let z = fold_prefix(f, z, p);
          let z = go(go_pair(f), z, c, kont);
          list_of_suffix(f, z, s);
        }

    and go_pair: type b. ((z, b) => z, z, pair(b)) => z =
      (f, z, [@implicit_arity] Pair(p, k)) => {
        let z = fold_prefix(f, z, p);
        go_kont(go_pair(f), z, k);
      }

    and go_kont: type b c. ((z, b) => z, z, kont(b, c)) => z =
      (f, z, kont) =>
        switch (kont) {
        | Suffix(s) => list_of_suffix(f, z, s)
        | [@implicit_arity] Y(c, k) => go(f, z, c, k)
        | [@implicit_arity] Yr(c, k) => go(f, z, c, k)
        | [@implicit_arity] R(c, k) => go(f, z, c, k)
        | [@implicit_arity] G(c, k) => go(f, z, c, k)
        };

    go_kont(f, z, t);
  };

let fold_right: type a z. ((a, z) => z, t(a), z) => z =
  (f, T(t), z) => {
    let fold_prefix: type b c. ((b, z) => z, prefix(b, c), z) => z =
      (f, p, z) =>
        switch (p) {
        | [@implicit_arity] P2(a, b) => f(a, f(b, z))
        | [@implicit_arity] P3(a, b, c) => f(a, f(b, f(c, z)))
        | [@implicit_arity] P4(a, b, c, d, deq) =>
          let z = Dequeue.fold_right(f, deq, z);
          f(a, f(b, f(c, f(d, z))));
        };

    let list_of_suffix = (f, s, z) => Dequeue.fold_right(f, s, z);

    let rec go:
      type b1 b2 c1 c2 k.
        ((b1, z) => z, steque(b1, b2, c1, k), z, kont(b2, c2)) => z =
      (f, steque, z, kont) =>
        switch (steque) {
        | KONT => go_kont(f, kont, z)
        | [@implicit_arity] Triple(p, c, s) =>
          let z = list_of_suffix(f, s, z);
          let z = go(go_pair(f), c, z, kont);
          fold_prefix(f, p, z);
        }

    and go_pair: type b. ((b, z) => z, pair(b), z) => z =
      (f, [@implicit_arity] Pair(p, k), z) => {
        let z = go_kont(go_pair(f), k, z);
        fold_prefix(f, p, z);
      }

    and go_kont: type b c. ((b, z) => z, kont(b, c), z) => z =
      (f, kont, z) =>
        switch (kont) {
        | Suffix(s) => list_of_suffix(f, s, z)
        | [@implicit_arity] Y(c, k) => go(f, c, z, k)
        | [@implicit_arity] Yr(c, k) => go(f, c, z, k)
        | [@implicit_arity] R(c, k) => go(f, c, z, k)
        | [@implicit_arity] G(c, k) => go(f, c, z, k)
        };

    go_kont(f, t, z);
  };

let rev = t => fold_left((t, x) => cons(x, t), empty, t);

let of_dequeue = d => T(Suffix(d));

let make = (n, x) => of_dequeue(Dequeue.make(n, x));

let singleton = x => of_dequeue(Dequeue.singleton(x));

let length = t => fold_left((s, _) => s + 1, 0, t);
