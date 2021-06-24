type m('a) =
  | Done: m('a)
  | Yield('a, unit => m('a)): m('a)
  | Interleave(m('a), m('a)): m('a)
  | Nest(unit => m('a)): m('a)
  | Bind(m('a), 'a => m('b)): m('b)
  | Delay(unit => m('a)): m('a);

let (@) = (a, b) => [@implicit_arity] Interleave(a, b);
let return = x => [@implicit_arity] Yield(x, () => Done);
let (>>=) = (m, f) => [@implicit_arity] Bind(m, f);
let map = (f, m) => [@implicit_arity] Bind(m, x => return(f(x)));
let (<$>) = (f, x) => map(f, x);
let (<*>) = (f, x) => f >>= (f => map(f, x));

let rec of_list = lst =>
  switch (lst) {
  | [] => Done
  | [x, ...xs] => [@implicit_arity] Yield(x, () => of_list(xs))
  };

let rec force1: type a. m(a) => option(a) =
  fun
  | Done => None
  | [@implicit_arity] Yield(x, _) => Some(x)
  | Delay(m) => force1(m())
  | [@implicit_arity] Interleave(a, b) =>
    switch (force1(a)) {
    | None => force1(b)
    | Some(x) => Some(x)
    }
  | [@implicit_arity] Bind(m, f) =>
    switch (force1(m)) {
    | None => None
    | Some(x) => force1(f(x))
    }
  | Nest(m) => force1(m());

let rec iteri: type a. (int, (int, a) => int, int, m(a)) => int =
  (count, f, d, xs) =>
    switch (xs) {
    | Done => count
    | [@implicit_arity] Yield(x, xs) =>
      let count = f(count, x);
      iteri(count + 1, f, d, xs());
    | Delay(m) => iteri(count, f, d, m())
    | [@implicit_arity] Interleave(a, b) =>
      let (a, b) =
        if (Random.bool()) {
          (a, b);
        } else {
          (b, a);
        };
      let count = iteri(count, f, d, a);
      iteri(count, f, d, b);
    | [@implicit_arity] Bind(m, g) =>
      iteri(count, (count, x) => iteri(count, f, d, g(x)), d, m)
    | Nest(m) =>
      if (d <= 0) {
        switch (force1(m())) {
        | None => count
        | Some(x) => f(count, x)
        };
      } else {
        iteri(count, f, d - 1, m());
      }
    };

let iteri = (f, d, xs) => iteri(0, f, d, xs);

module D = Deck_internal;
open D;

let rec list_gen = (size, m) =>
  if (size <= 0) {
    return([]);
  } else {
    let rest = list_gen(size - 1, m);
    m >>= (x => rest >>= (xs => return([x, ...xs])));
  };

let buffer_make = (e, size): m(Buffer.t('a, 's)) =>
  Obj.magic @@ map(Deque.Dequeue.of_list) @@ list_gen(size, e);

let sample =
  fun
  | [] => assert(false)
  | [x] => [x]
  | [x, ...xs] =>
    if (Random.bool()) {
      [x, List.nth(xs, Random.int(List.length(xs)))];
    } else {
      [x];
    };

let buffer_ge8 = (e): m(Buffer.t('a, ge8(z))) =>
  buffer_make(e, 8 + Random.int(6));
let buffer_ge7 = (e): m(Buffer.t('a, ge7(z))) =>
  buffer_make(e, 7 + Random.int(6));
let buffer_ge6 = (e): m(Buffer.t('a, ge6(z))) =>
  buffer_make(e, 6 + Random.int(6));
let buffer_ge5 = (e): m(Buffer.t('a, ge5(z))) =>
  buffer_make(e, 5 + Random.int(6));
let buffer_ge3 = (e): m(Buffer.t('a, ge3(z))) =>
  buffer_make(e, 3 + Random.int(6));
let buffer_eq2 = (e): m(Buffer.t('a, ge2(z))) => buffer_make(e, 2);
let buffer_ge1 = (e): m(Buffer.t('a, ge1(z))) =>
  buffer_make(e, 1 + Random.int(6));

type holy('a, 'preference, 'color) =
  | Holy(
      not_empty('a, 'b, 'preference, 'hole_loc),
      triple('b, 'b, [< | `green | `red] as 'color, 'hole_loc, nh, nh, nh),
    )
    : holy('a, 'preference, 'color);

type unholy('a, 'preference) =
  | Unholy(not_empty('a, 'a, 'preference, nh)): unholy('a, 'preference);

type gr_deque('a) =
  | GR_deq(deque('a, [< | `green | `red])): gr_deque('a);
type g_deque('a) =
  | G_deq(deque('a, [ | `green])): g_deque('a);

type gr_path('a, 'k) =
  | GR_path(path('a, [< | `green | `red], 'k)): gr_path('a, 'k);
type g_path('a, 'k) =
  | G_path(path('a, [ | `green], 'k)): g_path('a, 'k);

let rec stored_triple: type a. m(a) => m(stored_triple(a)) =
  e =>
    if (Random.bool()) {
      Nest(
        () =>
          (
            buffer_ge3(e)
            >>= (
              p =>
                semiregular(stored_triple(e))
                >>= (
                  (S(c)) =>
                    buffer_ge3(e)
                    >>= (s => return([@implicit_arity] Stored(p, c, s)))
                )
            )
          )
          @ (buffer_ge3(e) >>= (p => return(Stored_prefix(p)))),
      );
    } else {
      Nest(
        () =>
          (buffer_ge3(e) >>= (p => return(Stored_prefix(p))))
          @ (
            buffer_ge3(e)
            >>= (
              p =>
                semiregular(stored_triple(e))
                >>= (
                  (S(c)) =>
                    buffer_ge3(e)
                    >>= (s => return([@implicit_arity] Stored(p, c, s)))
                )
            )
          ),
      );
    }

and only_green:
  type a. m(a) => m(triple(a, a, [ | `green], only, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        (buffer_ge1(e) >>= (p => return(Only_prefix(p))))
        @ (
          buffer_ge8(e)
          >>= (
            p =>
              gr_deque(stored_triple(e))
              >>= (
                (GR_deq(c)) =>
                  buffer_ge8(e)
                  >>= (s => return([@implicit_arity] Only_green(p, c, s)))
              )
          )
        ),
    )

and only_red: type a. m(a) => m(triple(a, a, [ | `red], only, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        buffer_ge5(e)
        >>= (
          p =>
            g_deque(stored_triple(e))
            >>= (
              (G_deq(c)) =>
                buffer_ge5(e)
                >>= (s => return([@implicit_arity] Only_red(p, c, s)))
            )
        ),
    )

and only_yellow_green: type a. m(a) => m(path(a, [ | `green], only)) =
  e =>
    Nest(
      () =>
        (only_green(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_ge7(e)
          >>= (
            p =>
              not_empty_left_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge7(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Only_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_ge6(e)
          >>= (
            p =>
              not_empty_right_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge6(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Only_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and only_yellow_red: type a. m(a) => m(path(a, [ | `red], only)) =
  e =>
    Nest(
      () =>
        (only_red(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_ge7(e)
          >>= (
            p =>
              not_empty_left_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge7(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Only_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_ge6(e)
          >>= (
            p =>
              not_empty_right_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge6(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Only_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and left_yellow_green: type a. m(a) => m(path(a, [ | `green], left)) =
  e =>
    Nest(
      () =>
        (left_green(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_ge7(e)
          >>= (
            p =>
              not_empty_left_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_eq2(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Left_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_ge6(e)
          >>= (
            p =>
              not_empty_right_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_eq2(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Left_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and right_yellow_green: type a. m(a) => m(path(a, [ | `green], right)) =
  e =>
    Nest(
      () =>
        (right_green(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_eq2(e)
          >>= (
            p =>
              not_empty_left_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge7(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Right_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_eq2(e)
          >>= (
            p =>
              not_empty_right_green(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge6(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Right_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and left_yellow_red: type a. m(a) => m(path(a, [ | `red], left)) =
  e =>
    Nest(
      () =>
        (left_red(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_ge7(e)
          >>= (
            p =>
              not_empty_left_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_eq2(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Left_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_ge6(e)
          >>= (
            p =>
              not_empty_right_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_eq2(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Left_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and right_yellow_red: type a. m(a) => m(path(a, [ | `red], right)) =
  e =>
    Nest(
      () =>
        (right_red(e) >>= (k => return([@implicit_arity] Path(HOLE, k))))
        @ (
          buffer_eq2(e)
          >>= (
            p =>
              not_empty_left_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge7(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Right_yellow(p, c, s), k),
                      )
                  )
              )
          )
        )
        @ (
          buffer_eq2(e)
          >>= (
            p =>
              not_empty_right_red(stored_triple(e))
              >>= (
                ([@implicit_arity] Holy(c, k)) =>
                  buffer_ge6(e)
                  >>= (
                    s =>
                      return(
                        [@implicit_arity]
                        Path([@implicit_arity] Right_orange(p, c, s), k),
                      )
                  )
              )
          )
        ),
    )

and not_empty_left_red:
  type a. m(a) => m(holy(a, preferred_left, [ | `red])) =
  e =>
    Nest(
      () =>
        (
          left_yellow_red(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              gr_path_right(e)
              >>= (
                (GR_path(right)) =>
                  return(
                    [@implicit_arity]
                    Holy([@implicit_arity] Pair_left(y, right), k),
                  )
              )
          )
        )
        @ (
          only_yellow_red(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return([@implicit_arity] Holy(Only_of(y), k))
          )
        ),
    )

and not_empty_right_red:
  type a. m(a) => m(holy(a, preferred_right, [ | `red])) =
  e =>
    Nest(
      () =>
        (
          g_path_left(e)
          >>= (
            (G_path(left)) =>
              right_yellow_red(e)
              >>= (
                ([@implicit_arity] Path(y, k)) =>
                  return(
                    [@implicit_arity]
                    Holy([@implicit_arity] Pair_right(left, y), k),
                  )
              )
          )
        )
        @ (
          only_yellow_red(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return([@implicit_arity] Holy(Only_of(y), k))
          )
        ),
    )

and not_empty_left_green:
  type a. m(a) => m(holy(a, preferred_left, [ | `green])) =
  e =>
    Nest(
      () =>
        (
          only_yellow_green(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return([@implicit_arity] Holy(Only_of(y), k))
          )
        )
        @ (
          left_yellow_green(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              gr_path_right(e)
              >>= (
                (GR_path(right)) =>
                  return(
                    [@implicit_arity]
                    Holy([@implicit_arity] Pair_left(y, right), k),
                  )
              )
          )
        ),
    )

and not_empty_right_green:
  type a. m(a) => m(holy(a, preferred_right, [ | `green])) =
  e =>
    Nest(
      () =>
        (
          only_yellow_green(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return([@implicit_arity] Holy(Only_of(y), k))
          )
        )
        @ (
          g_path_left(e)
          >>= (
            (G_path(left)) =>
              right_yellow_green(e)
              >>= (
                ([@implicit_arity] Path(y, k)) =>
                  return(
                    [@implicit_arity]
                    Holy([@implicit_arity] Pair_right(left, y), k),
                  )
              )
          )
        ),
    )

and left_green:
  type a. m(a) => m(triple(a, a, [ | `green], left, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        (
          buffer_ge5(e)
          >>= (
            p =>
              buffer_eq2(e)
              >>= (s => return([@implicit_arity] Left_small(p, s)))
          )
        )
        @ (
          buffer_ge8(e)
          >>= (
            p =>
              gr_deque(stored_triple(e))
              >>= (
                (GR_deq(c)) =>
                  buffer_eq2(e)
                  >>= (s => return([@implicit_arity] Left_green(p, c, s)))
              )
          )
        ),
    )

and left_red: type a. m(a) => m(triple(a, a, [ | `red], left, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        buffer_ge5(e)
        >>= (
          p =>
            g_deque(stored_triple(e))
            >>= (
              (G_deq(c)) =>
                buffer_eq2(e)
                >>= (s => return([@implicit_arity] Left_red(p, c, s)))
            )
        ),
    )

and right_green:
  type a. m(a) => m(triple(a, a, [ | `green], right, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        (
          buffer_eq2(e)
          >>= (
            p =>
              buffer_ge5(e)
              >>= (s => return([@implicit_arity] Right_small(p, s)))
          )
        )
        @ (
          buffer_eq2(e)
          >>= (
            p =>
              gr_deque(stored_triple(e))
              >>= (
                (GR_deq(c)) =>
                  buffer_ge8(e)
                  >>= (s => return([@implicit_arity] Right_green(p, c, s)))
              )
          )
        ),
    )

and right_red: type a. m(a) => m(triple(a, a, [ | `red], right, nh, nh, nh)) =
  e =>
    Nest(
      () =>
        buffer_eq2(e)
        >>= (
          p =>
            g_deque(stored_triple(e))
            >>= (
              (G_deq(c)) =>
                buffer_ge5(e)
                >>= (s => return([@implicit_arity] Right_red(p, c, s)))
            )
        ),
    )

and g_deque: type a. m(a) => m(g_deque(a)) =
  e =>
    Nest(
      () =>
        (g_path_only(e) >>= ((G_path(p)) => return(G_deq(Only_path(p)))))
        @ (
          g_path_left(e)
          >>= (
            (G_path(left)) =>
              g_path_right(e)
              >>= (
                (G_path(right)) =>
                  return(G_deq([@implicit_arity] Pair_red(left, right)))
              )
          )
        ),
    )

and gr_deque: type a. m(a) => m(gr_deque(a)) =
  e =>
    Nest(
      () =>
        (
          gr_path_left(e)
          >>= (
            (GR_path(left)) =>
              gr_path_right(e)
              >>= (
                (GR_path(right)) =>
                  return(GR_deq([@implicit_arity] Pair_green(left, right)))
              )
          )
        )
        @ (
          g_path_left(e)
          >>= (
            (G_path(left)) =>
              g_path_right(e)
              >>= (
                (G_path(right)) =>
                  return(GR_deq([@implicit_arity] Pair_red(left, right)))
              )
          )
        )
        @ (
          gr_path_only(e)
          >>= ((GR_path(p)) => return(GR_deq(Only_path(p))))
        ),
    )

and gr_path_only: type a. m(a) => m(gr_path(a, only)) =
  e =>
    Nest(
      () =>
        (
          only_yellow_red(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return(GR_path([@implicit_arity] Path(y, k)))
          )
        )
        @ (
          only_yellow_green(e)
          >>= (
            ([@implicit_arity] Path(y, k)) =>
              return(GR_path([@implicit_arity] Path(y, k)))
          )
        ),
    )

and gr_path_left: type a. m(a) => m(gr_path(a, left)) =
  e =>
    Nest(
      () =>
        (left_yellow_red(e) >>= (p => return(GR_path(p))))
        @ (left_yellow_green(e) >>= (p => return(GR_path(p)))),
    )

and gr_path_right: type a. m(a) => m(gr_path(a, right)) =
  e =>
    Nest(
      () =>
        (right_yellow_red(e) >>= (y => return(GR_path(y))))
        @ (right_yellow_green(e) >>= (y => return(GR_path(y)))),
    )

and g_path_only: type a. m(a) => m(g_path(a, only)) =
  e => Nest(() => only_yellow_green(e) >>= (y => return(G_path(y))))

and g_path_left: type a. m(a) => m(g_path(a, left)) =
  e => Nest(() => left_yellow_green(e) >>= (p => return(G_path(p))))

and g_path_right: type a. m(a) => m(g_path(a, right)) =
  e => Nest(() => right_yellow_green(e) >>= (y => return(G_path(y))))

and semiregular: type a. m(a) => m(semi(a)) =
  e =>
    Nest(
      () =>
        return(S(Void))
        @ (gr_deque(e) >>= ((GR_deq(d)) => return(S(T(d))))),
    )

and regular: type a. m(a) => m(t(a)) =
  e =>
    return(Regular(Void))
    @ (g_deque(e) >>= ((G_deq(d)) => return(Regular(T(d)))));

let counter = ref(0);
let elt = () => {
  let v = counter^;
  counter := v + 1;
  v;
};

let gen_elt = Delay(() => return(elt()));

let all_semi = semiregular(gen_elt);
let all = regular(gen_elt);

let list_uncons =
  fun
  | [] => None
  | [x, ...xs] => Some((x, xs));

let rec list_unsnoc = acc =>
  fun
  | [] => None
  | [x] => Some((List.rev(acc), x))
  | [x, ...xs] => list_unsnoc([x, ...acc], xs);
let list_unsnoc = xs => list_unsnoc([], xs);

let option_eq = (eq, x, y) =>
  switch (x, y) {
  | (None, None) => true
  | (Some(x), Some(y)) => eq(x, y)
  | _ => false
  };

let max_depth = 6;

let rec uncons_to_list = (acc, deq) =>
  switch (D.uncons(deq)) {
  | None => List.rev(acc)
  | Some((x, d)) => uncons_to_list([x, ...acc], d)
  };
let uncons_to_list = deq => uncons_to_list([], deq);

let to_list = uncons_to_list;

let rec unsnoc_to_list = (acc, deq) =>
  switch (D.unsnoc(deq)) {
  | None => acc
  | Some((d, x)) => unsnoc_to_list([x, ...acc], d)
  };
let unsnoc_to_list = deq => unsnoc_to_list([], deq);

let rec un_to_list = (left, right, deq) =>
  if (Random.bool()) {
    switch (D.uncons(deq)) {
    | None => List.concat([List.rev(left), right])
    | Some((x, d)) => un_to_list([x, ...left], right, d)
    };
  } else {
    switch (D.unsnoc(deq)) {
    | None => List.concat([List.rev(left), right])
    | Some((d, x)) => un_to_list(left, [x, ...right], d)
    };
  };

let un_to_list = d => un_to_list([], [], d);

let fold_left_to_list = t =>
  List.rev(Deque.Deck.fold_left((xs, x) => [x, ...xs], [], Obj.magic(t)));

let test = (_, deq) => {
  let real_deq = D.regular_of_semi(deq);
  let lst = to_list(real_deq);
  assert(lst == Deque.Deck.to_list(Obj.magic(real_deq)));
  assert(lst == fold_left_to_list(real_deq));
  let x = elt();
  assert(to_list(D.cons(x, real_deq)) == [x, ...lst]);
  assert(to_list(D.snoc(real_deq, x)) == List.concat([lst, [x]]));
  assert(lst == un_to_list(real_deq));
  iteri(
    (j, deq') => {
      let real_deq' = D.regular_of_semi(deq');
      let lst' = to_list(real_deq');
      assert(
        to_list(D.concat(real_deq, real_deq')) == List.concat([lst, lst']),
      );
      let d1 = D.concat_semi(deq, deq');
      let d2 = D.concat_semi(deq', deq);
      let real_deq = D.regular_of_semi(d1);
      let dlst = to_list(D.regular_of_semi(d1));
      assert(dlst == un_to_list(real_deq));
      let real_deq = D.regular_of_semi(d2);
      let dlst = to_list(D.regular_of_semi(d2));
      assert(dlst == un_to_list(real_deq));
      j;
    },
    max_depth - 4,
    all_semi,
  );
};

let () = ignore(iteri(test, max_depth, all_semi));
