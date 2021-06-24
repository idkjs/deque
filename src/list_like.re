module type DEQUE = {
  type t('a);
  let empty: t('a);
  let is_empty: t('a) => bool;
  let cons: ('a, t('a)) => t('a);
  let uncons: t('a) => option(('a, t('a)));
  let snoc: (t('a), 'a) => t('a);

  let fold_left: (('a, 'b) => 'a, 'a, t('b)) => 'a;
  let fold_right: (('a, 'b) => 'b, t('a), 'b) => 'b;

  let rev: t('a) => t('a);
  let append: (t('a), t('a)) => t('a);

  let length: t('a) => int;
};

module type DEQUE_CAT = {
  include DEQUE;
  let append: (t('a), t('a)) => t('a);
  let rev: t('a) => t('a);
};

module Make = (D: DEQUE) => {
  let hd = t =>
    switch (D.uncons(t)) {
    | None => failwith("Deque.hd")
    | Some((x, _)) => x
    };

  let tl = t =>
    switch (D.uncons(t)) {
    | None => failwith("Deque.tl")
    | Some((_, t)) => t
    };

  let iter = (f, t) => D.fold_left(((), x) => f(x), (), t);

  let iteri = (f, t) => {
    let _ =
      D.fold_left(
        (i, x) => {
          f(i, x);
          i + 1;
        },
        0,
        t,
      );
    ();
  };

  let nth = (type a, t, idx) => {
    if (idx < 0) {
      invalid_arg("Deque.nth");
    };
    module M = {
      exception Found(a);
    };
    try(
      {
        iteri(
          (i, x) =>
            if (i == idx) {
              raise(M.Found(x));
            },
          t,
        );
        failwith("Deque.nth");
      }
    ) {
    | M.Found(x) => x
    };
  };

  let nth_opt = (t, idx) =>
    try(Some(nth(t, idx))) {
    | Failure(_) => None
    };

  let map = (f, t) =>
    D.fold_left((ys, x) => D.snoc(ys, f(x)), D.empty, t);

  let mapi = (f, t) => {
    let (_, t) =
      D.fold_left(
        ((i, ys), x) => (i + 1, D.snoc(ys, f(i, x))),
        (0, D.empty),
        t,
      );
    t;
  };

  let rev_map = (f, t) =>
    D.fold_left((ys, x) => D.cons(f(x), ys), D.empty, t);

  let filter_map = (f, t) =>
    D.fold_left(
      (ys, x) =>
        switch (f(x)) {
        | None => ys
        | Some(y) => D.snoc(ys, y)
        },
      D.empty,
      t,
    );

  let fold_left_map = (f, z, t) =>
    D.fold_left(
      ((z, ys), x) => {
        let (z, y) = f(z, x);
        (z, D.snoc(ys, y));
      },
      (z, D.empty),
      t,
    );

  exception Abort;

  let exists = (p, t) =>
    try(
      {
        iter(
          x =>
            if (p(x)) {
              raise(Abort);
            },
          t,
        );
        false;
      }
    ) {
    | Abort => true
    };

  let for_all = (p, t) => !exists(x => !p(x), t);

  let mem = (x, t) => exists((==)(x), t);
  let memq = (x, t) => exists((===)(x), t);

  let find = (type a, p, t) => {
    module M = {
      exception Found(a);
    };
    try(
      {
        iter(
          x =>
            if (p(x)) {
              raise(M.Found(x));
            },
          t,
        );
        raise(Not_found);
      }
    ) {
    | M.Found(x) => x
    };
  };

  let find_opt = (type a, p, t) => {
    module M = {
      exception Found(a);
    };
    try(
      {
        iter(
          x =>
            if (p(x)) {
              raise(M.Found(x));
            },
          t,
        );
        None;
      }
    ) {
    | M.Found(x) => Some(x)
    };
  };

  let find_map = (type a, f, t) => {
    module M = {
      exception Found(a);
    };
    let g = x =>
      switch (f(x)) {
      | None => ()
      | Some(y) => raise(M.Found(y))
      };
    try(
      {
        iter(g, t);
        None;
      }
    ) {
    | M.Found(x) => Some(x)
    };
  };

  let filter = (f, t) =>
    D.fold_left(
      (ys, x) =>
        if (f(x)) {
          D.snoc(ys, x);
        } else {
          ys;
        },
      D.empty,
      t,
    );

  let find_all = filter;

  let filteri = (f, t) => {
    let (_, t) =
      D.fold_left(
        ((i, ys), x) =>
          (
            i + 1,
            if (f(i, x)) {
              D.snoc(ys, x);
            } else {
              ys;
            },
          ),
        (0, D.empty),
        t,
      );
    t;
  };

  let partition = (f, t) =>
    D.fold_left(
      ((left, right), x) =>
        if (f(x)) {
          (D.snoc(left, x), right);
        } else {
          (left, D.snoc(right, x));
        },
      (D.empty, D.empty),
      t,
    );

  let assoc = (k, t) => {
    let (_, y) = find(((x, _)) => x == k, t);
    y;
  };

  let assoc_opt = (k, t) =>
    find_map(
      ((x, y)) =>
        if (x == k) {
          Some(y);
        } else {
          None;
        },
      t,
    );

  let assq = (k, t) => {
    let (_, y) = find(((x, _)) => x === k, t);
    y;
  };

  let assq_opt = (k, t) =>
    find_map(
      ((x, y)) =>
        if (x === k) {
          Some(y);
        } else {
          None;
        },
      t,
    );

  let mem_assoc = (x, t) => exists(((x', _)) => x == x', t);
  let mem_assq = (x, t) => exists(((x', _)) => x === x', t);

  let split = t =>
    D.fold_left(
      ((left, right), (x, y)) => (D.snoc(left, x), D.snoc(right, y)),
      (D.empty, D.empty),
      t,
    );

  let to_list = t => D.fold_right((x, xs) => [x, ...xs], t, []);

  let of_list = xs => List.fold_left((t, x) => D.snoc(t, x), D.empty, xs);

  let to_seq = t => Seq.unfold(D.uncons, t);

  let of_seq = s => Seq.fold_left((xs, x) => D.snoc(xs, x), D.empty, s);

  let init = (n, f) => {
    let rec go = (acc, i) =>
      if (i >= n) {
        acc;
      } else {
        let x = f(i);
        go(D.snoc(acc, x), i + 1);
      };

    go(D.empty, 0);
  };

  let to_array = t =>
    switch (D.uncons(t)) {
    | None => [||]
    | Some((x, t)) =>
      let n = D.length(t);
      let arr = Array.make(n + 1, x);
      iteri((i, x) => arr[i + 1] = x, t);
      arr;
    };

  let of_array = t => init(Array.length(t), Array.get(t));

  let rec merge = (cmp, acc, xs, ys) =>
    switch (D.uncons(xs)) {
    | None => D.append(acc, ys)
    | Some((x, xs)) => merge_left(cmp, acc, x, xs, ys)
    }

  and merge_left = (cmp, acc, x, xs, ys) =>
    switch (D.uncons(ys)) {
    | None => D.append(D.snoc(acc, x), xs)
    | Some((y, ys)) => merge_head(cmp, acc, x, xs, y, ys)
    }

  and merge_right = (cmp, acc, xs, y, ys) =>
    switch (D.uncons(xs)) {
    | None => D.append(D.snoc(acc, y), ys)
    | Some((x, xs)) => merge_head(cmp, acc, x, xs, y, ys)
    }

  and merge_head = (cmp, acc, x, xs, y, ys) =>
    if (cmp(x, y) <= 0) {
      merge_right(cmp, D.snoc(acc, x), xs, y, ys);
    } else {
      merge_left(cmp, D.snoc(acc, y), x, xs, ys);
    };

  let merge = (cmp, xs, ys) => merge(cmp, D.empty, xs, ys);

  let sort = (cmp, t) => {
    let t = to_array(t);
    Array.sort(cmp, t);
    of_array(t);
  };

  let stable_sort = (cmp, t) => {
    let t = to_array(t);
    Array.stable_sort(cmp, t);
    of_array(t);
  };

  let fast_sort = (cmp, t) => {
    let t = to_array(t);
    Array.fast_sort(cmp, t);
    of_array(t);
  };

  let sort_uniq = (cmp, t) => of_list @@ List.sort_uniq(cmp) @@ to_list(t);

  let fold_left2 = (~exn, f, z, xs, ys) => {
    let (z, ys) =
      D.fold_left(
        ((z, ys), x) =>
          switch (D.uncons(ys)) {
          | None => raise(exn)
          | Some((y, ys)) => (f(z, x, y), ys)
          },
        (z, ys),
        xs,
      );

    if (D.is_empty(ys)) {
      z;
    } else {
      raise(exn);
    };
  };

  let iter2 = (f, xs, ys) =>
    fold_left2(
      ~exn=Invalid_argument("Deque.iter2"),
      ((), x, y) => f(x, y),
      (),
      xs,
      ys,
    );

  let map2 = (f, xs, ys) =>
    fold_left2(
      ~exn=Invalid_argument("Deque.map2"),
      (t, x, y) => D.snoc(t, f(x, y)),
      D.empty,
      xs,
      ys,
    );

  let rev_map2 = (f, xs, ys) =>
    fold_left2(
      ~exn=Invalid_argument("Deque.rev_map2"),
      (t, x, y) => D.cons(f(x, y), t),
      D.empty,
      xs,
      ys,
    );

  let exists2 = (p, xs, ys) =>
    try(
      fold_left2(
        ~exn=Invalid_argument("Deque.exists2"),
        (b, x, y) =>
          if (p(x, y)) {
            raise(Abort);
          } else {
            b;
          },
        false,
        xs,
        ys,
      )
    ) {
    | Abort => true
    };

  let for_all2 = (p, xs, ys) =>
    try(
      fold_left2(
        ~exn=Invalid_argument("Deque.for_all2"),
        (b, x, y) =>
          if (p(x, y)) {
            b;
          } else {
            raise(Abort);
          },
        true,
        xs,
        ys,
      )
    ) {
    | Abort => false
    };

  let combine = (xs, ys) =>
    fold_left2(
      ~exn=Invalid_argument("Deque.combine"),
      (t, x, y) => D.snoc(t, (x, y)),
      D.empty,
      xs,
      ys,
    );

  let fold_left2 = (f, z, xs, ys) =>
    fold_left2(~exn=Invalid_argument("Deque.fold_left2"), f, z, xs, ys);

  let compare = (cmp, xs, ys) => {
    module M = {
      exception Return(int);
    };
    try({
      let ys =
        D.fold_left(
          (ys, x) =>
            switch (D.uncons(ys)) {
            | None => raise(M.Return(-1))
            | Some((y, ys)) =>
              switch (cmp(x, y)) {
              | 0 => ys
              | n => raise(M.Return(n))
              }
            },
          ys,
          xs,
        );

      if (D.is_empty(ys)) {
        0;
      } else {
        1;
      };
    }) {
    | M.Return(n) => n
    };
  };

  let equal = (eq, xs, ys) =>
    try({
      let ys =
        D.fold_left(
          (ys, x) =>
            switch (D.uncons(ys)) {
            | None => raise(Abort)
            | Some((y, ys)) =>
              if (eq(x, y)) {
                ys;
              } else {
                raise(Abort);
              }
            },
          ys,
          xs,
        );

      D.is_empty(ys);
    }) {
    | Abort => false
    };

  let (==) = (xs, ys) => equal((==), xs, ys);

  let (@) = D.append;

  let rev_append = (xs, ys) => D.rev(xs) @ ys;

  let concat = xss => D.fold_left((z, x) => x @ z, D.empty, xss);

  let flatten = concat;

  let concat_map = (f, t) => D.fold_left((ys, x) => ys @ f(x), D.empty, t);
};
