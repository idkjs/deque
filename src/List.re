/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* An alias for the type of lists. */
type t('a) = list('a) = | [] | ::('a, list('a));

/* List operations */

let rec length_aux = len =>
  fun
  | [] => len
  | [_, ...l] => length_aux(len + 1, l);

let length = l => length_aux(0, l);

let cons = (a, l) => [a, ...l];

let hd =
  fun
  | [] => failwith("hd")
  | [a, ..._] => a;

let tl =
  fun
  | [] => failwith("tl")
  | [_, ...l] => l;

let nth = (l, n) =>
  if (n < 0) {
    invalid_arg("List.nth");
  } else {
    let rec nth_aux = (l, n) =>
      switch (l) {
      | [] => failwith("nth")
      | [a, ...l] =>
        if (n == 0) {
          a;
        } else {
          nth_aux(l, n - 1);
        }
      };
    nth_aux(l, n);
  };

let nth_opt = (l, n) =>
  if (n < 0) {
    invalid_arg("List.nth");
  } else {
    let rec nth_aux = (l, n) =>
      switch (l) {
      | [] => None
      | [a, ...l] =>
        if (n == 0) {
          Some(a);
        } else {
          nth_aux(l, n - 1);
        }
      };
    nth_aux(l, n);
  };

let append = (@);

let rec rev_append = (l1, l2) =>
  switch (l1) {
  | [] => l2
  | [a, ...l] => rev_append(l, [a, ...l2])
  };

let rev = l => rev_append(l, []);

let rec init_tailrec_aux = (acc, i, n, f) =>
  if (i >= n) {
    acc;
  } else {
    init_tailrec_aux([f(i), ...acc], i + 1, n, f);
  };

let rec init_aux = (i, n, f) =>
  if (i >= n) {
    [];
  } else {
    let r = f(i);
    [r, ...init_aux(i + 1, n, f)];
  };

let rev_init_threshold =
  switch (Sys.backend_type) {
  | Sys.Native
  | Sys.Bytecode => 10_000
  /* We don't know the size of the stack, better be safe and assume it's
     small. */
  | Sys.Other(_) => 50
  };

let init = (len, f) =>
  if (len < 0) {
    invalid_arg("List.init");
  } else if (len > rev_init_threshold) {
    rev(init_tailrec_aux([], 0, len, f));
  } else {
    init_aux(0, len, f);
  };

let rec flatten =
  fun
  | [] => []
  | [l, ...r] => l @ flatten(r);

let concat = flatten;

let rec map = f =>
  fun
  | [] => []
  | [a, ...l] => {
      let r = f(a);
      [r, ...map(f, l)];
    };

let rec mapi = (i, f) =>
  fun
  | [] => []
  | [a, ...l] => {
      let r = f(i, a);
      [r, ...mapi(i + 1, f, l)];
    };

let mapi = (f, l) => mapi(0, f, l);

let rev_map = (f, l) => {
  let rec rmap_f = accu =>
    fun
    | [] => accu
    | [a, ...l] => rmap_f([f(a), ...accu], l);

  rmap_f([], l);
};

let rec iter = f =>
  fun
  | [] => ()
  | [a, ...l] => {
      f(a);
      iter(f, l);
    };

let rec iteri = (i, f) =>
  fun
  | [] => ()
  | [a, ...l] => {
      f(i, a);
      iteri(i + 1, f, l);
    };

let iteri = (f, l) => iteri(0, f, l);

let rec fold_left = (f, accu, l) =>
  switch (l) {
  | [] => accu
  | [a, ...l] => fold_left(f, f(accu, a), l)
  };

let rec fold_right = (f, l, accu) =>
  switch (l) {
  | [] => accu
  | [a, ...l] => f(a, fold_right(f, l, accu))
  };

let rec map2 = (f, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => []
  | ([a1, ...l1], [a2, ...l2]) =>
    let r = f(a1, a2);
    [r, ...map2(f, l1, l2)];
  | (_, _) => invalid_arg("List.map2")
  };

let rev_map2 = (f, l1, l2) => {
  let rec rmap2_f = (accu, l1, l2) =>
    switch (l1, l2) {
    | ([], []) => accu
    | ([a1, ...l1], [a2, ...l2]) => rmap2_f([f(a1, a2), ...accu], l1, l2)
    | (_, _) => invalid_arg("List.rev_map2")
    };

  rmap2_f([], l1, l2);
};

let rec iter2 = (f, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => ()
  | ([a1, ...l1], [a2, ...l2]) =>
    f(a1, a2);
    iter2(f, l1, l2);
  | (_, _) => invalid_arg("List.iter2")
  };

let rec fold_left2 = (f, accu, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => accu
  | ([a1, ...l1], [a2, ...l2]) => fold_left2(f, f(accu, a1, a2), l1, l2)
  | (_, _) => invalid_arg("List.fold_left2")
  };

let rec fold_right2 = (f, l1, l2, accu) =>
  switch (l1, l2) {
  | ([], []) => accu
  | ([a1, ...l1], [a2, ...l2]) => f(a1, a2, fold_right2(f, l1, l2, accu))
  | (_, _) => invalid_arg("List.fold_right2")
  };

let rec for_all = p =>
  fun
  | [] => true
  | [a, ...l] => p(a) && for_all(p, l);

let rec exists = p =>
  fun
  | [] => false
  | [a, ...l] => p(a) || exists(p, l);

let rec for_all2 = (p, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => true
  | ([a1, ...l1], [a2, ...l2]) => p(a1, a2) && for_all2(p, l1, l2)
  | (_, _) => invalid_arg("List.for_all2")
  };

let rec exists2 = (p, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => false
  | ([a1, ...l1], [a2, ...l2]) => p(a1, a2) || exists2(p, l1, l2)
  | (_, _) => invalid_arg("List.exists2")
  };

let rec mem = x =>
  fun
  | [] => false
  | [a, ...l] => compare(a, x) == 0 || mem(x, l);

let rec memq = x =>
  fun
  | [] => false
  | [a, ...l] => a === x || memq(x, l);

let rec assoc = x =>
  fun
  | [] => raise(Not_found)
  | [(a, b), ...l] =>
    if (compare(a, x) == 0) {
      b;
    } else {
      assoc(x, l);
    };

let rec assoc_opt = x =>
  fun
  | [] => None
  | [(a, b), ...l] =>
    if (compare(a, x) == 0) {
      Some(b);
    } else {
      assoc_opt(x, l);
    };

let rec assq = x =>
  fun
  | [] => raise(Not_found)
  | [(a, b), ...l] =>
    if (a === x) {
      b;
    } else {
      assq(x, l);
    };

let rec assq_opt = x =>
  fun
  | [] => None
  | [(a, b), ...l] =>
    if (a === x) {
      Some(b);
    } else {
      assq_opt(x, l);
    };

let rec mem_assoc = x =>
  fun
  | [] => false
  | [(a, _), ...l] => compare(a, x) == 0 || mem_assoc(x, l);

let rec mem_assq = x =>
  fun
  | [] => false
  | [(a, _), ...l] => a === x || mem_assq(x, l);

let rec remove_assoc = x =>
  fun
  | [] => []
  | [(a, _) as pair, ...l] =>
    if (compare(a, x) == 0) {
      l;
    } else {
      [pair, ...remove_assoc(x, l)];
    };

let rec remove_assq = x =>
  fun
  | [] => []
  | [(a, _) as pair, ...l] =>
    if (a === x) {
      l;
    } else {
      [pair, ...remove_assq(x, l)];
    };

let rec find = p =>
  fun
  | [] => raise(Not_found)
  | [x, ...l] =>
    if (p(x)) {
      x;
    } else {
      find(p, l);
    };

let rec find_opt = p =>
  fun
  | [] => None
  | [x, ...l] =>
    if (p(x)) {
      Some(x);
    } else {
      find_opt(p, l);
    };

let rec find_map = f =>
  fun
  | [] => None
  | [x, ...l] =>
    switch (f(x)) {
    | Some(_) as result => result
    | None => find_map(f, l)
    };

let find_all = p => {
  let rec find = accu =>
    fun
    | [] => rev(accu)
    | [x, ...l] =>
      if (p(x)) {
        find([x, ...accu], l);
      } else {
        find(accu, l);
      };
  find([]);
};

let filter = find_all;

let filteri = (p, l) => {
  let rec aux = (i, acc) =>
    fun
    | [] => rev(acc)
    | [x, ...l] =>
      aux(
        i + 1,
        if (p(i, x)) {
          [x, ...acc];
        } else {
          acc;
        },
        l,
      );

  aux(0, [], l);
};

let filter_map = f => {
  let rec aux = accu =>
    fun
    | [] => rev(accu)
    | [x, ...l] =>
      switch (f(x)) {
      | None => aux(accu, l)
      | Some(v) => aux([v, ...accu], l)
      };

  aux([]);
};

let concat_map = (f, l) => {
  let rec aux = (f, acc) =>
    fun
    | [] => rev(acc)
    | [x, ...l] => {
        let xs = f(x);
        aux(f, rev_append(xs, acc), l);
      };
  aux(f, [], l);
};

let fold_left_map = (f, accu, l) => {
  let rec aux = (accu, l_accu) =>
    fun
    | [] => (accu, rev(l_accu))
    | [x, ...l] => {
        let (accu, x) = f(accu, x);
        aux(accu, [x, ...l_accu], l);
      };
  aux(accu, [], l);
};

let partition = (p, l) => {
  let rec part = (yes, no) =>
    fun
    | [] => (rev(yes), rev(no))
    | [x, ...l] =>
      if (p(x)) {
        part([x, ...yes], no, l);
      } else {
        part(yes, [x, ...no], l);
      };
  part([], [], l);
};

let partition_map = (p, l) => {
  let rec part = (left, right) =>
    fun
    | [] => (rev(left), rev(right))
    | [x, ...l] =>
      switch (p(x)) {
      | Either.Left(v) => part([v, ...left], right, l)
      | Either.Right(v) => part(left, [v, ...right], l)
      };

  part([], [], l);
};

let rec split =
  fun
  | [] => ([], [])
  | [(x, y), ...l] => {
      let (rx, ry) = split(l);
      ([x, ...rx], [y, ...ry]);
    };

let rec combine = (l1, l2) =>
  switch (l1, l2) {
  | ([], []) => []
  | ([a1, ...l1], [a2, ...l2]) => [(a1, a2), ...combine(l1, l2)]
  | (_, _) => invalid_arg("List.combine")
  };

/** sorting */;

let rec merge = (cmp, l1, l2) =>
  switch (l1, l2) {
  | ([], l2) => l2
  | (l1, []) => l1
  | ([h1, ...t1], [h2, ...t2]) =>
    if (cmp(h1, h2) <= 0) {
      [h1, ...merge(cmp, t1, l2)];
    } else {
      [h2, ...merge(cmp, l1, t2)];
    }
  };

let stable_sort = (cmp, l) => {
  let rec rev_merge = (l1, l2, accu) =>
    switch (l1, l2) {
    | ([], l2) => rev_append(l2, accu)
    | (l1, []) => rev_append(l1, accu)
    | ([h1, ...t1], [h2, ...t2]) =>
      if (cmp(h1, h2) <= 0) {
        rev_merge(t1, l2, [h1, ...accu]);
      } else {
        rev_merge(l1, t2, [h2, ...accu]);
      }
    };

  let rec rev_merge_rev = (l1, l2, accu) =>
    switch (l1, l2) {
    | ([], l2) => rev_append(l2, accu)
    | (l1, []) => rev_append(l1, accu)
    | ([h1, ...t1], [h2, ...t2]) =>
      if (cmp(h1, h2) > 0) {
        rev_merge_rev(t1, l2, [h1, ...accu]);
      } else {
        rev_merge_rev(l1, t2, [h2, ...accu]);
      }
    };

  let rec sort = (n, l) =>
    switch (n, l) {
    | (2, [x1, x2, ...tl]) =>
      let s =
        if (cmp(x1, x2) <= 0) {
          [x1, x2];
        } else {
          [x2, x1];
        };
      (s, tl);
    | (3, [x1, x2, x3, ...tl]) =>
      let s =
        if (cmp(x1, x2) <= 0) {
          if (cmp(x2, x3) <= 0) {
            [x1, x2, x3];
          } else if (cmp(x1, x3) <= 0) {
            [x1, x3, x2];
          } else {
            [x3, x1, x2];
          };
        } else if (cmp(x1, x3) <= 0) {
          [x2, x1, x3];
        } else if (cmp(x2, x3) <= 0) {
          [x2, x3, x1];
        } else {
          [x3, x2, x1];
        };

      (s, tl);
    | (n, l) =>
      let n1 = n asr 1;
      let n2 = n - n1;
      let (s1, l2) = rev_sort(n1, l);
      let (s2, tl) = rev_sort(n2, l2);
      (rev_merge_rev(s1, s2, []), tl);
    }
  and rev_sort = (n, l) =>
    switch (n, l) {
    | (2, [x1, x2, ...tl]) =>
      let s =
        if (cmp(x1, x2) > 0) {
          [x1, x2];
        } else {
          [x2, x1];
        };
      (s, tl);
    | (3, [x1, x2, x3, ...tl]) =>
      let s =
        if (cmp(x1, x2) > 0) {
          if (cmp(x2, x3) > 0) {
            [x1, x2, x3];
          } else if (cmp(x1, x3) > 0) {
            [x1, x3, x2];
          } else {
            [x3, x1, x2];
          };
        } else if (cmp(x1, x3) > 0) {
          [x2, x1, x3];
        } else if (cmp(x2, x3) > 0) {
          [x2, x3, x1];
        } else {
          [x3, x2, x1];
        };

      (s, tl);
    | (n, l) =>
      let n1 = n asr 1;
      let n2 = n - n1;
      let (s1, l2) = sort(n1, l);
      let (s2, tl) = sort(n2, l2);
      (rev_merge(s1, s2, []), tl);
    };

  let len = length(l);
  if (len < 2) {
    l;
  } else {
    fst(sort(len, l));
  };
};

let sort = stable_sort;
let fast_sort = stable_sort;

/* Note: on a list of length between about 100000 (depending on the minor
      heap size and the type of the list) and Sys.max_array_size, it is
      actually faster to use the following, but it might also use more memory
      because the argument list cannot be deallocated incrementally.

      Also, there seems to be a bug in this code or in the
      implementation of obj_truncate.

   external obj_truncate : 'a array -> int -> unit = "caml_obj_truncate"

   let array_to_list_in_place a =
     let l = Array.length a in
     let rec loop accu n p =
       if p <= 0 then accu else begin
         if p = n then begin
           obj_truncate a p;
           loop (a.(p-1) :: accu) (n-1000) (p-1)
         end else begin
           loop (a.(p-1) :: accu) n (p-1)
         end
       end
     in
     loop [] (l-1000) l


   let stable_sort cmp l =
     let a = Array.of_list l in
     Array.stable_sort cmp a;
     array_to_list_in_place a

   */

/** sorting + removing duplicates */;

let sort_uniq = (cmp, l) => {
  let rec rev_merge = (l1, l2, accu) =>
    switch (l1, l2) {
    | ([], l2) => rev_append(l2, accu)
    | (l1, []) => rev_append(l1, accu)
    | ([h1, ...t1], [h2, ...t2]) =>
      let c = cmp(h1, h2);
      if (c == 0) {
        rev_merge(t1, t2, [h1, ...accu]);
      } else if (c < 0) {
        rev_merge(t1, l2, [h1, ...accu]);
      } else {
        rev_merge(l1, t2, [h2, ...accu]);
      };
    };

  let rec rev_merge_rev = (l1, l2, accu) =>
    switch (l1, l2) {
    | ([], l2) => rev_append(l2, accu)
    | (l1, []) => rev_append(l1, accu)
    | ([h1, ...t1], [h2, ...t2]) =>
      let c = cmp(h1, h2);
      if (c == 0) {
        rev_merge_rev(t1, t2, [h1, ...accu]);
      } else if (c > 0) {
        rev_merge_rev(t1, l2, [h1, ...accu]);
      } else {
        rev_merge_rev(l1, t2, [h2, ...accu]);
      };
    };

  let rec sort = (n, l) =>
    switch (n, l) {
    | (2, [x1, x2, ...tl]) =>
      let s = {
        let c = cmp(x1, x2);
        if (c == 0) {
          [x1];
        } else if (c < 0) {
          [x1, x2];
        } else {
          [x2, x1];
        };
      };

      (s, tl);
    | (3, [x1, x2, x3, ...tl]) =>
      let s = {
        let c = cmp(x1, x2);
        if (c == 0) {
          let c = cmp(x2, x3);
          if (c == 0) {
            [x2];
          } else if (c < 0) {
            [x2, x3];
          } else {
            [x3, x2];
          };
        } else if (c < 0) {
          let c = cmp(x2, x3);
          if (c == 0) {
            [x1, x2];
          } else if (c < 0) {
            [x1, x2, x3];
          } else {
            let c = cmp(x1, x3);
            if (c == 0) {
              [x1, x2];
            } else if (c < 0) {
              [x1, x3, x2];
            } else {
              [x3, x1, x2];
            };
          };
        } else {
          let c = cmp(x1, x3);
          if (c == 0) {
            [x2, x1];
          } else if (c < 0) {
            [x2, x1, x3];
          } else {
            let c = cmp(x2, x3);
            if (c == 0) {
              [x2, x1];
            } else if (c < 0) {
              [x2, x3, x1];
            } else {
              [x3, x2, x1];
            };
          };
        };
      };

      (s, tl);
    | (n, l) =>
      let n1 = n asr 1;
      let n2 = n - n1;
      let (s1, l2) = rev_sort(n1, l);
      let (s2, tl) = rev_sort(n2, l2);
      (rev_merge_rev(s1, s2, []), tl);
    }
  and rev_sort = (n, l) =>
    switch (n, l) {
    | (2, [x1, x2, ...tl]) =>
      let s = {
        let c = cmp(x1, x2);
        if (c == 0) {
          [x1];
        } else if (c > 0) {
          [x1, x2];
        } else {
          [x2, x1];
        };
      };

      (s, tl);
    | (3, [x1, x2, x3, ...tl]) =>
      let s = {
        let c = cmp(x1, x2);
        if (c == 0) {
          let c = cmp(x2, x3);
          if (c == 0) {
            [x2];
          } else if (c > 0) {
            [x2, x3];
          } else {
            [x3, x2];
          };
        } else if (c > 0) {
          let c = cmp(x2, x3);
          if (c == 0) {
            [x1, x2];
          } else if (c > 0) {
            [x1, x2, x3];
          } else {
            let c = cmp(x1, x3);
            if (c == 0) {
              [x1, x2];
            } else if (c > 0) {
              [x1, x3, x2];
            } else {
              [x3, x1, x2];
            };
          };
        } else {
          let c = cmp(x1, x3);
          if (c == 0) {
            [x2, x1];
          } else if (c > 0) {
            [x2, x1, x3];
          } else {
            let c = cmp(x2, x3);
            if (c == 0) {
              [x2, x1];
            } else if (c > 0) {
              [x2, x3, x1];
            } else {
              [x3, x2, x1];
            };
          };
        };
      };

      (s, tl);
    | (n, l) =>
      let n1 = n asr 1;
      let n2 = n - n1;
      let (s1, l2) = sort(n1, l);
      let (s2, tl) = sort(n2, l2);
      (rev_merge(s1, s2, []), tl);
    };

  let len = length(l);
  if (len < 2) {
    l;
  } else {
    fst(sort(len, l));
  };
};

let rec compare_lengths = (l1, l2) =>
  switch (l1, l2) {
  | ([], []) => 0
  | ([], _) => (-1)
  | (_, []) => 1
  | ([_, ...l1], [_, ...l2]) => compare_lengths(l1, l2)
  };

let rec compare_length_with = (l, n) =>
  switch (l) {
  | [] =>
    if (n == 0) {
      0;
    } else if (n > 0) {
      (-1);
    } else {
      1;
    }
  | [_, ...l] =>
    if (n <= 0) {
      1;
    } else {
      compare_length_with(l, n - 1);
    }
  };

/** {1 Comparison} */;

/* Note: we are *not* shortcutting the list by using
   [List.compare_lengths] first; this may be slower on long lists
   immediately start with distinct elements. It is also incorrect for
   [compare] below, and it is better (principle of least surprise) to
   use the same approach for both functions. */
let rec equal = (eq, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => true
  | ([], [_, ..._])
  | ([_, ..._], []) => false
  | ([a1, ...l1], [a2, ...l2]) => eq(a1, a2) && equal(eq, l1, l2)
  };

let rec compare = (cmp, l1, l2) =>
  switch (l1, l2) {
  | ([], []) => 0
  | ([], [_, ..._]) => (-1)
  | ([_, ..._], []) => 1
  | ([a1, ...l1], [a2, ...l2]) =>
    let c = cmp(a1, a2);
    if (c != 0) {
      c;
    } else {
      compare(cmp, l1, l2);
    };
  };

/** {1 Iterators} */;

let to_seq = l => {
  let rec aux = (l, ()) =>
    switch (l) {
    | [] => Seq.Nil
    | [x, ...tail] => Seq.Cons(x, aux(tail))
    };

  aux(l);
};

let of_seq = seq => {
  let rec direct = (depth, seq): list(_) =>
    if (depth == 0) {
      Seq.fold_left((acc, x) => [x, ...acc], [], seq) |> rev;
    } else {
      /* tailrec */
      switch (seq()) {
      | Seq.Nil => []
      | Seq.Cons(x, next) => [
          x,
          ...direct(depth - 1, next),
        ]
      };
    };

  direct(500, seq);
};
