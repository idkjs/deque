let () = Random.self_init();

let make_f = () => {
  let calls = ref([]);
  let f = x => {
    calls := [x, ...calls^];
    x;
  };
  (f, calls);
};

let make_fs = () => {
  let (f, fc) = make_f();
  let (g, gc) = make_f();
  let check = () => {
    if (fc^ != gc^) {
      failwith("differing call order");
    };
    if (fc^ == []) {
      failwith("functions were not called");
    };
  };

  (f, g, check);
};

let counter = ref(min_int);
let elt = () => {
  incr(counter);
  counter^;
};

let make_list = n => List.init(n, _ => elt());

let test = (name, fn) =>
  try(
    {
      fn();
      Printf.printf("OK %s\n%!", name);
    }
  ) {
  | err =>
    Printf.fprintf(
      stderr,
      "ERROR %s\n%s\n%!",
      name,
      Printexc.to_string(err),
    );
    Printexc.print_backtrace(stderr);
    raise(err);
  };

module Test = (D: (module type of Deque.Dequeue)) => {
  let input_size = 10;

  let make = (~size=input_size, ()) => {
    let lst = make_list(size);
    let deq = D.of_list(lst);
    (lst, deq);
  };

  let assert_eq = (lst, deq) => assert(lst == D.to_list(deq));
  let assert_not_found = f =>
    assert(
      try({
        let _ = f();
        false;
      }) {
      | Not_found => true
      },
    );
  let assert_invalid = f =>
    assert(
      try({
        let _ = f();
        false;
      }) {
      | Invalid_argument(_) => true
      },
    );
  let assert_failure = f =>
    assert(
      try({
        let _ = f();
        false;
      }) {
      | Failure(_) => true
      },
    );

  let () =
    test("iter") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        List.iter(x => ignore(f(x)), lst);
        D.iter(x => ignore(g(x)), deq);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("iteri") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        List.iteri((i, x) => ignore(f((i, x))), lst);
        D.iteri((i, x) => ignore(g((i, x))), deq);
        check();
      }
    );

  let () =
    test("map") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let lst = List.map(f, lst);
        let deq = D.map(g, deq);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("mapi") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let lst = List.mapi((i, x) => f((i, x)), lst);
        let deq = D.mapi((i, x) => g((i, x)), deq);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("rev") @@
    (
      () => {
        let (lst as lst_orig, deq) = make();
        let lst = List.rev(lst);
        let deq = D.rev(deq);
        assert_eq(lst, deq);
        assert(lst != lst_orig);
      }
    );

  let () =
    test("append & rev_append") @@
    (
      () => {
        let (lst0, deq0) = make(~size=input_size, ());
        let (lst1, deq1) = make(~size=2 * input_size, ());
        let lst01 = List.append(lst0, lst1);
        let deq01 = D.append(deq0, deq1);
        assert_eq(lst01, deq01);
        let lst10 = List.rev_append(lst0, lst1);
        let deq10 = D.rev_append(deq0, deq1);
        assert_eq(lst10, deq10);
        assert(lst10 != lst01);
      }
    );

  let () =
    test("rev_map") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let lst = List.rev_map(f, lst);
        let deq = D.rev_map(g, deq);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("filter_map") @@
    (
      () => {
        let (lst as lst_orig, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) =>
          if (f(x) mod 2 == 0) {
            Some(x * x);
          } else {
            None;
          };
        let lst = List.filter_map(is_even(f), lst);
        let deq = D.filter_map(is_even(g), deq);
        assert(List.length(lst) < List.length(lst_orig));
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("nth & nth_opt") @@
    (
      () => {
        let (lst, deq) = make(~size=1234, ());
        for (i in 0 to List.length(lst) - 1) {
          let x = List.nth(lst, i);
          let y = D.nth(deq, i);
          assert(x == y);
          switch (D.nth_opt(deq, i)) {
          | None => assert(false)
          | Some(y) => assert(x == y)
          };
        };
        assert_invalid(() => D.nth(deq, -1));
        assert_invalid(() => D.nth_opt(deq, -1));
        assert_failure(() => D.nth(deq, D.length(deq)));
        assert(None == D.nth_opt(deq, D.length(deq)));
      }
    );

  type acc('a) =
    | Z
    | X('a)
    | F(acc('a), acc('a));

  let () =
    test("fold_left") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let acc = (f, z, x) => [@implicit_arity] F(z, X(f(x)));
        let x = List.fold_left(acc(f), Z, lst);
        let y = D.fold_left(acc(g), Z, deq);
        assert(x == y);
        let x = List.fold_left(acc(f), Z, List.rev(lst));
        let y = D.fold_left(acc(g), Z, D.rev(deq));
        assert(x == y);
        check();
      }
    );

  let () =
    test("fold_left2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        let acc = (f, z, x, y) => [@implicit_arity] F(z, X(f((x, y))));
        let x = List.fold_left2(acc(f), Z, lst0, lst1);
        let y = D.fold_left2(acc(g), Z, deq0, deq1);
        assert(x == y);
        check();
      }
    );

  let () =
    test("fold_right") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let acc = (f, x, z) => [@implicit_arity] F(X(f(x)), z);
        let x = List.fold_right(acc(f), lst, Z);
        let y = D.fold_right(acc(g), deq, Z);
        assert(x == y);
        let x = List.fold_right(acc(f), List.rev(lst), Z);
        let y = D.fold_right(acc(g), D.rev(deq), Z);
        assert(x == y);
        check();
      }
    );

  let () =
    test("fold_left_map") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let go = (f, acc, x) => ([x, ...acc], f(x));
        let (lst_acc, lst) = List.fold_left_map(go(f), [], lst);
        let (deq_acc, deq) = D.fold_left_map(go(g), [], deq);
        assert(lst_acc == deq_acc);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("iter2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        List.iter2((x, y) => ignore(f((x, y))), lst0, lst1);
        D.iter2((x, y) => ignore(g((x, y))), deq0, deq1);
        check();
      }
    );

  let () =
    test("map2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        let lst = List.map2((x, y) => f((x, y)), lst0, lst1);
        let deq = D.map2((x, y) => g((x, y)), deq0, deq1);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("rev_map2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        let lst = List.rev_map2((x, y) => f((x, y)), lst0, lst1);
        let deq = D.rev_map2((x, y) => g((x, y)), deq0, deq1);
        assert_eq(lst, deq);
        check();
      }
    );

  let () =
    test("exists") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) => f(x) mod 2 == 0;
        assert(true == List.exists(is_even(f), lst));
        assert(true == D.exists(is_even(g), deq));
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert(false == List.exists(nope(f), lst));
        assert(false == D.exists(nope(g), deq));
        let fail = _ => failwith("should not be called");
        assert(false == List.exists(fail, []));
        assert(false == D.exists(fail, D.empty));
        check();
      }
    );

  let () =
    test("for_all") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) => f(x) mod 2 == 0;
        assert(false == List.for_all(is_even(f), lst));
        assert(false == D.for_all(is_even(g), deq));
        let yeap = (f, x) => {
          ignore(f(x));
          true;
        };
        assert(true == List.for_all(yeap(f), lst));
        assert(true == D.for_all(yeap(g), deq));
        let fail = _ => failwith("should not be called");
        assert(true == List.for_all(fail, []));
        assert(true == D.for_all(fail, D.empty));
        check();
      }
    );

  let () =
    test("exists2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x, y) => f(x * y) mod 2 == 0;
        assert(true == List.exists2(is_even(f), lst0, lst1));
        assert(true == D.exists2(is_even(g), deq0, deq1));
        let nope = (f, x, y) => {
          ignore(f(x * y));
          false;
        };
        assert(false == List.exists2(nope(f), lst0, lst1));
        assert(false == D.exists2(nope(g), deq0, deq1));
        let fail = (_, _) => failwith("should not be called");
        assert(false == List.exists2(fail, [], []));
        assert(false == D.exists2(fail, D.empty, D.empty));
        assert(
          try(List.exists2(fail, [], lst1)) {
          | Invalid_argument(_) => true
          },
        );
        assert(
          try(D.exists2(fail, D.empty, deq1)) {
          | Invalid_argument(_) => true
          },
        );
        check();
      }
    );

  let () =
    test("for_all2") @@
    (
      () => {
        let (lst0, deq0) = make();
        let (lst1, deq1) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x, y) => f(x * y) mod 2 == 0;
        assert(false == List.for_all2(is_even(f), lst0, lst1));
        assert(false == D.for_all2(is_even(g), deq0, deq1));
        let yeap = (f, x, y) => {
          ignore(f(x * y));
          true;
        };
        assert(true == List.for_all2(yeap(f), lst0, lst1));
        assert(true == D.for_all2(yeap(g), deq0, deq1));
        let fail = (_, _) => failwith("should not be called");
        assert(true == List.for_all2(fail, [], []));
        assert(true == D.for_all2(fail, D.empty, D.empty));
        assert(
          try(List.for_all2(fail, [], lst1)) {
          | Invalid_argument(_) => true
          },
        );
        assert(
          try(D.for_all2(fail, D.empty, deq1)) {
          | Invalid_argument(_) => true
          },
        );
        check();
      }
    );

  let () =
    test("mem") @@
    (
      () => {
        let (lst, deq) = make();
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', x)) =>
          assert(true == List.mem(x, lst));
          assert(true == D.mem(x, deq));
          assert(false == D.mem(x, deq'));
          ();
        };
      }
    );

  let () =
    test("memq") @@
    (
      () => {
        let lst = List.map(x => ref(x), make_list(input_size));
        let deq = D.of_list(lst);
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', x)) =>
          assert(true == List.memq(x, lst));
          assert(true == D.memq(x, deq));
          assert(false == D.memq(x, deq'));
          let y = ref(x^);
          assert(true == List.mem(y, lst));
          assert(true == D.mem(y, deq));
          assert(false == List.memq(y, lst));
          assert(false == D.memq(y, deq));
          ();
        };
      }
    );

  let () =
    test("find") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let elt = List.nth(lst, 3);
        let eq = (f, x) => f(x) == elt;
        let x0 = List.find(eq(f), lst);
        let x1 = D.find(eq(g), deq);
        assert(x0 == elt);
        assert(x0 == x1);
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert_not_found(() => List.find(nope(f), lst));
        assert_not_found(() => D.find(nope(g), deq));
        check();
      }
    );

  let () =
    test("find_opt") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let elt = List.nth(lst, 3);
        let eq = (f, x) => f(x) == elt;
        let x0 = List.find_opt(eq(f), lst);
        let x1 = D.find_opt(eq(g), deq);
        assert(x0 == Some(elt));
        assert(x0 == x1);
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert(None == List.find_opt(nope(f), lst));
        assert(None == D.find_opt(nope(g), deq));
        check();
      }
    );

  let () =
    test("find_map") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let elt = List.nth(lst, 3);
        let eq = (f, x) =>
          if (f(x) == elt) {
            Some(x * x);
          } else {
            None;
          };
        let x0 = List.find_map(eq(f), lst);
        let x1 = D.find_map(eq(g), deq);
        assert(x0 == Some(elt * elt));
        assert(x0 == x1);
        let nope = (f, x) => {
          ignore(f(x));
          None;
        };
        assert(None == List.find_map(nope(f), lst));
        assert(None == D.find_map(nope(g), deq));
        check();
      }
    );

  let () =
    test("filter") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) => f(x) mod 2 == 0;
        let lst = List.filter(is_even(f), lst);
        let deq = D.filter(is_even(g), deq);
        assert_eq(lst, deq);
        assert(List.length(lst) > 0);
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert([] == List.filter(nope(f), lst));
        assert(D.is_empty @@ D.filter(nope(g), deq));
        check();
      }
    );

  let () =
    test("find_all") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) => f(x) mod 2 == 0;
        let lst = List.find_all(is_even(f), lst);
        let deq = D.find_all(is_even(g), deq);
        assert_eq(lst, deq);
        assert(List.length(lst) > 0);
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert([] == List.find_all(nope(f), lst));
        assert(D.is_empty @@ D.find_all(nope(g), deq));
        check();
      }
    );

  let () =
    test("partition") @@
    (
      () => {
        let (lst, deq) = make();
        let (f, g, check) = make_fs();
        let is_even = (f, x) => f(x) mod 2 == 0;
        let (lst0, lst1) = List.partition(is_even(f), lst);
        let (deq0, deq1) = D.partition(is_even(g), deq);
        assert_eq(lst0, deq0);
        assert_eq(lst1, deq1);
        assert(List.length(lst0) > 0);
        assert(List.length(lst1) > 0);
        let nope = (f, x) => {
          ignore(f(x));
          false;
        };
        assert([] == fst @@ List.partition(nope(f), lst));
        assert(D.is_empty @@ fst @@ D.partition(nope(g), deq));
        let yeap = (f, x) => {
          ignore(f(x));
          true;
        };
        assert([] == snd @@ List.partition(yeap(f), lst));
        assert(D.is_empty @@ snd @@ D.partition(yeap(g), deq));
        check();
      }
    );

  let () =
    test("assoc & mem_assoc") @@
    (
      () => {
        let (keys, values) = (
          make_list(input_size),
          make_list(input_size),
        );
        let lst = List.combine(keys, values);
        let deq = D.of_list(lst);
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', (k, v))) =>
          assert(v == List.assoc(k, lst));
          assert(v == D.assoc(k, deq));
          assert(List.mem_assoc(k, lst));
          assert(D.mem_assoc(k, deq));
          let lst' = D.to_list(deq');
          assert_not_found(() => List.assoc(k, lst'));
          assert_not_found(() => D.assoc(k, deq'));
          assert(!List.mem_assoc(k, lst'));
          assert(!D.mem_assoc(k, deq'));
          ();
        };
      }
    );

  let () =
    test("assoc_opt") @@
    (
      () => {
        let (keys, values) = (
          make_list(input_size),
          make_list(input_size),
        );
        let lst = List.combine(keys, values);
        let deq = D.of_list(lst);
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', (k, v))) =>
          assert(Some(v) == List.assoc_opt(k, lst));
          assert(Some(v) == D.assoc_opt(k, deq));
          assert(List.mem_assoc(k, lst));
          assert(D.mem_assoc(k, deq));
          let lst' = D.to_list(deq');
          assert(None == List.assoc_opt(k, lst'));
          assert(None == D.assoc_opt(k, deq'));
          assert(!List.mem_assq(k, lst'));
          assert(!D.mem_assq(k, deq'));
          ();
        };
      }
    );

  let () =
    test("assq & mem_assq") @@
    (
      () => {
        let (keys, values) = (
          make_list(input_size),
          make_list(input_size),
        );
        let keys = List.map(x => ref(x), keys);
        let lst = List.combine(keys, values);
        let deq = D.of_list(lst);
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', (k, v))) =>
          assert(v == List.assq(k, lst));
          assert(v == D.assq(k, deq));
          let k' = ref(k^);
          assert_not_found(() => List.assq(k', lst));
          assert_not_found(() => D.assq(k', deq));
          let lst' = D.to_list(deq');
          assert_not_found(() => List.assq(k, lst'));
          assert_not_found(() => D.assq(k, deq'));
          ();
        };
      }
    );

  let () =
    test("assq_opt") @@
    (
      () => {
        let (keys, values) = (
          make_list(input_size),
          make_list(input_size),
        );
        let keys = List.map(x => ref(x), keys);
        let lst = List.combine(keys, values);
        let deq = D.of_list(lst);
        switch (D.unsnoc(deq)) {
        | None => assert(false)
        | Some((deq', (k, v))) =>
          assert(Some(v) == List.assq_opt(k, lst));
          assert(Some(v) == D.assq_opt(k, deq));
          let k' = ref(k^);
          assert(None == List.assq_opt(k', lst));
          assert(None == D.assq_opt(k', deq));
          let lst' = D.to_list(deq');
          assert(None == List.assq_opt(k, lst'));
          assert(None == D.assq_opt(k, deq'));
          ();
        };
      }
    );

  let () =
    test("combine & split") @@
    (
      () => {
        let ((keys, dkeys), (values, dvalues)) = (make(), make());
        let lst = List.combine(keys, values);
        let deq = D.combine(dkeys, dvalues);
        assert_eq(lst, deq);
        let (lst0, lst1) = List.split(lst);
        let (deq0, deq1) = D.split(deq);
        assert(lst0 == keys);
        assert_eq(lst0, deq0);
        assert(lst1 == values);
        assert_eq(lst1, deq1);
      }
    );

  let () =
    test("to_seq & of_seq") @@
    (
      () => {
        let (lst, deq) = make();
        let lst_s = List.to_seq(lst);
        let deq_s = D.to_seq(deq);
        let lst' = List.of_seq(lst_s);
        let deq' = D.of_seq(deq_s);
        assert(lst == lst');
        assert_eq(lst, deq');
      }
    );

  let () =
    test("to_array & of_array") @@
    (
      () => {
        let lst = make_list(input_size);
        let arr = Array.of_list(lst);
        let deq = D.of_array(arr);
        assert_eq(lst, deq);
        let arr' = D.to_array(deq);
        assert(arr == arr');
      }
    );

  let () =
    test("make") @@
    (
      () =>
        for (i in 0 to 100) {
          let deq = D.make(i, "x");
          assert(D.length(deq) == i);
        }
    );

  let () =
    test("init") @@
    (
      () => {
        let (f, g, check) = make_fs();
        let lst = List.init(input_size, f);
        let deq = D.init(input_size, g);
        assert_eq(lst, deq);
        check();
      }
    );

  let random_list = () => List.init(1000, _ => Random.int(100));
  let make_rnd = () => {
    let lst = random_list();
    (lst, D.of_list(lst));
  };

  let rec is_sorted =
    fun
    | []
    | [_] => true
    | [x0, x1, ...xs] => x0 <= x1 && is_sorted([x1, ...xs]);

  let () =
    test("sort & merge") @@
    (
      () => {
        let (lst0, deq0) = make_rnd();
        let lst0 = List.sort(compare, lst0);
        let deq0 = D.sort(compare, deq0);
        assert_eq(lst0, deq0);
        let (lst1, deq1) = make_rnd();
        let lst1 = List.sort(compare, lst1);
        let deq1 = D.sort(compare, deq1);
        assert_eq(lst1, deq1);
        let (f, g, check) = make_fs();
        let count = ref(0);
        let comparing = (f, x, y) => {
          incr(count);
          ignore(f((x, y)));
          compare(x, y);
        };
        let lst = List.merge(comparing(f), lst0, lst1);
        let deq = D.merge(comparing(g), deq0, deq1);
        assert_eq(lst, deq);
        assert(is_sorted(lst));
        assert(count^ <= 2 * D.length(deq));
        check();
      }
    );

  let () =
    test("merge not sorted") @@
    (
      () => {
        let (lst0, deq0) = make_rnd();
        let (lst1, deq1) = make_rnd();
        let (f, g, check) = make_fs();
        let count = ref(0);
        let comparing = (f, x, y) => {
          incr(count);
          ignore(f((x, y)));
          compare(x, y);
        };

        let lst = List.merge(comparing(f), lst0, lst1);
        let deq = D.merge(comparing(g), deq0, deq1);
        assert_eq(lst, deq);
        assert(!is_sorted(lst));
        assert(count^ <= 2 * D.length(deq));
        check();
      }
    );
};

let header = name =>
  Printf.printf(
    "-- %s %s\n%!",
    name,
    String.make(70 - String.length(name) - 4, '-'),
  );

let () = header("Dequeue");
module Test_dequeue = Test(Deque.Dequeue);
let () = Printf.printf("\n%!");

let () = header("Steque");
module Test_steque =
  Test({
    include Deque.Steque;
    let unsnoc = t =>
      switch (uncons(rev(t))) {
      | None => None
      | Some((x, t)) => Some((rev(t), x))
      };
  });
let () = Printf.printf("\n%!");

let () = header("Deck");
module Test_deck = Test(Deque.Deck);
let () = Printf.printf("\n%!");

let () = header("Deckrev");
module Test_deckrev = Test(Deque.Deckrev);
let () = Printf.printf("\n%!");
