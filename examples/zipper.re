type t('a) = {
  left: Deque.t('a),
  focus: 'a,
  right: Deque.t('a),
};

let of_deque = deq =>
  switch (Deque.uncons(deq)) {
  | None => None
  | Some((focus, right)) => Some({left: Deque.empty, focus, right})
  };

let focus = t => t.focus;

let go_right = t =>
  switch (Deque.uncons(t.right)) {
  | None => None
  | Some((focus, right)) =>
    Some({left: Deque.snoc(t.left, t.focus), focus, right})
  };

let go_left = t =>
  switch (Deque.unsnoc(t.left)) {
  | None => None
  | Some((left, focus)) =>
    Some({left, focus, right: Deque.cons(t.focus, t.right)})
  };

let to_deque = t => Deque.(t.left @ cons(t.focus, t.right));
