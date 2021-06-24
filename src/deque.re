module Dequeue = Dequeue;

module Steque = {
  include List_like.Make(Steque);
  include Steque;
};

module Deck = {
  include List_like.Make(Deck);
  include Deck;
};

module Deckrev = {
  include List_like.Make(Deckrev);
  include Deckrev;
};

include Deck;
