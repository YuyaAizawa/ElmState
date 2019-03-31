module State exposing
  ( State
  , return
  , get
  , put
  , map
  , andThen
  , run
  )

type State s a = State (s -> ( a, s ))

return : a -> State s a
return a =
  State (\s -> ( a, s ))

get : State s s
get = State (\s -> ( s, s ))

put : s -> State s ()
put s = State (\_ -> ( (), s ))

map : (a -> b) -> State s a -> State s b
map f (State x) =
  State <|
    \s ->
      let ( v, t ) = x s in
      ( (f v), t )

andThen : (a -> State s b) -> State s a -> State s b
andThen f (State s) =
  State <| \t ->
    let (a, n) = s t in
    let (State g) = f a in
    g n


run : s -> State s a -> (a, s)
run init (State s) =
  s init

eval : s -> State s a -> a
eval init =
  run init >> Tuple.first

exec : s -> State s a -> s
exec init =
  run init >> Tuple.second