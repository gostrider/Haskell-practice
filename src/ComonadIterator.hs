data Iterator a = a :< (Iterator a) deriving (Show)

infixr 5 :<


(#) :: a -> (a -> b) -> b
x # f = f x

infixl 0 #


initialHistory :: Iterator String
initialHistory = "" :< initialHistory


exampleHistory :: Iterator String
exampleHistory =
     "^D"
  :< "^C"
  :< "eat flaming death"
  :< "hello?"
  :< "bye"
  :< "exit"
  :< "quit"
  :< "?"
  :< "help"
  :< "ed"
  :< initialHistory


extract :: Iterator a -> a
extract (cmd :< _) = cmd


next :: Iterator a -> a
next (_ :< (cmd :< _)) = cmd


-- next $ next exampleHistory Error!


next' :: Iterator a -> Iterator a
next' (_ :< iterator) = iterator


next2 :: Iterator a -> a
next2 iterator = next (next' iterator)
-- same as:
-- next2 iterator = extract (next (next' iterator))


extend :: (Iterator a -> a) -> Iterator a -> Iterator a
extend f iter@(_ :< as) = (f iter) :< (extend f as)


next3 :: Iterator a -> a
next3 iter = next (extend next (extend next iter))


next31 :: Iterator a -> a
next31 iter =
  let i1 =     extend (\this -> this # next) iter
      i2 =     extend (\this -> this # next)  i1
  in extract $ extend (\this -> this # next)  i2


-- next123 :: Iterator a -> [a]
-- next123 = method
--       this # next
--   i1> this # next
--   i2> this # next
--   i3> [ i1 # extract, i2 # extract, i3 # extract ]
--
--
-- next123 :: Iterator a -> [a]
-- next123 iter =
--     let i1 =     extend (\this -> this # next) iter
--         i2 =     extend (\this -> this # next)  i1
--         i3 =     extend (\this -> this # next)  i2
--     in extract $ extend (\this -> [ i1 # extract, i2 # extract, i3 # extract ]) i3
--
--
next123 :: Iterator a -> [a]
next123 iter =
  [ next iter
  , next $ extend next iter
  , next $ extend next $ extend next iter
  ]


-- next123456 :: Iterator a -> [a]
-- next123456 = method
--           this # next123
--     w123> this # next123
--     w456> (w123 # extract) ++ (w456 # extract)
--
--
-- next123456 :: Iterator a -> [a]
-- next123456 iter =
--   let w123 =   extend (\this -> this # next123) iter
--       w456 =   extend (\this -> this # next123) w123
--   in extract # extend (\this -> (w123 # extract) ++ (w456 # extract)) w456


-- next123456 :: Iterator a -> [a]
-- next123456 = \_w0 ->
--     next123 _w0 ++ next123 (extend next123 _w0)
