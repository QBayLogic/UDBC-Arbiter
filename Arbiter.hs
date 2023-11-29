module Arbiter where

import Clash.Prelude
import Clash.Prelude.Testbench

type Item    a = Maybe a
type Store n a = Vec n (Item a)
type Grant n a = Maybe (Index n, a)
type Request a = Maybe a

udbcArbiter
  :: forall dom n a. (KnownDomain dom, HiddenClockResetEnable dom, KnownNat n, NFDataX a)
  => Signal dom (Vec n (Request a)) -> Signal dom (Grant n a)
udbcArbiter requests = grant
  where
    grant :: Signal dom (Grant n a)
    grant = register Nothing (next <$> store'' <*> grant)

    store :: Signal dom (Store n a)
    store   = register (repeat Nothing) store''
    store'  = rmGranted      <$> store  <*> grant
    store'' = addNewRequests <$> store' <*> requests

    addNewRequests :: Store n a -> Vec n (Request a) -> Store n a
    addNewRequests s v = maybe id (const . Just) <$> v <*> s

    rmGranted :: Store n a -> Grant n a -> Store n a
    rmGranted s = maybe s (\(i, _) -> replace i Nothing s)

    next :: Store n a -> Grant n a -> Grant n a
    next st cur =
      let maxBy myMax = fold myMax (Nothing :> openRI st) in
        (\i -> (i, let Just x = st !! i in x))
           <$> (maxBy (maxLI (fst <$> cur)) <|> maxBy max)

    openRI :: Store n a -> Store n (Index n)
    openRI st = maybe (const Nothing) (const Just) <$> st <*> indicesI

    maxLI :: Ord b => Item b -> Item b -> Item b -> Item b
    maxLI cur a b
      | a < cur && b < cur = max a b
      | a < cur            = a
      | b < cur            = b
      | otherwise          = Nothing

testInput :: HiddenClockResetEnable dom => Signal dom (Vec 3 (Request Int))
testInput = stimuliGenerator
  (  (Nothing :> Nothing :> Nothing :> Nil)
  :> (Just 1  :> Nothing :> Nothing :> Nil)
  :> (Nothing :> Nothing :> Nothing :> Nil)
  :> (Nothing :>  Just 2 :>  Just 3 :> Nil)
  :> ( Just 4 :> Nothing :> Nothing :> Nil)
  :> (Nothing :> Nothing :>  Just 3 :> Nil)
  :> (Nothing :> Nothing :> Nothing :> Nil)
  :> ( Just 5 :>  Just 6 :>  Just 7 :> Nil)
  :> (Nothing :> Nothing :> Nothing :> Nil)
  :> Nil
  )

simulateArbiter :: IO ()
simulateArbiter =
  mapM_ print
    $ sampleN @System 12
    $ bundle (testInput, udbcArbiter testInput)
