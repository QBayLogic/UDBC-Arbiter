-----------------------------------------------------------------------------
-- |
-- Module: Arbiter
-- Maintainer: QBayLogic B.V. <devops@qbaylogic.com>
--
-- A Clash arbiter implementation supporting a bounded number of
-- clients and arbitrary (theoretically unbounded) data packages to be
-- exchanged. Data packages additionally are classified according to
-- LOW or HIGH security levels. The given implementation has no
-- particular practical relevance, but is more of theoretical interest
-- in term of safety and security property verification.
--
-----------------------------------------------------------------------------

module Arbiter where

-----------------------------------------------------------------------------

import Data.Proxy
import Clash.Prelude
import Clash.Prelude.Testbench

-----------------------------------------------------------------------------

-- | The input request type of the arbiter, as offered by clients.
data Request a =
    NoRq
    -- ^ The client does not request anything.
  | Lo a
    -- ^ The client places a request with the given low data packet.
  | Hi a
    -- ^ The client places a request with the given high data packet.
  deriving (Generic, NFDataX, Show)

-- | The outputted grant type of the arbiter.
data Grant n a =
    NoRequest
    -- ^ There is currently no open request to be served.
  | ReqFrom (Index n) a
    -- ^ The client with the given index has placed a request with the
    -- attached data package.
  | AccessDenied
    -- ^ The security policy does not allow any insights here.
  deriving (Generic, NFDataX, Show)

-- | The security level, as used by the internal state.
data SecurityLevel =
    L -- ^ low security data
  | H -- ^ high security data
  deriving (Generic, NFDataX, Show)

-- | An element of the store.
type Item a = Maybe a

-- | The internal state for holing the open requests and attached data
-- packets.
type Store n a = Vec n (Item a)

-- | A data packet with an attached security level.
type Sec a = (a, SecurityLevel)

-----------------------------------------------------------------------------

-- | The bounded client / unbounded data arbiter.
udbcArbiter ::
  forall dom n a.
  (KnownDomain dom, HiddenClockResetEnable dom, KnownNat n, NFDataX a) =>
  Signal dom (Vec n (Request a)) -> Signal dom (Grant n a, Grant n a)
udbcArbiter requests = bundle
  ( permitted L <$> grant -- LOW  security output channel
  , permitted H <$> grant -- HIGH security output channel
  )
 where
  -- restricts data access according to the given security level
  permitted H (Just (i, (x, _))) = ReqFrom i x
  permitted L (Just (i, (x, L))) = ReqFrom i x
  permitted L (Just (_, (_, H))) = AccessDenied
  permitted _ _                  = NoRequest

  -- the store for holding the still open requests
  store0, store1, store2 :: Signal dom (Store n (Sec a))
  store0 = register (repeat Nothing) store2
  store1 = rmGranted      <$> store0 <*> prevSelected
  store2 = addNewRequests <$> store1 <*> requests

  -- the currently selected grant
  grant =
    (\st -> maybe Nothing (\i -> (i, ) <$> (st !! i)))
      <$> store2 <*> selected

  -- the currently and previously selected index to be granted
  selected, prevSelected :: Signal dom (Maybe (Index n))
  selected = next <$> store2 <*> prevSelected
  prevSelected = register Nothing selected

  -- adds the new requests of the current cycle to the store
  addNewRequests :: Store n (Sec a) -> Vec n (Request a) -> Store n (Sec a)
  addNewRequests st rs = add <$> rs <*> st
   where
    add NoRq   = id
    add (Lo x) = const $ Just (x, L)
    add (Hi x) = const $ Just (x, H)

  -- removes the last granted element from the store
  rmGranted :: Store n b -> Maybe (Index n) -> Store n b
  rmGranted s = maybe s (\i -> replace i Nothing s)

  -- determines the next index in the store to be selected using a
  -- round robin strategy. The maximum index being smaller than the
  -- given index is chosen, if it exists. Otherwise the maximum of all
  -- elements is taken.
  next :: Store n b -> Maybe (Index n) -> Maybe (Index n)
  next st cur = maxBy (maxLI cur) <|> maxBy max
   where
    -- select the maximum of all elements according to the given
    -- implementation
    maxBy myMax = fold myMax (Nothing :> filledPositions)

    -- all indices that reference non-empty data elements in the store
    filledPositions =
      maybe (const Nothing) (const Just) <$> st <*> indicesI

    -- choose the maximal element among the second and third argument
    -- that is strictly smaller than the first argument
    maxLI :: Ord b => Item b -> Item b -> Item b -> Item b
    maxLI cur a b
      | a < cur && b < cur = max a b
      | a < cur            = a
      | b < cur            = b
      | otherwise          = Nothing

-----------------------------------------------------------------------------

-- | Some test input to see the UDBC-arbiter in action.
testInput :: HiddenClockResetEnable dom => Signal dom (Vec 3 (Request Int))
testInput = stimuliGenerator
  (  (NoRq :> NoRq :> NoRq :> Nil)
  :> (Lo 1 :> NoRq :> NoRq :> Nil)
  :> (NoRq :> NoRq :> NoRq :> Nil)
  :> (NoRq :> Hi 2 :> Lo 3 :> Nil)
  :> (Hi 4 :> NoRq :> NoRq :> Nil)
  :> (NoRq :> NoRq :> Lo 3 :> Nil)
  :> (NoRq :> NoRq :> NoRq :> Nil)
  :> (Lo 5 :> Hi 6 :> Hi 7 :> Nil)
  :> (NoRq :> NoRq :> NoRq :> Nil)
  :> Nil
  )

-- | Simulates the UDBC Arbiter on 'testInput'.
simulateArbiter :: IO ()
simulateArbiter = do
  mapM_ pr $ sampleN @System 12 $ bundle (testInput, udbcArbiter testInput)
  putStrLn sep
 where
  pr (inp, (l, h)) = do
    putStrLn sep
    putStrLn $ "Inputs:   " <> show inp
    putStrLn $ "Low  Out: " <> show l
    putStrLn $ "High Out: " <> show h

  sep = "--------------------------------------"

-----------------------------------------------------------------------------
