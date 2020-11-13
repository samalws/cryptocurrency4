import Control.Monad.State
import Control.Monad.Trans.Maybe

type Money = Int
type Score = Money
type User = ()
type Time = Int

data Signed t = Signed { signed :: t, sigs :: [(User, ())] }
data Txn = Txn { amt :: Money, fee :: Money, from :: User, to :: User, expireTime :: Time }
data BlockCert = BlockCert { certUser :: User, certAmt :: Score }
data Block = Block { txns :: [Signed Txn], certs :: [BlockCert], timestamp :: Time, tail :: Signed Block } | GenesisBlock
data ChainState = ChainState { money :: User -> Money, score :: User -> Score, currTime :: Time, difficulty :: Float }

type ChainMonad = MaybeT (State ChainState)

setMoney :: User -> Money -> ChainState -> ChainState
setMoney u m s = ChainState f (score s) (currTime s) (difficulty s) where
  f uu
    | uu == u = m
    | otherwise = money s uu
setScore :: User -> Score -> ChainState -> ChainState
setScore u m s = ChainState (money s) f (currTime s) (difficulty s) where
  f uu
    | uu == u = m
    | otherwise = score s uu
allUsers :: ChainState -> [User]
allUsers _ = [] -- TODO need to have something iterable

getMoney :: User -> ChainMonad Money
getMoney u = do
  s <- lift get
  return $ money s u
getScore :: User -> ChainMonad Score
getScore u = do
  s <- lift get
  return $ score s u
getTime :: ChainMonad Time
getTime = do
  s <- lift get
  return $ currTime s
getDifficulty :: ChainMonad Float
getDifficulty = do
  s <- lift get
  return $ difficulty s

addMoney :: Money -> User -> ChainMonad ()
addMoney m u = do
  mm <- getMoney u
  s <- lift get
  put $ setMoney u (m + mm) s
addScore :: Score -> User -> ChainMonad ()
addScore m u = do
  mm <- getScore u
  s <- lift get
  put $ setScore u (m + mm) s
setTime :: Int -> ChainMonad ()
setTime t = do
  s <- lift get
  put $ ChainState (money s) (score s) t (difficulty s)

checkSig :: ([User] -> t -> ChainMonad a) -> Signed t -> ChainMonad a
checkSig f s = do
  f (map fst $ sigs s) (signed s)

checkTxn :: [User] -> Txn -> ChainMonad Money
checkTxn [u] t = do
  guard $ u == (from t)
  m <- getMoney u
  guard $ m >= (amt t + fee t)
  tm <- getTime
  guard $ tm < expireTime t
  addMoney (-(amt t) - (fee t)) u
  addMoney (amt t) (to t)
  return $ fee t
checkTxn _ _ = MaybeT $ return Nothing

checkCert :: BlockCert -> ChainMonad (User, Score)
checkCert c = do
  u <- return $ certUser c
  s <- getScore u
  guard $ s >= (certAmt c)
  addScore (-(certAmt c)) u
  return $ (certUser c, certAmt c)

checkBlock :: [User] -> Block -> ChainMonad ()
checkBlock u b = do
  guard $ u == (map certUser $ certs b)
  txnFees <- sequence $ map (checkSig checkTxn) $ txns b
  txnCerts <- sequence $ map checkCert $ certs b
  totalScore <- return $ sum $ map snd txnCerts
  t <- getTime
  d <- getDifficulty
  guard $ (timestamp b - t) < (round $ fromIntegral totalScore * d)
  setTime $ timestamp b
  snd $ foldr f ((sum txnFees, totalScore), return ()) txnCerts where
    f :: (User, Score) -> ((Money, Score), ChainMonad ()) -> ((Money, Score), ChainMonad ())
    f (usr, scr) ((feesLeft, scoreLeft), m) = ((feesLeft - amt, scoreLeft - scr), m >> addMoney amt usr) where
      amt = (feesLeft * scr) `div` scoreLeft
  -- adjust difficulty

main = return ()
