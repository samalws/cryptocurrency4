import Control.Monad.State
import Control.Monad.Maybe

type Money = Int
type Score = Money
type User = ()
type Time = Int

data Signed t = Signed { signed :: t, sigs :: [(User, ())] }
data Txn = Txn { amt :: Money, fee :: Money, from :: User, to :: User, expireTime :: Time }
data BlockCert = BlockCert { certUser :: User, certAmt :: Score }
data Block = Block { txns :: [Signed Txn], certs :: [BlockCert], timestamp :: Time, tail :: Signed Block } | GenesisBlock
data ChainState = ChainState { money :: User -> Money, score :: User -> Score, currTime :: Time, difficulty :: Int }

type ChainMonad = MaybeT (State ChainState)

setMoney :: User -> Money -> ChainState -> ChainState
setMoney u m s = ChainState f (score s) (currTime s) where
  f uu
    | uu == u = m
    | otherwise = money s uu
setScore :: User -> Score -> ChainState -> ChainState
setScore u m s = ChainState (money s) f (currTime s) where
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
getDifficulty :: ChainMonad Int
getDifficulty = do
  s <- lift get
  return $ difficulty s

addMoney :: Money -> User -> ChainMonad ()
addMoney m u = do
  mm <- getMoney u
  s <- lift get
  put $ setMoney u (m + mm) s
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
checkTxn _ _ = Nothing

checkCert :: [User] -> BlockCert -> ChainMonad (User, Score)
checkCert [u] c = do
  guard $ u == (certUser c)
  s <- getScore u
  guard $ s >= (certAmt c)
  addScore (-(certAmt c)) u
  return $ (certUser c, certAmt c)
checkCert _ _ = Nothing

checkBlock :: [User] -> Block -> ChainMonad ()
checkBlock u b = do
  guard $ u == (map fst $ certs b)
  txnFees <- sequence $ map checkTxn $ txns b
  txnCerts <- sequence $ map checkCert $ certs b
  totalScore <- sum $ map snd txnCerts
  t <- getTime
  d <- getDifficulty
  guard $ (timestamp b - t) < (totalScore / d)
  setTime $ timestamp b
  snd $ foldr f ((sum txnFees, totalScore), return ()) txnCerts where
    f ((feesLeft, scoreLeft), m) (usr, scr) = ((feesLeft - amt, scoreLeft - scr), totalScore m >> addMoney usr amt ) where
      amt = (feesLeft * scr) / scoreLeft
  -- adjust difficulty

main = return ()
