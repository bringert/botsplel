import Control.Monad.State
import Data.Char
import Data.List hiding (transpose, delete, insert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import System.Random

{-
Word error classes:
 * insertion
 * transposition
 * substitution
 * deletion
 * double initial capitalization
 * phonetic

Inter-word errors:
 * split compounds
 * missing spaces

Need to take into account:

 * letters on the keyboard (keyboard layout dependent)
   - insertion
 * proximity of keyboard keys (keyboard layout dependent)
   - insertion (when hitting two keys instead of one)
   - substitution (Grundin, 1983)
 * letter frequency (language dependent)
   - substition favors higher-frequency letters (Grundin, 1983)
-}

--
-- * Simple random monad
--

type Rand a = State StdGen a

inState :: MonadState s m => (s -> (a,s)) -> m a
inState f = do
            s <- get
            let (x,s') = f s
            put s'
            return x

getRandomR :: Random a => (a,a) -> Rand a
getRandomR r = inState (randomR r)

runRand :: Rand a -> StdGen -> (a, StdGen)
runRand = runState

runRandSeed :: Rand a -> Int -> a
runRandSeed r s = fst $ runRand r (mkStdGen s)

runRandIO :: Rand a -> IO a
runRandIO r = getStdRandom (runRand r)

--
-- * Error configuration
--

data Conf = Conf 
    {
     errorsPerChar :: Double,
     transformations :: FreqTab (String -> Rand String)
    }

--
-- * Introduce errors
--

transform :: Conf -> String -> Rand String
transform c s = iterateM (length s) (oneErrorP c (errorsPerChar c)) s

-- | With a given probability, introduce one random error 
--   into the string.
oneErrorP :: Conf -> Double -> String -> Rand String
oneErrorP c p s = do
                  r <- getRandomR (0,1.0)
                  if r <= p then
                     oneError c s
                   else 
                     return s

-- | Introduce one random error into the string.
oneError :: Conf -> String -> Rand String
oneError c s = randomF (transformations c) >>= \t -> t s



--
-- * Frequency table based random generation
--

data FreqTab a = FreqTab Int [(a,Int)]
  deriving Show

mkFreqTab :: [(a,Int)] -- ^ elements and relative frequencies.
          -> FreqTab a
mkFreqTab t = uncurry FreqTab $ mapAccumL addFreq 0 t
  where addFreq s (x,f) = (f+s,(x,f+s))

mapFreqTab :: (a -> Int -> Int) -> FreqTab a -> FreqTab a
mapFreqTab f (FreqTab s t) = FreqTab s' t'
  where t' = [ (x, f x y) | (x,y) <- t ]
        s' = if null t' then s else snd (last t')

randomF :: FreqTab a -> Rand a
randomF (FreqTab s t) 
    | s < 1 || null t = error "Empty frequency table"
    | otherwise = do
                  r <- getRandomR (1,s)
                  return $ head $ [ x | (x,f) <- t, r <= f ]


--
-- * Confusion matrix
--

type Confusion a = Map a (FreqTab a)

mkConfusion :: Ord a => [(a,[(a,Int)])] -> Confusion a
mkConfusion m = Map.fromList [ (x, mkFreqTab t) | (x,t) <- m ]

-- | Make a confusion matrix based on the distance between
--   elements in a 2D grid. 
mkProximityConfusion :: Ord a => Int -- ^ frequency of (x,x)
                     -> [(a,(Int,Int))] -> Confusion a
mkProximityConfusion i xs = mkConfusion ys
  where ys = [ (x,[ (y,d px py) | (y,py) <- xs]) | (x,px) <- xs ]
        d p1@(x1,y1) p2@(x2,y2) | p1 == p2 = i
                                | otherwise = round (m / s)
           where s = sqrt (fromIntegral ((x2-x1)^2 + (y2-y1)^2))
        m = 10.0

mapConfusion :: (a -> a -> Int -> Int) -> Confusion a -> Confusion a
mapConfusion f = Map.mapWithKey (\x t -> mapFreqTab (f x) t)

-- | Multiply the frequencies by the given frequencies.
adjustConfusion :: Ord a => [(a,Int)] -> Confusion a -> Confusion a
adjustConfusion m = mapConfusion (\_ y f -> lookup' y m * f)

confuseR :: Ord a => Confusion a -> a -> Rand a
confuseR m x = case Map.lookup x  m of
                   Nothing -> return x
                   Just t -> randomF t



--
-- * Single letter transposition
--

transpose :: Int -- 0 .. (length s - 2)
          -> String -> String
transpose n s | l < 2 = s
              | otherwise = x ++ z:y:w
  where l = length s
        (x,y:z:w) = splitAt n s

transposeR :: String -> Rand String
transposeR s = do
               n <- getRandomR (0,length s - 2)
               return (transpose n s)

--
-- * Single letter insertion
-- 

insert :: Int  -- 0 .. length s
       -> Char -- char to insert
       -> String -> String
insert n c s = x ++ c:y
  where (x,y) = splitAt n s

-- FIXME: maybe this should look at the adjacent letters
insertR :: Rand Char -> String -> Rand String
insertR f s = do
              n <- getRandomR (0,length s)
              c <- f
              return (insert n c s)

--
-- * Single letter substitution
--

substitute :: Int -- 0 .. (length s - 1)
           -> Char -- char to substiutue for the existing one
           -> String -> String
substitute n c s = x ++ c:y
  where (x,_:y) = splitAt n s


substituteR :: (Char -> Rand Char) -> String -> Rand String
substituteR f s = do
                  n <- getRandomR (0, length s - 1)
                  c <- f (s!!n)
                  return (substitute n c s)

--
-- * Single letter deletion
--

delete :: Int -- 0 .. (length s - 1)
       -> String -> String
delete n s = x ++ y
  where (x,_:y) = splitAt n s

deleteR :: String -> Rand String
deleteR s = do
            n <- getRandomR (0,length s - 1)
            return (delete n s)

--
-- * Add more initial capitals
--

oneMoreInitCap :: String -> String
oneMoreInitCap s 
    | length s >= 2 && not (null x) = x ++ map toUpper c ++ cs
    | otherwise = s
  where (x,y) = span isUpper s
        (c,cs) = splitAt 1 y

oneMoreInitCapR :: String -> Rand String
oneMoreInitCapR = return . oneMoreInitCap

--
-- * Whole word substitutions
--

substituteWord :: Map String [String] -> String 
               -> [String] -- ^ a possibly empty list of alternatives
substituteWord m s = Map.findWithDefault [] s m


--
-- * Utilities
--

iterateM :: Monad m => Int -> (a -> m a) -> a -> m a
iterateM 0 f x = return x
iterateM n f x = f x >>= iterateM (n-1) f 

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' x xs = fromMaybe (error "lookup', key not found") (lookup x xs)

--
-- * English config
--

basicEnglishConf = 
    Conf {
          errorsPerChar = 0.1,
          transformations = 
              mkFreqTab [
                         (transposeR, 10),
                         (insertR randomLetterEng, 10),
                         (substituteR (confuseR confusionEng), 10),
                         (deleteR, 10),
                         (oneMoreInitCapR, 10)
                        ]
         }


{- 
From http://en.wikipedia.org/wiki/Talk:Letter_frequencies

In the book The Code Book: The Science of Secrecy from Ancient
Egypt to Quantum Cryptography by Simon Singh, I found the following
table with a caption that reads:

  This table of relative frequencies is based on passages taken from
  newspapers and novels, and the total sample was 100,362 alphabetic
  characters. The table was compiled by H. Beker and F. Piper, and
  originally published in Cipher Systems: The Protection Of
  Communication.  
-}

letterFreqsEng :: [(Char,Int)]
letterFreqsEng =
             [('a',  82),
              ('b',  15),
              ('c',  28),
              ('d',  43),
              ('e', 127),
              ('f',  22),
              ('g',  20),
              ('h',  61),
              ('i',  70),
              ('j',   2),
              ('k',   8),
              ('l',  40),
              ('m',  24),
              ('n',  67),
              ('o',  75),
              ('p',  19),
              ('q',   1),
              ('r',  60),
              ('s',  63),
              ('t',  91),
              ('u',  28),
              ('v',  10),
              ('w',  24),
              ('x',   2),
              ('y',  20),
              ('z',   1)]

letterFreqTabEng :: FreqTab Char
letterFreqTabEng = mkFreqTab letterFreqsEng

randomLetterEng :: Rand Char
randomLetterEng = randomF letterFreqTabEng


confusionEng :: Confusion Char
confusionEng = adjustConfusion letterFreqsEng $ mkProximityConfusion 0
    (zipWith (\c x -> (c,(0,x))) "qwertyuiop" [0..]
  ++ zipWith (\c x -> (c,(1,x))) "asdfghjkl" [0..]
  ++ zipWith (\c x -> (c,(2,x))) "zxcvbnm" [0..])
