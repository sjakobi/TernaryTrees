module Data.Map.StringMap (
                StringMap,
                insert,
                singleton,
                member,
                size,
                fromList,
                lookup,
                (!),
                findWithDefault,
                insertWith,
                insertWithKey,
                keys,
                assocs,
                elems,
                null,
                CompactStringMap(..)
                ) where
import Data.Map.StringMap.Internal
import Data.Bits
import Data.Binary
import Data.Binary.Serialise.CBOR (Serialise)
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.Binary.Serialise.CBOR.Encoding as CBOR
import qualified Data.Binary.Serialise.CBOR.Decoding as CBOR
import qualified Data.Serialize as Serialize
import Data.List (genericLength)
import Data.Monoid
import Control.Monad
import Control.Arrow (first)
import Prelude hiding (null,lookup)



-- | Quickly build a tree without an initial tree. This should be used
-- to create an initial tree, using insert there after.
singleton :: String -> v -> StringMap v
singleton (x:xs) v = Node x End (singleton xs v) End
singleton []     v = Null v End

-- | Inserts an entrie into a tree. Values with the same key will be replaced
-- with the newer value.
insert :: String -> v -> StringMap v -> StringMap v
insert xss@(_:_)  v End              = singleton xss v
insert xss@(_:_)  v (Null v' rest)   = Null v' $ insert xss v rest
insert []         v End              = Null v End
insert []         v (Node ele l e h) = Node ele (insert [] v l) e h
insert []         v (Null _ rest)    = Null v rest
insert xss@(x:xs) v (Node ele l e h) =
    case compare x ele of
        LT -> Node ele (insert xss v l) e h
        EQ -> Node ele l (insert xs v e) h
        GT -> Node ele l e (insert xss v h)


-- | Inserts a new value into the tree with a given function that combines the new value
-- and the old value together to for a new entry.
--
-- > insertWith f key newval (fromList [(notkey,val1),(key,oldval)]) == fromList [(notkey,val1),(key,f newval oldval)]
insertWith ::(v -> v -> v) -> String -> v -> StringMap v -> StringMap v
insertWith _ xss@(_:_)  v End              = singleton xss v
insertWith f xss@(_:_)  v (Null v' rest)   = Null (f v v') $ insertWith f xss v rest
insertWith _ []         v End              = Null v End
insertWith f []         v (Node ele l e h) = Node ele (insertWith f [] v l) e h
insertWith _ []         v (Null _ rest)    = Null v rest
insertWith f xss@(x:xs) v (Node ele l e h) =
    case compare x ele of
        LT -> Node ele (insertWith f xss v l) e h
        EQ -> Node ele l (insertWith f xs v e) h
        GT -> Node ele l e (insertWith f xss v h)


-- | Inserts a new value into the tree with a given function that combines the new value
-- and the old value together to for a new entry.
--
-- > insertWithKey f key newval (fromList [(notkey,val1),(key,oldval)]) == fromList [(notkey,val1),(key,f key newval oldval)]
insertWithKey :: (String -> v -> v -> v) -> String -> v -> StringMap v -> StringMap v
insertWithKey f ks v m = insertWith (f ks) ks v m
-- | Returns true if the `String` is a key in the TernaryMap.
member :: String -> StringMap v -> Bool
member         _   End             = False
member         [] (Null _ _)       = True
member         [] (Node _ l _ _)   = member [] l
member xss@(_:_)  (Null _ rest)    = member xss rest
member xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> member xss l
        EQ -> member  xs e
        GT -> member xss h


lookup :: String -> StringMap v -> Maybe v
lookup _ End                       = Nothing
lookup [] (Null v _)               = Just v
lookup [] (Node _ l _ _)           = lookup [] l
lookup xs (Null _ rest)            = lookup xs rest
lookup xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> lookup xss l
        EQ -> lookup  xs e
        GT -> lookup xss h

(!) :: StringMap v -> String -> Maybe v
(!) = flip lookup

findWithDefault :: v -> String -> StringMap v -> v
findWithDefault k _ End                       = k
findWithDefault _ [] (Null v _)               = v
findWithDefault k [] (Node _ l _ _)           = findWithDefault k [] l
findWithDefault k xs (Null _ rest)            = findWithDefault k xs rest
findWithDefault k xss@(x:xs) (Node ele l e h) =
    case compare x ele of
        LT -> findWithDefault k xss l
        EQ -> findWithDefault k  xs e
        GT -> findWithDefault k xss h

-- | Returns the number of non-Val Elems. not exported
treeSize :: StringMap v -> Int
treeSize End = 0
treeSize (Node _ l e h) = 1 + treeSize l + treeSize e + treeSize h
treeSize (Null _ rest) = treeSize rest

-- | Counts how many entries there are in the tree.
size :: StringMap v -> Int
size End            = 0
size (Node _ l e h) = size l + size e + size h
size (Null _ rest)  = 1 + size rest

-- | Creates a new tree from a list of 'strings'
fromList :: [(String,v)] -> StringMap v
fromList = foldl (\tree (as,v) -> insert as v tree) empty

-- | An empty map.
empty :: StringMap v
empty = End

-- | Makes a list of all the values in the map.
elems :: StringMap v -> [v]
elems  End           = []
elems (Node _ l e h) = elems l ++ (elems e ++ elems h)
elems (Null v rest)  = v : elems rest

-- | Returns a (sorted) list of all keys in the map.
keys :: StringMap v -> [String]
keys End              = []
keys (Null _ rest)    = []:keys rest
keys (Node ele l e g) = keys l ++ map (ele:) (keys e) ++ keys g


-- | Returns a (sorted) list of all keys in the map.
assocs :: StringMap v -> [(String,v)]
assocs  End             = []
assocs (Null v rest)    = ([],v):assocs rest
assocs (Node ele l e g) = assocs l ++ map (first (ele:)) (assocs e) ++ assocs g

-- | Returns true if the map is empty.
null :: StringMap v -> Bool
null End = True
null _   = False

-- keySet :: StringMap v -> S.TernarySet a
-- keySet End = S.End
-- keySet (Node (C x) l e h) = S.Node (S.C x) (keySet l) (keySet e) (keySet h)
-- keySet (Node (Val _) l e h) = S.Node (S.Null) (keySet l) (keySet e) (keySet h)


instance Functor (StringMap) where
    fmap _ End = End
    fmap f (Null v rest)    = Null (f v) (fmap f rest)
    fmap f (Node ele l e h) = Node ele (fmap f l) (fmap f e) (fmap f h)

-- | A rather long Binary instance, that uses binary numbers to indicate
-- where Ends are efficiently.
instance Binary v => Binary (StringMap v) where
    put (Node ch End End End) = do
        putWord8 0
        put ch
    put (Node ch End End h) = do
        putWord8 1
        put ch
        put h
    put (Node ch End e End) = do
        putWord8 2
        put ch
        put e
    put (Node ch End e h) = do
        putWord8 3
        put ch
        put e
        put h
    put (Node ch l End End) = do
        putWord8 4
        put ch
        put l
    put (Node ch l End h) = do
        putWord8 5
        put ch
        put l
        put h
    put (Node ch l e End) = do
        putWord8 6
        put ch
        put l
        put e
    -- General case
    put (Node ch l e h) = do
        putWord8 7
        put ch
        put l
        put e
        put h
    put (Null v End) = putWord8 8 >> put v
    put (Null v rest) = do
        putWord8 9
        put v
        put rest
    put End = putWord8 10

    get = do
        tag <- getWord8
        case tag of
            _ | tag < 8 ->
                do
                    ch <- get
                    l <- if tag `testBit` 2 then get else return End
                    e <- if tag `testBit` 1 then get else return End
                    h <- if tag `testBit` 0 then get else return End
                    return (Node ch l e h)
            8 -> liftM (flip Null End) get
            9 -> liftM2 Null get get
            10 -> return End
            _ -> error ("Invalid data in binary stream. tag: " ++ show tag)

instance Serialize.Serialize v => Serialize.Serialize (StringMap v) where
  put (Node ch End End End) = do
    Serialize.putWord8 0
    Serialize.put ch
  put (Node ch End End h) = do
    Serialize.putWord8 1
    Serialize.put ch
    Serialize.put h
  put (Node ch End e End) = do
    Serialize.putWord8 2
    Serialize.put ch
    Serialize.put e
  put (Node ch End e h) = do
    Serialize.putWord8 3
    Serialize.put ch
    Serialize.put e
    Serialize.put h
  put (Node ch l End End) = do
    Serialize.putWord8 4
    Serialize.put ch
    Serialize.put l
  put (Node ch l End h) = do
    Serialize.putWord8 5
    Serialize.put ch
    Serialize.put l
    Serialize.put h
  put (Node ch l e End) = do
    Serialize.putWord8 6
    Serialize.put ch
    Serialize.put l
    Serialize.put e
  -- General case
  put (Node ch l e h) = do
    Serialize.putWord8 7
    Serialize.put ch
    Serialize.put l
    Serialize.put e
    Serialize.put h
  put (Null v End) = do
    Serialize.putWord8 8
    Serialize.put v
  put (Null v rest) = do
    Serialize.putWord8 9
    Serialize.put v
    Serialize.put rest
  put End = Serialize.putWord8 10

  get = do
    tag <- Serialize.getWord8
    case tag of
      _ | tag < 8 -> do
        ch <- Serialize.get
        l <- if tag `testBit` 2 then Serialize.get else return End
        e <- if tag `testBit` 1 then Serialize.get else return End
        h <- if tag `testBit` 0 then Serialize.get else return End
        return (Node ch l e h)
      8 -> liftM (flip Null End) Serialize.get
      9 -> liftM2 Null Serialize.get Serialize.get
      10 -> return End
      _ -> error ("Invalid data in binary stream. tag: " ++ show tag)

instance Serialise a => Serialise (StringMap a) where
  encode (Node ch l e h) =
    CBOR.encodeListLen 4 <> CBOR.encode ch <> foldMap CBOR.encode [l, e, h]
  encode (Null v End) =
    CBOR.encodeListLen 1 <> CBOR.encode v
  encode (Null v rest) =
    CBOR.encodeListLen 2 <> CBOR.encode v <> CBOR.encode rest
  encode End = CBOR.encodeListLen 0

  decode = do
    len <- CBOR.decodeListLen
    case len of
      0 -> return End
      1 -> do
        v <- CBOR.decode
        return (Null v End)
      2 -> do
        v <- CBOR.decode
        rest <- CBOR.decode
        return (Null v rest)
      4 -> do
        ch <- CBOR.decode
        [l, e, h] <- replicateM 3 CBOR.decode
        return (Node ch l e h)
      _ -> fail ("bad length: " ++ show len)

encodeTaggedNode :: Serialise a => Word -> Char -> [StringMap a] -> CBOR.Encoding
encodeTaggedNode tag ch ms =
    CBOR.encodeListLen listLen <> CBOR.encode tag <> CBOR.encode ch <> foldMap CBOR.encode ms
  where listLen = 2 + genericLength ms

-- | Just a 'StringMap' with a non-CBOR-compliant, compact 'Serialise' instance
newtype CompactStringMap a = CompactStringMap (StringMap a)
  deriving (Eq, Show)

instance Serialise a => Serialise (CompactStringMap a) where
  encode (CompactStringMap m) =
    case m of
      Node ch End End End ->
        CBOR.encodeSimple 0 <> CBOR.encode ch
      Node ch End End h ->
        compactEncodeNode 1 ch [h]
      Node ch End e End ->
        compactEncodeNode 2 ch [e]
      Node ch End e h ->
        compactEncodeNode 3 ch [e, h]
      Node ch l End End ->
        compactEncodeNode 4 ch [l]
      Node ch l End h ->
        compactEncodeNode 5 ch [l, h]
      Node ch l e End ->
        compactEncodeNode 6 ch [l, e]
      Node ch l e h ->
        compactEncodeNode 7 ch [l, e, h]
      Null v End ->
        CBOR.encodeSimple 8 <> CBOR.encode v
      Null v rest ->
        CBOR.encodeSimple 9 <> CBOR.encode v <> CBOR.encode (CompactStringMap rest)
      End ->
        CBOR.encodeSimple 10

  decode = do
    tag <- CBOR.decodeSimple
    case tag of
      _ | tag < 8 -> do
        ch <- CBOR.decode
        CompactStringMap l <-
          if tag `testBit` 2
            then CBOR.decode
            else return compactEnd
        CompactStringMap e <-
          if tag `testBit` 1
            then CBOR.decode
            else return compactEnd
        CompactStringMap h <-
          if tag `testBit` 0
            then CBOR.decode
            else return compactEnd
        return $ CompactStringMap (Node ch l e h)
      8 -> do
        v <- CBOR.decode
        return (CompactStringMap (Null v End))
      9 -> do
        v <- CBOR.decode
        CompactStringMap rest <- CBOR.decode
        return (CompactStringMap (Null v rest))
      10 -> return (CompactStringMap End)
      _ -> fail ("Invalid data in binary stream. tag: " ++ show tag)

compactEncodeNode :: Serialise a => Word8 -> Char -> [StringMap a] -> CBOR.Encoding
compactEncodeNode tag ch ms = CBOR.encodeSimple tag
                           <> CBOR.encode ch
                           <> foldMap (CBOR.encode . CompactStringMap) ms

compactEnd :: CompactStringMap a
compactEnd = CompactStringMap End
