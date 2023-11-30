{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DCLabel (module DCLabel) where

import App
import Enclave
import Data.Dynamic
import Data.Typeable
import Data.Set (Set)
import qualified Data.Set as S

newtype Principal = Principal { principalName :: String }
  deriving (Ord, Show, Eq, Typeable, Read)

principal :: String -> Principal
principal = Principal


-- Disjunctive clauses
newtype Disjunction = Disjunction { dToSet :: Set Principal }
  deriving (Ord, Show, Eq, Typeable, Read)

instance Semigroup Disjunction where
  (<>) = dUnion

instance Monoid Disjunction where
  mempty = dFalse

dFalse :: Disjunction
dFalse = Disjunction S.empty

dSingleton :: Principal -> Disjunction
dSingleton p = Disjunction (S.singleton p)

dUnion :: Disjunction -> Disjunction -> Disjunction
dUnion (Disjunction ps1) (Disjunction ps2) =
  Disjunction (S.union ps1 ps2)

-- | Convert a list of 'Principal's into a 'Disjunction'.
dFromList :: [Principal] -> Disjunction
dFromList pl = Disjunction (S.fromList pl)

-- | Returns 'True' iff the first disjunction is a subset of the second.
{-

A \/ B => A \/ B \/ C

If (A \/ B) holds and this is a subset of (A \/ B \/ C)
then atleast a subset of the disjunction holds. If one part
of the disjunction holds that sufficient for the entire
formula to hold.
-}
dImplies :: Disjunction -> Disjunction -> Bool
dImplies (Disjunction ps1) (Disjunction ps2)
  = ps1 `S.isSubsetOf` ps2


-- Conjunctive Normal Form (CNF) Formulas
newtype CNF = CNF { cToSet :: Set Disjunction }
  deriving (Ord, Show, Eq, Typeable, Read)

instance Semigroup CNF where
  (<>) = cUnion

instance Monoid CNF where
  mempty = cTrue

-- | A 'CNF' that is always @True@--i.e., trivially satisfiable.  When
-- @'dcSecrecy' = cTrue@, it means data is public.  When
-- @'dcIntegrity' = cTrue@, it means data carries no integrity
-- guarantees.  As a description of privileges, @cTrue@ conveys no
-- privileges; @'canFlowToP' cTrue l1 l2@ is equivalent to
-- @'canFlowTo' l1 l2@.
--
-- Note that @'toCNF' 'True' = cTrue@.  Hence @'dcPublic' = 'DCLabel'
-- cTrue cTrue@.
cTrue :: CNF
cTrue = CNF $ S.empty

-- | A 'CNF' that is always @False@.  If @'dcSecrecy' = cFalse@, then
-- no combination of principals is powerful enough to make the data
-- public.  For that reason, @cFalse@ generally shouldn't appear in a
-- data label.  However, it is convenient to include as the
-- 'dcSecrecy' component of 'lioClearance' to indicate a thread may
-- arbitrarily raise its label.
--
-- @'dcIntegrity' = cFalse@ indicates impossibly much integrity--i.e.,
-- data that no combination of principals is powerful enough to modify
-- or have created.  Generally this is not a useful concept.
--
-- As a privilege description, @cFalse@ indicates impossibly high
-- privileges (i.e., higher than could be achieved through any
-- combination of 'Principal's).  @cFalse ``speaksFor`` p@ for any
-- 'CNF' @p@.  This can be a useful concept for bootstrapping
-- privileges within the 'DC' monad itself.  For instance, the result
-- of @'privInit' cFalse@ can be passed to fully-trusted 'DC' code,
-- which can in turn use 'delegate' to create arbitrary finite
-- privileges to pass to less privileged code.
cFalse :: CNF
cFalse = CNF $ S.singleton dFalse

cSingleton :: Disjunction -> CNF
cSingleton = CNF . S.singleton

-- Does any element in `Set a` satisfies the predicate `prd`
setAny :: (a -> Bool) -> Set a -> Bool
setAny prd = S.foldr' (\a -> (prd a ||)) False

-- Do all the elements in `Set a` satisfies the predicate `prd`
setAll :: (a -> Bool) -> Set a -> Bool
setAll prd = S.foldr' (\a -> (prd a &&)) True

{-
does the disjunction imply any of the formulaes in the CNF?
if yes, no need to extend the CNF
if no, then add `dnew` to the conjunction in the CNF that
       `dNew` cannot be reduced to
-}
cInsert :: Disjunction -> CNF -> CNF
cInsert dnew c@(CNF ds)
  | setAny (`dImplies` dnew) ds = c
  | otherwise = CNF $ S.insert dnew $ S.filter (not . (dnew `dImplies`)) ds

cUnion :: CNF -> CNF -> CNF
cUnion c (CNF ds) = S.foldr cInsert c ds

cOr :: CNF -> CNF -> CNF
cOr (CNF ds1) (CNF ds2) =
  cFromList $ [dUnion d1 d2 | d1 <- S.toList ds1, d2 <- S.toList ds2]

-- | Convert a list of 'Disjunction's into a 'CNF'.  Mostly useful if
-- you wish to de-serialize a 'CNF'.
cFromList :: [Disjunction] -> CNF
cFromList = S.foldr cInsert cTrue . S.fromList

cImplies1 :: CNF -> Disjunction -> Bool
cImplies1 (CNF ds) d = setAny (`dImplies` d) ds

cImplies :: CNF -> CNF -> Bool
cImplies c (CNF ds) = setAll (c `cImplies1`) ds


--
-- DCLabel
--

data DCLabel = DCLabel { dcSecrecy :: CNF
                         -- ^ Describes the authority required to make
                         -- the data public.
                       , dcIntegrity :: CNF
                         -- ^ Describes the authority with which
                         -- immutable data was endorsed, or the
                         -- authority required to modify mutable data.
                       } deriving (Ord, Show, Eq, Typeable, Read)


class ToCNF c where
  toCNF :: c -> CNF

instance ToCNF CNF where
  toCNF = id

instance ToCNF (Priv CNF) where
  toCNF = privDesc

instance ToCNF Disjunction where
  toCNF = cSingleton

instance ToCNF Principal where
  toCNF = toCNF . dSingleton

instance ToCNF [Char] where
  toCNF = toCNF . principal

instance ToCNF Bool where
  toCNF True = cTrue
  toCNF False = cFalse

-- | The primary way of creating a 'DCLabel'.  The secrecy component
-- goes on the left, while the integrity component goes on the right,
-- e.g.:
--
-- > label = secrecyCNF %% integrityCNF
--
-- Unlike the 'DCLabel' constructor, the arguments can be any instance
-- of 'ToCNF'.  @%%@ has fixity:
--
-- > infix 6 %%
(%%) :: (ToCNF a, ToCNF b) => a -> b -> DCLabel
a %% b = toCNF a `DCLabel` toCNF b
infix 6 %%

-- | Compute a conjunction of two 'CNF's or 'ToCNF' instances.
--
-- Has fixity:
--
-- > infixr 7 /\
(/\) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a /\ b = toCNF a `cUnion` toCNF b
infixr 7 /\

-- | Compute a disjunction of two 'CNF's or 'ToCNF' instances.  Note
-- that this can be an expensive operation if the inputs have many
-- conjunctions.
--
-- The fixity is specifically chosen so that @&#92;&#47;@ and '/\'
-- cannot be mixed in the same expression without parentheses:
--
-- > infixl 7 \/
(\/) :: (ToCNF a, ToCNF b) => a -> b -> CNF
a \/ b = toCNF a `cOr` toCNF b
infixl 7 \/

-- |
-- > dcPublic = True %% True
--
-- This label corresponds to public data with no integrity guarantees.
-- For instance, an unrestricted Internet socket should be labeled
-- @dcPublic@.  The significance of @dcPublic@ is that given data
-- labeled @(s %% i)@, @s@ is the exact minimum authority such that
-- @(s %% i) &#x2291;&#x209b; dcPublic@, while @i@ is the exact
-- minimum authority such that @dcPublic &#x2291;&#x1d62; (s %% i)@.
dcPublic :: DCLabel
dcPublic = True %% True


instance Label DCLabel where
  lub (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cUnion s1 s2) (cOr i1 i2)
  glb (DCLabel s1 i1) (DCLabel s2 i2) = DCLabel (cOr s1 s2) (cUnion i1 i2)
  canFlowTo (DCLabel s1 i1) (DCLabel s2 i2) = cImplies s2 s1 && cImplies i1 i2

instance SpeaksFor CNF where
  speaksFor = cImplies

--
-- Type aliases
--

-- | A common default starting state, where @'lioLabel' = 'dcPublic'@
-- and @'lioClearance' = False '%%' True@ (i.e., the highest
-- possible clearance).
dcDefaultState :: LIOState DCLabel
dcDefaultState = LIOState { lioLabel = dcPublic
                          , lioClearance = False %% True }

-- | The main monad type alias to use for 'LIO' computations that are
-- specific to 'DCLabel's.
type EnclaveDC = Enclave DCLabel

-- | An alias for 'Labeled' values labeled with a 'DCLabel'.
type DCLabeled = Labeled DCLabel


------------------------------ Privileges----------------------------

-- | Every privilege type must be an instance of 'SpeaksFor', which is
-- a partial order specifying when one privilege value is at least as
-- powerful as another.  If @'canFlowToP' p1 l1 l2@ and @p2
-- `speaksFor` p1@, then it should also be true that @'canFlowToP' p2
-- l1 l2@.
--
-- As a partial order, 'SpeaksFor' should obey the reflexivity,
-- antisymmetry and transitivity laws.  However, if you do not wish to
-- allow delegation of a particular privilege type, you can define
-- @'speaksFor' _ _ = False@ (which violates the reflexivity law, but
-- is reasonable when you don't want the partial order).
class (Typeable p, Show p) => SpeaksFor p where
  -- | @speaksFor p1 p2@ returns 'True' iff @p1@ subsumes all the
  -- privileges of @p2@.  In other words, it is safe for 'delegate' to
  -- hand out @p2@ to a caller who already has @p1@.
  --
  -- Has fixity:
  --
  -- > infix 4 `speaksFor`
  speaksFor :: p -> p -> Bool

infix 4 `speaksFor`

--
-- Privileges
--

-- | A newtype wrapper that can be used by trusted code to transform a
-- powerless description of privileges into actual privileges.  The
-- constructor, 'PrivTCB', is dangerous as it allows creation of
-- arbitrary privileges.  Hence it is only exported by the unsafe
-- module "LIO.TCB".  A safe way to create arbitrary privileges is to
-- call 'privInit' (see "LIO.Run#v:privInit") from the 'IO' monad
-- before running your 'LIO' computation.
newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)

-- | Turns privileges into a powerless description of the privileges
-- by unwrapping the 'Priv' newtype.
privDesc :: Priv a -> a
{-# INLINE privDesc #-}
privDesc (PrivTCB a) = a

instance Semigroup p => Semigroup (Priv p) where
  (PrivTCB m1) <> (PrivTCB m2) = PrivTCB $ m1 <> m2

instance Monoid p => Monoid (Priv p) where
  mempty = PrivTCB mempty
  {-# INLINE mconcat #-}
  mconcat ps = PrivTCB $ mconcat $ map (\(PrivTCB p) -> p) ps

-- | Initialize some privileges (within the 'IO' monad) that can be
-- passed to 'LIO' computations run with 'runLIO' or 'evalLIO'.  This
-- is a pure function, but the result is encapsulated in 'IO' to
-- make the return value inaccessible from 'LIO' computations.
--
-- Note the same effect can be achieved using the 'PrivTCB'
-- constructor, but 'PrivTCB' is easier to misuse and is only available by
-- importing "LIO.TCB".
privInit :: (SpeaksFor p) => p -> IO (Priv p)
privInit p | isPriv p  = fail "privInit called on Priv object"
           | otherwise = return $ PrivTCB p

-- | Uses dynamic typing to return 'True' iff the type of the argument
-- is @'Priv' a@ (for any @a@).  Mostly useful to prevent users from
-- accidentally wrapping 'Priv' objects inside other 'Priv' objects or
-- accidentally including real privileges in an exception.
isPriv :: (Typeable p) => p -> Bool
isPriv p = typeRepTyCon (typeOf p) == privcon
  where privcon = typeRepTyCon $ typeOf noPrivs

-- | This class represents privilege descriptions, which define a
-- pre-order on labels in which distinct labels become equivalent.
-- The pre-oder implied by a privilege description is specified by the
-- method 'canFlowToP'.  In addition, this this class defines a method
-- 'downgradeP', which is important for finding least labels
-- satisfying a privilege equivalence.
--
-- Minimal complete definition: 'downgradeP'.
--
-- (The 'downgradeP' requirement represents the fact that a generic
-- 'canFlowToP' can be implemented efficiently in terms of
-- 'downgradeP', but not vice-versa.)
class (Label l, SpeaksFor p) => PrivDesc l p where
-- Note: SpeaksFor is a superclass for security reasons.  Were it not
-- a superclass, then if a label format ever failed to define
-- SpeaksFor, or defined it in a different module from the PrivDesc
-- instance, then an attacker could produce an vacuous instance that
-- allows all delegation.

    -- | Privileges are described in terms of a pre-order on labels in
    -- which sets of distinct labels become equivalent.  @downgradeP p
    -- l@ returns the lowest of all labels equivalent to @l@ under
    -- privilege description @p@.
    --
    -- Less formally, @downgradeP p l@ returns a label representing
    -- the furthest you can downgrade data labeled @l@ given
    -- privileges described by @p@.
    downgradeP :: p     -- ^ Privilege description
                  -> l  -- ^ Label to downgrade
                  -> l  -- ^ Lowest label equivelent to input

    -- | @canFlowToP p l1 l2@ determines whether @p@ describes
    -- sufficient privileges to observe data labeled @l1@ and
    -- subsequently write it to an object labeled @l2@.  The function
    -- returns 'True' if and only if either @canFlowTo l1 l2@ or @l1
    -- and l2@ are equivalent under @p@.
    --
    -- The default definition is:
    --
    -- > canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2
    -- 
    -- @canFlowToP@ is a method rather than a function so that it can
    -- be optimized in label-specific ways.  However, custom
    -- definitions should behave identically to the default.
    canFlowToP :: p -> l -> l -> Bool
    canFlowToP p l1 l2 = downgradeP p l1 `canFlowTo` l2

instance (SpeaksFor p) => SpeaksFor (Priv p) where
  {-# INLINE speaksFor #-}
  speaksFor p1 p2 = privDesc p1 `speaksFor` privDesc p2

instance (PrivDesc l p) => PrivDesc l (Priv p) where
  {-# INLINE downgradeP #-}
  downgradeP = downgradeP . privDesc
  {-# INLINE canFlowToP #-}
  canFlowToP = canFlowToP . privDesc

--
-- NoPrivs
--

-- | Generic 'PrivDesc' used to denote the lack of privileges.  Works
-- with any 'Label' type.  This is only a privilege description; a
-- more useful symbol is 'noPrivs', which actually embodies the
-- @NoPrivs@ privilege.
data NoPrivs = NoPrivs deriving (Show, Read, Typeable)

instance SpeaksFor NoPrivs where speaksFor _ _ = True

-- | 'downgradeP' 'NoPrivs' is the identify function.  Hence
-- 'canFlowToP' 'NoPrivs' is the same as 'canFlowTo'.
instance Label l => PrivDesc l NoPrivs where downgradeP _ l = l

instance Semigroup NoPrivs where
  _ <> _ = NoPrivs

instance Monoid NoPrivs where
  mempty = NoPrivs


-- | 'Priv' object corresponding to 'NoPrivs'.
noPrivs :: Priv NoPrivs
noPrivs = PrivTCB NoPrivs
