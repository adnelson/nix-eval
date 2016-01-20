-- | Describes generic values. `Value`s are the results of evaluating
-- expressions. They are "generic" in the sense that they can be
-- evaluated in any arbitrary monadic context. The `Lazy` and `Strict`
-- values (described in other modules) are more specific.
module Nix.Values.Generic where

import Nix.Common
import Nix.Expressions
import Nix.Constants
import Nix.Types (NExpr, Formals)
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Values ---------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | The type of runtime values. Is polymorphic over the computation
-- context type
data Value m
  = VConstant Constant
  -- ^ Constant values (isomorphic to constant expressions).
  | VAttrSet (AttrSet m)
  -- ^ Attribute set values.
  | VList (Seq (m (Value m)))
  -- ^ List values.
  | VFunction Params (Closure m)
  -- ^ Functions, with parameters and a closure.
  | VFunction' (Formals NExpr) (Closure' m)
  | forall v. VNative (Native m v)
  -- ^ Native values, which can be either values or functions.

instance Extract m => Show (Value m) where
  show (VConstant c) = "VConstant (" <> show c <> ")"
  show (VAttrSet set) = show set
  show (VList vs) = show $ map extract vs
  show (VFunction params closure) = concat [ show params, " => ("
                                           , show closure, ")"]
  show (VNative (NativeValue v)) = show $ extract v
  show (VNative _) = "(native function)"

instance Extract m => Eq (Value m) where
  VConstant c == VConstant c' = c == c'
  VAttrSet as == VAttrSet as' = as == as'
  VList vs == VList vs' = map extract vs == map extract vs'
  VFunction p1 e1 == VFunction p2 e2 = p1 == p2 && e1 == e2
  VNative (NativeValue v) == VNative (NativeValue v') =
    extract v == extract v'
  _ == _ = False

-- TODO: this is still incomplete
instance Extract m => Ord (Value m) where
  VConstant c1 <= VConstant c2 = c1 <= c2
  VConstant _ <= _ = True
  VList l1 <= VList l2 = map extract l1 <= map extract l2
  VList _ <= _ = True
  VAttrSet a1 <= VAttrSet a2 = a1 <= a2
  VAttrSet _ <= _ = True
  -- VFunction p1 e1 <= VFunction p2 e2 = p1 <= p2 && e1 <= e2
  VFunction _ _ <= _ = True
  VNative (NativeValue v) <= VNative (NativeValue v') = extract v <= extract v'
  VNative _ <= _ = True

instance IsString (Value m) where
  fromString = VConstant . fromString

instance Monad m => FromConstant (Value m) where
  fromConstant = VConstant
  fromConstants = VList . fromList . map (return . fromConstant)
  fromConstantSet set = VAttrSet $ Environment $
    map (return . fromConstant) set

instance Monad m => FromConstant (m (Value m)) where
  fromConstant = return . fromConstant
  fromConstants = return . fromConstants
  fromConstantSet = return . fromConstantSet

-- Convenience functions
-- | Shorthand for creating an Environment from a list.
mkEnv :: Monad m => [(Text, Value m)] -> Environment m
mkEnv = Environment . H.fromList . map (map pure)

-- | Same as 'mkEnv' but the values are lazy.
mkEnvL :: Monad m => [(Text, m (Value m))] -> Environment m
mkEnvL = Environment . H.fromList

-- | Shorthand to create a closure from a list and an expression.
mkClosure :: Monad m => [(Text, Value m)] -> Expression -> Closure m
mkClosure env expr = Closure (mkEnv env) expr

-- | Create a value from a string.
strV :: Monad m => Text -> Value m
strV = fromConstant . String

-- | Create a value from an integer.
intV :: Monad m => Integer -> Value m
intV = fromConstant . Int

-- | Create a value from a bool.
boolV :: Monad m => Bool -> Value m
boolV = fromConstant . Bool

-- | Create a null value.
nullV :: Monad m => Value m
nullV = fromConstant Null

-- | Create an attribute set value.
attrsV :: Monad m => [(Text, Value m)] -> Value m
attrsV = VAttrSet . mkEnv

-- | Create a list value.
listV :: Monad m => [Value m] -> Value m
listV = VList . fromList . map pure

-- | Create a function value.
functionV :: Monad m => Params -> Closure m -> Value m
functionV params closure = VFunction params closure

-- | Wrap a native into a value.
nativeV :: Monad m => Native m v -> Value m
nativeV = VNative

-- | Wrap a native into a monadic value.
pNativeV :: Monad m => Native m v -> m (Value m)
pNativeV = pure . nativeV

-------------------------------------------------------------------------------
-- Environments and Attribute Sets --------------------------------------------
-------------------------------------------------------------------------------

-- | An environment is conceptually just a name -> value mapping, but the
-- element type is parametric to allow usage of different kinds of values.
newtype Environment m = Environment {eEnv :: HashMap Text (m (Value m))}

instance Extract m => Eq (Environment m) where
  Environment e1 == Environment e2 = map extract e1 == map extract e2

-- | We can show an environment purely if the context implements extract.
instance (Extract ctx) => Show (Environment ctx) where
  show (Environment env) = "{" <> items <> "}" where
    showPair (n, v) = unpack n <> " = " <> show (extract v)
    items = intercalate "; " $ map showPair $ H.toList env

instance Extract ctx => Ord (Environment ctx) where
  Environment e1 <= Environment e2 = toMap e1 <= toMap e2 where
    toMap = M.fromList . H.toList . map extract

-- | We also use environments to represent attribute sets, since they
-- have the same behavior (in fact the `with` construct makes this
-- correspondence explicit).
type AttrSet = Environment

-- | A closure is an unevaluated expression, with just an environment.
data Closure m = Closure (Environment m) Expression
  deriving (Eq, Generic)

data Closure' m = Closure' (Environment m) NExpr
  deriving (Eq, Generic)

-- | TODO: make a proper ord instance...
instance Extract m => Ord (Closure m) where
  Closure env _ <= Closure env' _ = env <= env'

instance Extract m => Show (Closure m) where
  show (Closure env body) = "with " <> show env <> "; " <> show body

-- | Get the size of an environment.
envSize :: Environment m -> Int
envSize (Environment e) = H.size e

-- | Union two environments. Left-biased.
unionEnv :: Environment m -> Environment m -> Environment m
unionEnv (Environment e1) (Environment e2) = Environment (e1 `union` e2)

-- | Look up a name in an environment.
lookupEnv :: Text -> Environment m -> Maybe (m (Value m))
lookupEnv name (Environment env) = H.lookup name env

-- | Insert a name/value into an environment.
insertEnv :: Monad m => Text -> Value m -> Environment m -> Environment m
insertEnv name v (Environment env) = Environment $
  H.insert name (return v) env

-- | Insert a name/value into an environment, where the value is lazy.
insertEnvL :: Text -> m (Value m) -> Environment m -> Environment m
insertEnvL name v (Environment env) = Environment $ H.insert name v env

deleteEnv :: Text -> Environment m -> Environment m
deleteEnv key (Environment env) = Environment $ H.delete key env

-- | Convert an environment to a list of (name, v).
envToList :: Environment m -> [(Text, m (Value m))]
envToList (Environment env) = H.toList env

-- | Get the set of keys in the environment.
envKeySet :: Environment m -> Set Text
envKeySet (Environment env) = S.fromList $ H.keys env

-- | Get a list of keys in the environment.
envKeyList :: (IsSequence seq, Element seq ~ Text) => Environment m -> seq
envKeyList (Environment env) = fromList $ H.keys env

-- | Get a list of values in the environment.
envValueList :: (IsSequence seq, Element seq ~ m (Value m)) => Environment m -> seq
envValueList (Environment env) = fromList $ H.elems env

-- | An empty environment.
emptyE :: Environment m
emptyE = Environment mempty

-- | An empty closure.
emptyC :: Expression -> Closure m
emptyC = Closure emptyE

-------------------------------------------------------------------------------
-- Native Values --------------------------------------------------------------
-------------------------------------------------------------------------------

-- | An embedding of raw values. Lets us write functions in Haskell
-- which operate on Nix values, and expose these in Nix code.
data Native (m :: (* -> *)) :: * -> * where
  NativeValue :: m (Value m) -> Native m (Value m)
  -- ^ A terminal value (which has not necessarily been evaluated).
  NativeFunction :: (m (Value m) -> m (Native m v)) -> Native m (Value m -> v)
  -- ^ A function which lets us take the "next step" given a value.
  -- Either the argument or the result of this function might be
  -- failure, so we express that by having them be a monadic values.

-- | Apply a native value as if it were a function.
applyNative :: Native m (Value m -> t) -> m (Value m) -> m (Native m t)
applyNative (NativeFunction func) arg = func arg

-- | Apply a native value as if it were a function, to two arguments.
applyNative2 :: Monad m =>
             Native m (Value m -> Value m -> t) ->
             m (Value m) -> m (Value m) -> m (Native m t)
applyNative2 (NativeFunction func) x y = do
  NativeFunction newFunc <- func x
  newFunc y

-- | Turn a 'Native' into a monadic 'Value'. This is useful because
-- objects built with 'NativeValue' are isomorphic to 'Value's.
-- Unwrapping them here means the only 'Native' values that we need to
-- keep around are 'NativeFunction's.
unwrapNative :: Monad m => Native m v -> m (Value m)
unwrapNative (NativeValue v) = v
unwrapNative n = return $ VNative n

-------------------------------------------------------------------------------
-- * Abstract evaluation contexts
------------------------------------------------------------------------------
-- Not every monad can act as an evaluation context for Nix
-- expressions. In particular, there are certain primitive
-- side-effects we must support, such as printing to screen for the
-- `trace` builtin, or reading the contents of a directory.
-- So we have a more specialized type class here, which is actually
-- made up of a few more specific type classes.

-- | This class doesn't express any of its own methods; rather it just
-- wraps several other type classes into one.
class (WriteMessage m) => Nix m

-- | A monad in which we can print messages. This lets us implement
-- the `trace` builtin function.
class Monad m => WriteMessage m where
  -- | Write a message (e.g. to stdout)
  writeMessage :: Text -> m ()
