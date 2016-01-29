
-- | Defines an abstract evaluation context.
module Nix.Evaluator.AbstractContext where

import Nix.Common
import Nix.Evaluator.Errors

-- | Types of directories, as defined in the nix manual.
data FileType = RegularType | DirectoryType | SymlinkType | UnknownType
  deriving (Show, Eq, Ord, Enum)

-- | A representation of directory structure. Directory contents are
-- wrapped in some monadic action, which means we don't need to parse
-- an entire (potentially huge) directory.
data FileSystemRep m
  = File Text -- ^ A file, with its contents.
  | Symlink FilePath -- ^ A symlink, with some path referenced.
  | Directory (Map Text (m DirRep)) -- ^ A directory.

-- | Return the type of a filesystem representation.
fileTypeOf :: FileSystemRep m -> FileType
fileTypeOf = \case
  File _ -> RegularType
  Symlink _ -> SymlinkType
  Directory _ -> DirectoryType

class Monad m => ReadFileSystem m where
  -- | Given a file path, return an abstract representation of its
  -- contents, if it exists. Otherwise, return 'Nothing'.
  readPath :: FilePath -> m (Maybe (FileSystemRep m))

-- | If the given file path exists, return its type (else 'Nothing').
typeOfPath :: ReadFileSystem m => FilePath -> m (Maybe FileType)
typeOfPath = map fileTypeOf . readPath

class Monad m =>

-- | An abstraction of the operations we'll need to perform during
-- evaluation, in particular performing effectful actions.
class (MonadError EvalError m,
       WriteMessage m) => Nix m where

  -- | Read a file. If the file doesn't exist, throw 'FileDoesNotExist'.
  readFileFromDisk :: FilePath -> m Text
  -- | Read directory, and return its contents as map of (name -> type).
  listDirectory :: FilePath -> m (Record FileType)
  -- | Write a file to the nix store.
  writeFileToStore :: FilePath -> Text
