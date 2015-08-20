{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module PebbleGen where
import GHC.Generics (Generic)
import Control.Monad
import Data.HashMap
import Data.Hashable
import Control.Monad.State.Lazy
import System.Environment

translateCode :: PebbleCode a -> CodeFile
translateCode code = snd $ runState code emptyProgram

data Rect = Rect Float Float Float Float

type PebbleCode a = State CodeFile a

data PebbleType = PInt 
                  | PFloat 
                  | PVoid 
                  | PPtr PebbleType 
                  | PStruct String deriving 
                    (Generic, Ord, Eq)

instance Hashable PebbleType

data Function = Function {
                         identifier :: String, 
                         returnType :: PebbleType, 
                         argTypes :: [PebbleType]
                         } deriving (Generic, Ord, Eq)

instance Hashable Function

type CodeFile = Map Function [String]

setupAndCleanup :: Function -> String -> Function -> String -> CodeFile -> CodeFile
setupAndCleanup setupFunction setupCode cleanupFunction cleanupCode codeFile =
  let setupBody = findWithDefault [] setupFunction codeFile
      cleanupBody = findWithDefault [] cleanupFunction codeFile in
        insert setupFunction (setupBody ++ [setupCode]) $
          insert cleanupFunction (cleanupCode : cleanupBody) codeFile

emptyProgram :: CodeFile
emptyProgram = fromList []

renderCode :: CodeFile -> [String]
renderCode code = renderDecls code ++ renderDefns code

renderDecls :: CodeFile -> [String]
renderDecls code = foldWithKey (\fun -> \_ -> \decls -> unwords (makeDecl fun) : decls) [] code

renderDefns :: CodeFile -> [String]
renderDefns code = undefined

makeDecl :: Function -> [String]
makeDecl fun  = "static" : (renderType $ returnType fun) : identifier fun : "(" : fmap renderType (argTypes fun) ++ [");"]

renderType :: PebbleType -> String
renderType PInt = "int"
renderType PFloat = "float"
renderType PVoid = "void"
renderType (PPtr t) = "*(" ++ renderType t ++ ")"
renderType (PStruct str) = str
