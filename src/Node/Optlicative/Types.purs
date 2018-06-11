module Node.Optlicative.Types where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.List (List, singleton)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V(..), isValid, invalid)

newtype Optlicative a = Optlicative (OptState -> { state :: OptState, val :: V (List OptError) a })

derive instance newtypeOptlicative :: Newtype (Optlicative a) _

derive instance functorOptlicative :: Functor Optlicative

-- TODO: figure out why my type annotations make the compiler think
-- the explicit forall type variables are not general enough
instance monadRecOptlicative :: MonadRec Optlicative where
    --tailRecM :: forall a b. (a -> Optlicative (Step a b)) -> a -> Optlicative b
    tailRecM f init = Optlicative \optstate -> tailRec (f init) optstate 
      where
        --tailRec :: forall a b. Optlicative (Step a b) -> OptState -> Result b
        tailRec p@(Optlicative o) optstate = loop (go (o optstate))
          where
            --loop :: forall a b. Step (Result a) (Result b) -> Result b
            loop (Done r) = r
            loop (Loop { state, val }) =
              case val of
                Invalid e   -> { state, val: invalid e}
                Valid value -> 
                  let
                    (Optlicative a1) = f value
                  in
                    loop (go (a1 state))

            go :: forall a b. Result (Step a b) -> Step (Result a) (Result b)
            go { state, val } = case val of
              Invalid e      -> Done { state, val: invalid e }
              Valid (Loop v) -> Loop { state, val: Valid v }
              Valid (Done v) -> Done { state, val: Valid v }

instance bindOptlicative :: Bind Optlicative where
  bind (Optlicative f) g = Optlicative \s -> 
    let
      { state, val } = f s
    in case val of
         Invalid e -> { state, val: invalid e}
         Valid v   -> 
           let
             (Optlicative b) = g v
           in
             b state

instance monadOptlicative :: Monad Optlicative

instance applyOptlicative :: Apply Optlicative where
  apply (Optlicative f) (Optlicative a) = Optlicative \ s ->
    let
      r1 = f s
      a' = a r1.state
      val = r1.val <*> a'.val
      state = a'.state
    in
      {state, val}

instance applicativeOptlicative :: Applicative Optlicative where
  pure a = Optlicative \ state -> {state, val: pure a}

instance altOptlicative :: Alt Optlicative where
  alt (Optlicative x) (Optlicative y) = Optlicative \ s ->
    let
      {val} = x s
    in
      if isValid val then x s else y s

instance plusOptlicative :: Plus Optlicative where
  empty = Optlicative \ state ->
    {state, val: invalid (singleton (Custom "Error: empty called"))}

instance alternativeOptlicative :: Alternative Optlicative

data OptError
  = TypeError ErrorMsg
  | MissingOpt ErrorMsg
  | MissingArg ErrorMsg
  | UnrecognizedOpt String
  | UnrecognizedCommand String
  | Custom ErrorMsg

renderOptError :: OptError -> String
renderOptError = case _ of
  TypeError msg -> "Type error: " <> msg
  MissingOpt msg -> "Missing option: " <> msg
  MissingArg msg -> "Missing argument: " <> msg
  UnrecognizedOpt msg -> "Unrecognized option: " <> msg
  UnrecognizedCommand msg -> "Unrecognized command: " <> msg
  Custom msg -> msg

instance showError :: Show OptError where
  show (TypeError msg) = "(TypeError " <> show msg <> ")"
  show (MissingOpt msg) = "(MissingOpt " <> show msg <> ")"
  show (MissingArg msg) = "(MissingArg " <> show msg <> ")"
  show (UnrecognizedOpt msg) = "(UnrecognizedOpt " <> show msg <> ")"
  show (UnrecognizedCommand msg) = "(UnrecognizedCommand " <> show msg <> ")"
  show (Custom msg) = "(Custom " <> show msg <> ")"

type Value a = V (List OptError) a

type OptState = {unparsed :: List String}

type Result a = {state :: OptState, val :: Value a}

type ErrorMsg = String

type Preferences a =
  { errorOnUnrecognizedOpts :: Boolean
  , usage :: Maybe String
  , globalOpts :: Optlicative a
  }
