module Pfsm.NimService2B

import Data.List
import Data.SortedMap
import Pfsm
import Pfsm.Data

public export
record Index where
  constructor MkIndex
  name: Name
  fields: List String

export
liftIndexes : Maybe (List Meta) -> List Index
liftIndexes metas
  = case lookup "index" metas of
         Just (MVList dicts) => liftIndexes' [] dicts
         _ => []
  where
    liftIndex : SortedMap String MetaValue -> Maybe Index
    liftIndex meta
      = do name <- liftName meta
           fields <- liftFields meta
           pure (MkIndex name fields)
      where
        liftName : SortedMap String MetaValue -> Maybe Name
        liftName meta'
          = case lookup "name" meta' of
                 Just (MVString name) => Just name
                 _ => Nothing

        liftFields : SortedMap String MetaValue -> Maybe (List String)
        liftFields meta'
          = case lookup "fields" meta' of
                 Just (MVList fs) => liftMaybeList $ map liftField fs
                 _ => Nothing
          where
            liftField : MetaValue -> Maybe String
            liftField (MVString str) = Just str
            liftField _              = Nothing

    liftIndexes' : List Index -> List MetaValue -> List Index
    liftIndexes' acc []                   = acc
    liftIndexes' acc ((MVString _) :: ms) = liftIndexes' acc ms
    liftIndexes' acc ((MVList _)   :: ms) = liftIndexes' acc ms
    liftIndexes' acc ((MVDict m)   :: ms) = case liftIndex m of
                                                 Just index => liftIndexes' (index :: acc) ms
                                                 Nothing => liftIndexes' acc ms
