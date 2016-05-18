module Web.Harvest.APISpec
  ( main
  , spec )
where

import Test.Hspec
-- import Web.Harvest.API

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getUsers (JSON parsing)"       getUsersJsonSpec
  describe "getTimeEntries (JSON parsing)" getTimeEntriesJsonSpec

getUsersJsonSpec :: Spec
getUsersJsonSpec = return () -- TODO

getTimeEntriesJsonSpec :: Spec
getTimeEntriesJsonSpec = return () -- TODO
