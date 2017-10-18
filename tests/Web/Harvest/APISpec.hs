{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Web.Harvest.APISpec
  ( main
  , spec )
where

import Data.FileEmbed
import Data.Time
import Test.Hspec
import Web.Harvest.API
import qualified Data.Aeson as A

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "getUsers (JSON parsing)"       getUsersJsonSpec
  describe "getTimeEntries (JSON parsing)" getTimeEntriesJsonSpec

getUsersJsonSpec :: Spec
getUsersJsonSpec =
  context "when parsing API response" $
    it "returns correct data" $
      result `shouldBe` Just
        [ User
          { userId        = UserId 508343
          , userEmail     = "user@example.com"
          , userCreatedAt = UTCTime
            { utctDay = ModifiedJulianDay 56412
            , utctDayTime = secondsToDiffTime 73692 }
          , userIsAdmin   = True
          , userFirstName = "Harvest"
          , userLastName  = "User"
          , userTimeZone  = "Eastern Time (US & Canada)"
          , userIsContractor = False
          , userTelephone = ""
          , userIsActive  = True
          , userAccessFuture = True
          , userDefaultHourlyRate = Just 0
          , userDepartment = Just ""
          , userWantsNewsletter = True
          , userUpdatedAt  = UTCTime
            { utctDay = ModifiedJulianDay 57141
            , utctDayTime = secondsToDiffTime 53659 }
          , userCostRate = Nothing
          , userIdentityAccountId = Nothing
          , userIdentityUserId = Nothing }
        , User
          { userId        = UserId 508343
          , userEmail     = "user@example.com"
          , userCreatedAt = UTCTime
            { utctDay = ModifiedJulianDay 56412
            , utctDayTime = secondsToDiffTime 73692 }
          , userIsAdmin   = True
          , userFirstName = "John"
          , userLastName  = "Smith"
          , userTimeZone  = "Eastern Time (US & Canada)"
          , userIsContractor = False
          , userTelephone = ""
          , userIsActive  = True
          , userAccessFuture = True
          , userDefaultHourlyRate = Just 0
          , userDepartment = Nothing
          , userWantsNewsletter = False
          , userUpdatedAt  = UTCTime
            { utctDay = ModifiedJulianDay 57141
            , utctDayTime = secondsToDiffTime 53659 }
          , userCostRate = Nothing
          , userIdentityAccountId = Just 302900
          , userIdentityUserId = Just 20725 }
        ]
  where
    response = $(embedFile "data-examples/people.json")
    result = A.decodeStrict response

getTimeEntriesJsonSpec :: Spec
getTimeEntriesJsonSpec =
  context "when parsing API response" $
    it "returns correct data" $
      shouldBe result . Just . TimeEntries (ModifiedJulianDay 57414) $
        [ TimeEntry
          { teProjectId = "5198193"
          , teProject   = "Internal"
          , teUserId    = UserId 508343
          , teSpentAt   = ModifiedJulianDay 57414
          , teTaskId    = "2892243"
          , teTask      = "Internal Development"
          , teClient    = "Apple"
          , teId        = TimeEntryId 420923769
          , teNotes     = Just "Here goes the description entered by the user."
          , teTimerStartedAt = Nothing
          , teCreatedAt = UTCTime
            { utctDay = ModifiedJulianDay 57414
            , utctDayTime = secondsToDiffTime 14844 }
          , teUpdatedAt = UTCTime
            { utctDay = ModifiedJulianDay 57414
            , utctDayTime = secondsToDiffTime 19697 }
          , teHoursWithoutTimer = 1.16
          , teHours     = 2.77 }
        , TimeEntry
          { teProjectId = "5198193"
          , teProject   = "Internal"
          , teUserId    = UserId 508343
          , teSpentAt   = ModifiedJulianDay 57414
          , teTaskId    = "2892243"
          , teTask      = "Internal Development"
          , teClient    = "Apple"
          , teId        = TimeEntryId 420923783
          , teNotes     = Nothing
          , teTimerStartedAt = Just UTCTime
            { utctDay = ModifiedJulianDay 57414
            , utctDayTime = secondsToDiffTime 19697 }
          , teCreatedAt = UTCTime
            { utctDay = ModifiedJulianDay 57414
            , utctDayTime = secondsToDiffTime 14844 }
          , teUpdatedAt = UTCTime
            { utctDay = ModifiedJulianDay 57414
            , utctDayTime = secondsToDiffTime 19697 }
          , teHoursWithoutTimer = 1
          , teHours     = 2 }
        ]
  where
    response = $(embedFile "data-examples/daily.json")
    result = A.decodeStrict response
