{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Web.Harvest.API.Type
  ( Credentials (..)
  , UserId (..)
  , User (..)
  , TimeEntryId (..)
  , TimeEntries (..)
  , TimeEntry (..) )
where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime, Day, TimeOfDay)
import Servant.API
import qualified Data.Aeson.Types as A
import qualified Data.Text        as T
import qualified Data.Time        as Time

-- | Information that is necessary for interaction with Harvest API.

data Credentials = Credentials
  { credentialsUsername :: ByteString -- ^ Username
  , credentialsPassword :: ByteString -- ^ Password
  , credentialsAccount  :: ByteString -- ^ Account (name of your organization)
  } deriving (Eq, Show)

-- | User identifier.

newtype UserId = UserId { unUserId :: Word }
  deriving (Eq, Show, FromJSON, ToHttpApiData)

-- | User record.

data User = User
  { userId           :: UserId -- ^ User id
  , userEmail        :: Text -- ^ Email address
  , userCreatedAt    :: UTCTime -- ^ When the user was created
  , userIsAdmin      :: Bool -- ^ Is the user admin?
  , userFirstName    :: Text -- ^ The user's first name
  , userLastName     :: Text -- ^ The user's last name
  , userTimeZone     :: Text -- ^ The user's time zone ('Text' for now)
  , userIsContractor :: Bool -- ^ Is the user contractor?
  , userTelephone    :: Maybe Text -- ^ User's telephone (if available)
  , userIsActive     :: Bool -- ^ Is it an active user?
  , userAccessFuture :: Bool
    -- ^ Does he\/she have access to all future projects?
  , userDefaultHourlyRate :: Word -- ^ Default hourly rate
  , userDepartment   :: Maybe Text -- ^ User's department
  , userWantsNewsletter :: Bool
    -- ^ Does he\/she want to recieve newsletter?
  , userUpdatedAt    :: UTCTime -- ^ Time of last update of account
  , userCostRate     :: Maybe Word -- ^ User's cost rate
  , userIdentityAccountId :: Word -- ^ Identity account id
  , userIdentityUserId :: Word -- ^ Identity user id
  } deriving (Eq, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    userId           <- o .: "id"
    userEmail        <- o .: "email"
    userCreatedAt    <- o .: "created_at"
    userIsAdmin      <- o .: "is_admin"
    userFirstName    <- o .: "first_name"
    userLastName     <- o .: "last_name"
    userTimeZone     <- o .: "timezone"
    userIsContractor <- o .: "is_contractor"
    userTelephone    <- o .: "telephone"
    userIsActive     <- o .: "is_active"
    userAccessFuture <- o .: "has_access_to_all_future_projects"
    userDefaultHourlyRate <- o .: "default_hourly_rate"
    userDepartment   <- o .: "department"
    userWantsNewsletter <- o .: "wants_newsletter"
    userUpdatedAt    <- o .: "updated_at"
    userCostRate     <- o .: "cost_rate"
    userIdentityAccountId <- o .: "identity_user_id"
    userIdentityUserId <- o .: "identity_user_id"
    return User {..}

-- | Collection of entries for specific day.

data TimeEntries = TimeEntries
  { teDayEntries :: [TimeEntry] -- ^ Collection of time entries
  , teForDay     :: Day -- ^ That collection is for this day
  } deriving (Eq, Show)

instance FromJSON TimeEntries where
  parseJSON = withObject "TimeEntries" $ \o -> do
    teDayEntries <- o .: "day_entries"
    teForDay     <- (o .: "for_day") >>= parseDay
    return TimeEntries {..}

newtype TimeEntryId = TimeEntryId
  { unTimeEntryId :: Word }
  deriving (Eq, Show, FromJSON)

-- | A time entry.

data TimeEntry = TimeEntry
  { teProjectId :: Text -- ^ Project id
  , teProject   :: Text -- ^ Project
  , teUserId    :: UserId -- ^ User id
  , teSpentAt   :: Day  -- ^ The day this time is spent on
  , teTaskId    :: Text -- ^ Task id
  , teTask      :: Text -- ^ Backend programming
  , teClient    :: Text -- ^ Client name
  , teId        :: TimeEntryId -- ^ Time entry id
  , teNotes     :: Maybe Text -- ^ Notes
  , teStartedAt :: TimeOfDay -- ^ When the task was started
  , teEndedAt   :: TimeOfDay -- ^ When the task was finished
  , teCreatedAt :: UTCTime -- ^ When the task was created
  , teUpdatedAt :: UTCTime -- ^ When the task was updated
  , teHoursWithoutTimer :: Word -- ^ Hours without timer
  , teHours     :: Word -- ^ Hours
  } deriving (Eq, Show)

instance FromJSON TimeEntry where
  parseJSON = withObject "TimeEntry" $ \o -> do
    teProjectId      <- o .: "project_id"
    teProject        <- o .: "project"
    teUserId         <- o .: "user_id"
    teSpentAt        <- (o .: "spent_at") >>= parseDay
    teTaskId         <- o .: "task_id"
    teTask           <- o .: "task"
    teClient         <- o .: "client"
    teId             <- o .: "id"
    teNotes          <- o .: "notes"
    teStartedAt      <- (o .: "started_at") >>= parseDiffTime
    teEndedAt        <- (o .: "ended_at") >>= parseDiffTime
    teCreatedAt      <- o .: "created_at"
    teUpdatedAt      <- o .: "updated_at"
    teHoursWithoutTimer <- o .: "hours_without_timer"
    teHours          <- o .: "hours"
    return TimeEntry {..}

-- | Parse day in ISO 8601 format (\"YYYY-MM-DD\").

parseDay :: Value -> A.Parser Day
parseDay = withText "time in ISO 8601 format"
  (Time.parseTimeM False Time.defaultTimeLocale "%F" . T.unpack)

-- | Parse time of day in the \"HH:MMam\" (\"HH:MMpm\") format.

parseDiffTime :: Value -> A.Parser TimeOfDay
parseDiffTime = withText "time of day"
  (Time.parseTimeM False Time.defaultTimeLocale "%l:%M%P" . T.unpack)
