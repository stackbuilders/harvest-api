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
import Data.Time (UTCTime, Day)
import Servant.API

-- | Information that is necessary for interaction with Harvest API.

data Credentials = Credentials
  { credentialsUsername :: ByteString -- ^ Username
  , credentialsPassword :: ByteString -- ^ Password
  , credentialsAccount  :: ByteString -- ^ Account (name of your organization)
  } deriving (Eq, Ord, Show)

-- | User identifier.

newtype UserId = UserId { unUserId :: Word }
  deriving (Eq, Ord, Show, FromJSON, ToHttpApiData)

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
  , userTelephone    :: Text -- ^ User's telephone
  , userIsActive     :: Bool -- ^ Is it an active user?
  , userAccessFuture :: Bool
    -- ^ Does he\/she have access to all future projects?
  , userDefaultHourlyRate :: Word -- ^ Default hourly rate
  , userDepartment   :: Maybe Text -- ^ User's department
  , userWantsNewsletter :: Bool
    -- ^ Does he\/she want to recieve newsletter?
  , userUpdatedAt    :: UTCTime -- ^ Time of last update of account
  , userCostRate     :: Maybe Word -- ^ User's cost rate
  , userIdentityAccountId :: Maybe Word -- ^ Identity account id
  , userIdentityUserId :: Maybe Word -- ^ Identity user id
  } deriving (Eq, Ord, Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    u                <- o .: "user"
    userId           <- u .: "id"
    userEmail        <- u .: "email"
    userCreatedAt    <- u .: "created_at"
    userIsAdmin      <- u .: "is_admin"
    userFirstName    <- u .: "first_name"
    userLastName     <- u .: "last_name"
    userTimeZone     <- u .: "timezone"
    userIsContractor <- u .: "is_contractor"
    userTelephone    <- u .: "telephone"
    userIsActive     <- u .: "is_active"
    userAccessFuture <- u .: "has_access_to_all_future_projects"
    userDefaultHourlyRate <- u .: "default_hourly_rate"
    userDepartment   <- u .: "department"
    userWantsNewsletter <- u .: "wants_newsletter"
    userUpdatedAt    <- u .: "updated_at"
    userCostRate     <- u .: "cost_rate"
    userIdentityAccountId <- u .:? "identity_account_id"
    userIdentityUserId <- u .:? "identity_user_id"
    return User {..}

-- | Collection of entries for specific day.

-- TODO This response also contains the "projects" field with some stuff,
-- we could parse it as well, but let's skip it for now.

data TimeEntries = TimeEntries
  { teForDay     :: Day -- ^ That collection is for this day
  , teDayEntries :: [TimeEntry] -- ^ Collection of time entries
  } deriving (Eq, Ord, Show)

instance FromJSON TimeEntries where
  parseJSON = withObject "TimeEntries" $ \o -> do
    teDayEntries <- o .: "day_entries"
    teForDay     <- o .: "for_day"
    return TimeEntries {..}

-- | Time entry identifier.

newtype TimeEntryId = TimeEntryId
  { unTimeEntryId :: Word }
  deriving (Eq, Ord, Show, FromJSON)

-- | A time entry.

data TimeEntry = TimeEntry
  { teProjectId :: Text -- ^ Project id
  , teProject   :: Text -- ^ Project
  , teUserId    :: UserId -- ^ User id
  , teSpentAt   :: Day  -- ^ The day this time is spent on
  , teTaskId    :: Text -- ^ Task id
  , teTask      :: Text -- ^ Task name
  , teClient    :: Text -- ^ Client name
  , teId        :: TimeEntryId -- ^ Time entry id
  , teNotes     :: Maybe Text -- ^ Notes
  , teTimerStartedAt :: Maybe UTCTime
    -- ^ When the task was started, if 'Nothing' — timer is not running
  , teCreatedAt :: UTCTime -- ^ When the task was created
  , teUpdatedAt :: UTCTime -- ^ When the task was updated
  , teHoursWithoutTimer :: Double -- ^ Hours without timer
  , teHours     :: Double -- ^ Hours
  } deriving (Eq, Ord, Show)

instance FromJSON TimeEntry where
  parseJSON = withObject "TimeEntry" $ \o -> do
    teProjectId      <- o .: "project_id"
    teProject        <- o .: "project"
    teUserId         <- o .: "user_id"
    teSpentAt        <- o .: "spent_at"
    teTaskId         <- o .: "task_id"
    teTask           <- o .: "task"
    teClient         <- o .: "client"
    teId             <- o .: "id"
    teNotes          <- o .: "notes"
    teTimerStartedAt <- o .:? "timer_started_at"
    teCreatedAt      <- o .: "created_at"
    teUpdatedAt      <- o .: "updated_at"
    teHoursWithoutTimer <- o .: "hours_without_timer"
    teHours          <- o .: "hours"
    return TimeEntry {..}

-- | Parse time of day in the \"HH:MMam\" (\"HH:MMpm\") format.

-- parseDiffTime :: Value -> A.Parser TimeOfDay
-- parseDiffTime = withText "time of day"
--   (Time.parseTimeM False Time.defaultTimeLocale "%l:%M%P" . T.unpack)
