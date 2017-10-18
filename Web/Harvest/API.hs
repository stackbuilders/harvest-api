-- |
-- Module      :  Web.Harvest.API
-- Copyright   :  © 2016 Stack Builders
-- License     :  MIT
--
-- Maintainer  :  Mark Karpov <mkarpov@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level bindings to the Harvest web API.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Harvest.API
  ( -- * Types
    module Web.Harvest.API.Type
    -- * Users
  , getUsers
    -- * Timesheets
  , getTimeEntries )
where

import Data.Monoid (Sum (..))
import Data.Proxy
import Data.Time (Day)
import Data.Void
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client
import Web.Harvest.API.Type
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Time             as Time

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (foldMap)
import Data.Word (Word)
#endif

-- | Type representation of Basic Auth. We don't serve the API, so user type
-- is just 'Void'.

type Auth = BasicAuth "" Void

-- | Entire Harvest API as a type.

type HarvestAPI =
       "people" :> Auth :> Get '[JSON] [User]
  :<|> "daily"  :> Capture "day" Word
                :> Capture "year" Word
                :> QueryParam "of_user" UserId
                :> Auth
                :> Get '[JSON] TimeEntries

-- | A shortcut for the boilerplate arguments.

type Query a = BasicAuthData -> ClientM a

getUsers_       :: Query [User]
getTimeEntries_ :: Word -> Word -> Maybe UserId -> Query TimeEntries

getUsers_ :<|> getTimeEntries_ = client (Proxy :: Proxy HarvestAPI)

-- | Get list of all users for specific account.

getUsers :: Manager                         -- ^ HTTP Manager
         -> Credentials                     -- ^ Credentials
         -> IO (Either ServantError [User]) -- ^ Result of request
getUsers manager creds =
  let env = ClientEnv manager (getBaseUrl creds)
  in runClientM (runHarvestQuery getUsers_ creds) env

-- | Get time entries for specific date and user.

getTimeEntries :: Manager           -- ^ HTTP Manager
               -> Credentials       -- ^ Credentials
               -> Day               -- ^ Date of interest
               -> UserId            -- ^ User id
               -> IO (Either ServantError TimeEntries)
getTimeEntries manager creds date uid =
  let env = ClientEnv manager (getBaseUrl creds)
      (day, year) = getDayAndYear date
  in runClientM (runHarvestQuery (getTimeEntries_ day year $ Just uid) creds) env

-- | A helper to run a query against Harvest API.

runHarvestQuery :: Query a        -- ^ Query function
                -> Credentials    -- ^ Credentials
                -> ClientM a      -- ^ The result
runHarvestQuery action Credentials {..} =
  action (BasicAuthData credentialsUsername credentialsPassword)

-- | Extract day and year from given date. Days are in the range from 1 to
-- 366.

getDayAndYear :: Day -> (Word, Word)
getDayAndYear date = (day, fromIntegral year)
  where
    (year, month, day') = Time.toGregorian date
    day = fromIntegral . (+ day') . getSum . foldMap
      (Sum . Time.gregorianMonthLength year) $ [1..pred month]


-- | Get base url from Credentials
getBaseUrl :: Credentials   -- ^ Credentials
           -> BaseUrl       -- ^ Base Url
getBaseUrl Credentials {..} =
  let host = BC8.unpack credentialsAccount ++ ".harvestapp.com"
  in BaseUrl Https host 443 ""
