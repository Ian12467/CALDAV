import Data.Conduit
import Data.Conduit.Ical
import Data.Time
import Data.Text (Text)
import Data.Icalendar
import Data.Icalendar.Types
import Data.Icalendar.Printer
import System.Cron

-- Parse iCalendar data from each subscription
getEvents :: [Text] -> IO [VEvent]
getEvents urls = do
    events <- forM urls $ \url -> do
        runResourceT $ parseIcs url $$ sinkIcs
    return $ concat events

-- Generate a new iCalendar file with the combined events
generateCalendar :: [VEvent] -> Text
generateCalendar events =
    let cal = Calendar [Version 2, ProdId "my-calendar"]
                (map (\event -> Component "VEVENT" [event]) events)
    in printIcs cal


-- Define the cron expression for the schedule
cronExpression = "0,30 * * * *" -- runs every 30 minutes

-- Define the task that should be executed on the schedule
task :: IO ()
task = do
    putStrLn "Updating calendar..."
    events <- getEvents urls -- get events from subscriptions
    let newCalendar = generateCalendar events -- generate new calendar
    -- write new calendar to file
    writeFile "calendar.ics" (unpack newCalendar)
    putStrLn "Calendar updated!"

-- Run the task on the schedule
main :: IO ()
main = withCron cronExpression task

