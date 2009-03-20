secsToWeeks secs = let perMinute = 60
                       perHour   = 60 * perMinute
                       perDay    = 24 * perHour
                       perWeek   =  7 * perDay
                   in  secs / perWeek