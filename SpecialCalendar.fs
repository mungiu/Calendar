module SpecialCalendar

let isLeap year =
    (year % 400) = 0 || ((year % 4) = 0 && (year % 100) <> 0)
//isLeap 2000 // true
//isLeap 2020 // true
//isLeap 2019 // false
//isLeap 2021 // false

let numberOfLeapYearsBetweenOneAnd year =
    [for i in 1 .. year -> i] 
    |> List.filter isLeap 
    |> List.length
// numberOfLeapYearsBetweenOneAnd 2000

let eraDaysToEndYear year =
    365 * year + (1 * numberOfLeapYearsBetweenOneAnd year)
//eraDaysToEndYear 1 // 365
//eraDaysToEndYear 1792 // 654515

// (367*m + 5)/12 - c, where c is a correction to allow for the possibility that y may be a leap-year.
let eraDaysToEndMonth month year =
    match month with
    | month when month = 0              -> (367 * month + 5) / 12 - 0 + eraDaysToEndYear (year - 1)
    | month when month >= 1 && month < 13 
        ->  match year with 
            | year when isLeap year         -> (367 * month + 5) / 12 - 1 + eraDaysToEndYear (year - 1)
            | year when not (isLeap year)   -> (367 * month + 5) / 12 - 2 + eraDaysToEndYear (year - 1)

//eraDaysToEndMonth 9 1792     // 654423
//eraDaysToEndMonth 7 622      // 227027
    
let eraDaysToDate date month year =
    date + (eraDaysToEndMonth (month-1) year)

//eraDaysToDate 1 1 1     // 1
//eraDaysToDate 19 7 622  // 227015
//eraDaysToDate 22 9 1792 // 654415
//eraDaysToDate 12 3 1999 // 729825


    