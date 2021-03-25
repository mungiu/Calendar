module SpecialCalendar

// find era-day of given date
// - the number of days between 1 January 1 and d. For example, the era-day of 1 January 1 is 1

// find date of a given era-day. 
// - the era-day of 31 December 1000 is 365242, and the era-day of 12 March 1999 is 729825.

// A year y is a leap-year if y is divisible by 400 or if it is divisible by 4 and not divisible by 100. 
// Define a function isleap : int -> bool that takes a year y and tells us if y is a leap-year. 
// For example, 2000 and 2020 are both leap-years; 2019 and 2021 are both not leap-years


//isLeap 2044

//Translating dates to era-days 
// The era-day of the last day of year y is 365*y plus 1 for each leap-year between 1 and y.
// We can use the principle of inclusion and exclusion to count these leap-years. 
// Add all the years that are multiples of 4; but this includes years like 2019 that aren’t 
// leap-years, so subtract all the years that are multiples of 100; but this excludes years 
// like 1600 that are leap-years, so add all the years that are multiples of 400. 
// Define a function daysToEndYear : int -> int that takes a year y and returns 
// the era-day of 31/12/y. Example, daysToEndYear 1 = 365 and daysToEndYear 
// 1792 = 654515. 
//let rec countLeapYearsRec yearList =
//    match yearList with
//    | [] -> 0
//    | head :: tail -> 
//        if (isLeap head) 
//            then 1 + countLeapYearsRec tail 
//            else countLeapYearsRec tail
let isLeap year =
    (year % 400 = 0) || (year % 4 = 0 && year % 100 <> 0)

let numberOfLeapYearsBetweenOneAnd year =
    [for i in 1 .. year -> i] 
    |> List.filter isLeap 
    |> List.length
//numberOfLeapYearsBetweenOneAnd 2000

let daysToEndYear year =
    365 * year + (1 * numberOfLeapYearsBetweenOneAnd year)
daysToEndYear 1792
    