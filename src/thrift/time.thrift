// namespace
namespace cpp birthday
namespace java birthday
// monthd
enum Month {
  kNone = 0,
  kJanuary = 1,
  kFebruary = 2,
  kMarch = 3,
  kApril = 4,
  kMay = 5,
  kJune = 6,
  kJuly = 7,
  kAugust = 8,
  kSeptember = 9,
  kOctober = 10,
  kNovember = 11,
  kDecember = 12,
  kEnd = 13,
}
// birthday
struct Birthday {
  1: optional i16 year = 2014,
  // month
  2: required byte month = 1,
  // day
  3: required byte day = 1,
}
// time
struct Time {
  // year
  1: optional i16 year = 2014,
  // month
  2: required byte month = 1,
  // day
  3: required byte day = 1,
  // hour
  4: required byte hour = 0,
  // minute
  5: required byte minute = 0,
  // seconds
  6: optional byte seconds = 0,
}